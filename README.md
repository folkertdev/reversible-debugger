# haskell-port

A haskell implementation of the µOz abstract machine and the CaReDeb causal-consistent reversible debugger

# Inspiration

* [The CaReDeb debugger](http://www.cs.unibo.it/caredeb/index.html) by Elena Giachino (developer), Ivan Lanese (developer) and Claudio A. Mezzina (main developer).
* [pi-calculus](https://github.com/renzyq19/pi-calculus) by Will de Renzy-Martin.

The semantics of the µOz abstract machine implemented here are detailed in this paper: 

[A Reversible Abstract Machine and Its Space Overhead](http://www.cs.unibo.it/caredeb/vm-oz.pdf)


## Installing and running 

This project uses [stack](https://docs.haskellstack.org/en/stable/README/) as the build tool and for dependency management. Once installed, the program can be run with

```sh
stack build && stack exec reversible-debugger -- send_receive.txt
```

The [Examples] folder contains a couple of µOz programs that you can step through.

[Examples]: https://github.com/folkertdev/reversible-debugger/tree/master/examples

## The Language

The µOz language instruction set is roughly equivalent to the haskell union type below: 

```haskell
data Program 
    = Sequence Program Program
    | Let Identifier Value Program
    | If BoolExp Program Program
    | SpawnThread Program
    | Skip
    | Apply Identifier (List Identifier)
    | Send Identifier Identifier
    | Assert BoolExp
```

Where `Identifier` is a wrapper around string, and `Value` is defined by 

```haskell
data Value
    = VTrue
    | VFalse
    | Receive Identifier
    | Procedure (List Identifier) Program 
    | Port 
    | VInt IntExp
```

The exact details and implementations for renaming variables can be found in [Types.hs]

[Types.hs]: https://github.com/folkertdev/reversible-debugger/blob/master/src/Types.hs

## Moving Forward 

One of the main insights I had is that a good interpreter does not necesarilly give a good debugging experience. The µOz language can be used 
to express concurrent operations (really, that's its raison d'etre), but debugging concurrently is not a fun expereience. 

So this interpreter simulates concurrency, but isn't actually in itself concurrent.  

We want to be able to step through our program, so given some program state, we need a function that executes exactly one instruction. 
This function is called `forwardThread`. Here is a short snippet: 

```haskell
    Apply functionName arguments -> do
        ( parameters, body ) <- lookupProcedure functionName 

        let withRenamedVariables = 
                foldr (uncurry renameVariable) body (zip parameters arguments)

        continue (CalledProcedure functionName arguments) (withRenamedVariables : rest)
                    
    Send channelName variable -> do
        writeChannel name channelName variable 
        continue (Sent channelName variable) rest
```

The function pattern matches on the next instruction, and for each instruction defines some modification to 

* the context - defining a variable, sending over a channel
* the history - enough information is kept to be able to revert the instruction
* the rest of the program 

### Moving on multiple threads

A thread in this interpreter is defined by its name, a stack of history instructions
and a stack of remaining program instructions.

```haskell
data Thread a = Thread ThreadName (List History) (List a) 
```

Now that we can move one thread forward, let's look at how we can keep track of multiple threads. 
We can define a tree-like structure to store the child-parent relationships: 

```haskell
data Task a 
    = Singleton (Thread a)
    | Parallel (Thread a) (Map ThreadName (Task a))
```

To move our program forward, we can go through the tree, and find the first thread that can make progress. A thread can make progress when

* its list of instructions is not empty and 
* it is not blocked on a receive.

## Moving Backwards

We introduce a type that keeps track of which instruction is evaluated and with what elements.
Because we know exactly what instructions produce each history instruction, we can reverse the execution

> Caveat: In actual systems it might not be possible to construct the exact reverse of an action.

The history type is defined as: 

```haskell
data History 
    = Skipped
    | Composed 
    | Sent Identifier Identifier
    | Received Identifier Identifier
    | CreatedVariable Identifier
    | CreatedChannel Identifier
    | CalledProcedure Identifier (List Identifier)
    | SpawnedThread ThreadName
    | BranchedOn BoolExp Bool Program 
    | AssertedOn BoolExp
```

With the stack of history instructions that forward evaluation produces, we can undo every instruction in the instruction set.
First of all on a per-thread level:

```haskell
    ( Skipped, restOfProgram ) ->
        continue (Skip : program)

    ( Composed, first : second : restOfProgram ) ->
        continue (Sequence first second : restOfProgram)

    ( CreatedVariable identifier, continuation : restOfProgram ) -> do
        value <- lookupVariable identifier 
        removeVariable identifier 
        continue (Let identifier value continuation : restOfProgram )
```

We have to pattern match both on the history instruction, and the remainder of the program. 

There is some extra logic that will first roll back a child thread before its spawning instruction is rolled back. 

## State Management

many functions have the return type `Interpreter a`, defined as

```
type Interpreter a = StateT Context (Either Error) a
```

Which can be read as "a computation that can read from and write to some state value `Context` and produces `Either` and `Error` or a value of type `a`". 

Where error describes all the errors that can occur in evaluating the program

```haskell
data Error
    = UndefinedVariable Identifier
    | UndefinedThread ThreadName
    | UndefinedChannel Identifier
    | TypeError Identifier String Value
    | AssertionError BoolExp
    | BlockedOnReceive ThreadName 
    | RuntimeException String
```

And context stores variable assignment, the variable count (used to generate fresh variable names), how many children every thread has (used to generate fresh thread names) 
and the available channels. 

*Leading underscores in field names are used to prevent name clashes*
```haskell
data Context = 
    Context 
        { _threads :: Map ThreadName Int
        , variableCount :: Int
        , _bindings :: Map Identifier Value
        , _channels :: Map Identifier (Queue Identifier)
        }
```

Now we can define some helpers, to make writing our interpreter easier:

```haskell
freshIdentifier :: Interpreter Identifier
freshIdentifier = do
    -- get the context 
    context <- State.get

    -- increment the variable count by one
    let new = 1 + variableCount context
    State.put (context { variableCount = new } )

    -- create the fresh identifier 
    return $ Identifier $ "var" ++ show new

insertVariable :: Identifier -> Value -> Interpreter () 
insertVariable identifier value = 
    State.modify $ \context -> 
        let 
            newBindings = Map.insert identifier value (_bindings context)
        in
            context { _bindings = newBindings } 
```

### Sending and Receiving

Sending and receiving is essentially pushing and popping off a queue. A bit of extra plumbing is needed in case

* the channel doesn't exist
* an empty channel is read

The undefined channel error (produced in `withChannel`) will bubble up, `BlockedOnReceive` might get caught higher up and trigger evaluation of other threads. 

```haskell
readChannel :: ThreadName -> Identifier -> Interpreter Identifier
readChannel threadName identifier = 
    withChannel identifier $ \queue ->
        case Queue.pop queue of
            Just ( first, rest ) -> do 
                -- put the rest of the queue back into the context
                mapChannel identifier (\_ -> rest)
                return first

            Nothing ->
                throw $ BlockedOnReceive threadName  


writeChannel :: ThreadName -> Identifier -> Identifier -> Interpreter ()  
writeChannel threadName identifier payload = 
    mapChannel identifier (Queue.push payload)
```


## Tradeoffs 

### Pros

On an abstract level, the implemenations don't differ that much.
The main benefit of this implementation is that **everything is data**. 

* we don't have to spawn threads, nor continually check that the current thread has enough memory available
* the complete state of the debugger can be dumped at any time
* the debugger state can be reliably serialized
* we can easily analyze the debugger state

Additionally, haskell has union types (also called sum types or tagged unions).  
In this case, union types allow us to express the µOz AST structure in 10 lines instead of 10 files. 
When we do a pattern match on our program, the compiler will check that our patterns are exhaustive.

The use of a union type mitigates the need for dynamic casting. The java version uses an enum to store the exact type of an instruction, 
The instruction then has to be dynamically cast to the type that the enum value says it should have. This is inherently unsafe and annoying.

Finally, union types can be used to guarantee that failure cases are handled. Lookup in a key-value map can fail, 
and instead of throwing an exception, haskell gives back a `Maybe a`. To access the `a`, the failure case (the key is missing) must be handled.

*Generality* 

This debugger works on any language that implements the `ReversibleLanguage` type class:

```haskell
class (Eq a, Show a, Show (Value a), Show (History a), Eq (Value a), Eq (History a)) => ReversibleLanguage a where 
    data Value a :: * 
    data History a :: * 
    forwardThread  :: Thread a -> Interpreter (Value a) (Progress (Thread a))
    backwardThread :: Thread a -> Interpreter (Value a) (Progress (Thread a))

    spawn :: a -> a

    spawned :: History a -> Maybe ThreadName
    createdVariable :: History a -> Maybe Identifier

```

Achieving generality in a type-safe way requires the usage of some of haskell's more advanced features. 
Here we define a class with two associated data types: `Value` and `History`. These types need to 
implement the `Show` and `Eq` type classes. 

Furthermore an instance needs to give two functions to move a thread forward and backward, 
a function to spawn a value, and two unpacking functions. 

The instance of the MicroOz language looks like

```
instance ReversibleLanguage.ReversibleLanguage Program where 
    {-| Values in the language. These may appear on the right-hand side of a variable declaration -} 
    data Value Program 
        = VTrue
        | VFalse
        | Receive Identifier
        | Procedure (List Identifier) Program
        | Port 
        | VInt IntExp
        deriving (Show, Eq)


         
    {-| Data type representing actions that have lead to the current program state -} 
    data History Program
        = Skipped
        | Composed 
        | Sent Identifier Identifier
        | Received Identifier Identifier
        | CreatedVariable Identifier
        | CreatedChannel Identifier
        | CalledProcedure Identifier (List Identifier)
        | SpawnedThread ThreadName
        | BranchedOn BoolExp Bool Program 
        | AssertedOn BoolExp
        deriving (Eq, Show)

    forwardThread = advance
    backwardThread = rollback

    spawn = SpawnThread

    createdVariable history =
        case history of
            CreatedVariable name -> Just name
            _ -> Nothing  

    spawned history = 
        case history of
            SpawnedThread name -> Just name
            _ -> Nothing
```


### Cons

* I had to write my own parser



You may considering questions such as the following:
- do both implementations accept and allow to debug the same input
programs? (I would expect so, but you can exploit this to indicate
interesting extensions for your debugger)
- how would users interact with the debugger in each case?
(interfaces, outputs from the tool, etc)
- are there things easier to do in one tool than in the other? (here
again you can use this to highlight the advantages of your approach)

In other words, your comments on 'differences' should reveal that
- you understand well CaReDeb
- you have a good motivation in following a different approach
- you obtained a new solution that is at least as good than CaReDeb
(and that has potential for extensions not present in CaReDeb)


