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
    | Esc
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

        continue (CalledProcedure functionName arguments) (withRenamedVariables : Esc : rest)
                    
    Send channelName variable -> do
        writeChannel name channelName variable 
        continue (Sent channelName) rest

    Esc ->
        continue HistoryEsc rest
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

This logic is implemented in [`forward`][] and its helpers

## Moving Backwards

Because we know exactly 

The history type is defined as: 

```haskell
data History 
    = Skipped
    | Composed 
    | Sent Identifier
    | Received Identifier Identifier
    | CreatedVariable Identifier
    | CreatedChannel Identifier
    | CalledProcedure Identifier (List Identifier)
    | SpawnedThread ThreadName
    | BranchedOn BoolExp Bool Program 
    | AssertedOn BoolExp
    | HistoryEsc
```

With the stack of history instructions that forward evaluation produces, we can undo every instruction in the instruction set.
First of all on a per-thread level:

```haskell
    ( Skipped, restOfProgram ) ->
        continue (Skip : program)

    ( Composed, first : Esc : second : Esc : restOfProgram ) ->
        continue (Sequence first second : restOfProgram)

    ( CreatedVariable identifier, continuation : Esc : restOfProgram ) -> do
        value <- lookupVariable identifier 
        removeVariable identifier 
        continue (Let identifier value continuation : restOfProgram )
```

We have to pattern match both on the history instruction, and the remainder of the program. 


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
readChannel :: ThreadName -> Identifier -> MonadInterpreter Identifier
readChannel threadName identifier = 
    withChannel identifier $ \queue ->
        case Queue.pop queue of
            Just ( first, rest ) -> do 
                -- put the rest of the queue back into the context
                mapChannel identifier (\_ -> rest)
                return first

            Nothing ->
                throw $ BlockedOnReceive threadName  


writeChannel :: ThreadName -> Identifier -> Identifier -> MonadInterpreter ()  
writeChannel threadName identifier payload = 
    mapChannel identifier (Queue.push payload)
```


## Tradeoffs 

I personally feel that Haskell as a language is much better suited for writing this kind of tools than the mainstream imperative languages. 
Features like union types and pattern matching make implementing compilers and interpreters so much easier. 

Additionally, haskell's purity guides good design. 


On an abstract level, the implemenations don't differ that much 

pros

* everything is data  
    No threads are spawned, no real channels used: everything is data


* union types 
    Makes is so that the µOz AST structure can be 10 lines instead of 10 files. The compiler will also check that all cases have been handled.

* pure
    An added benefit is that the interpreter can't mutate the outside world by accident. 

* guaranteed error handling
    For instance in 

* abstracting away control flow 

    The java code has to check very often whether the current thread is out of memory, has to do dynamic casts, and repeatedly checks that values are valid. 
    The Haskell code doesn't have to deal with 1, uses union types to make 2 safe, and also uses types to ensure values are always valid and failures are always handled.
cons


* Parser
    The Java implementation uses a parser generator, I had to write one on my own 


