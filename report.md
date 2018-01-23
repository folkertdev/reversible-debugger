# A sequential debugger for parallel programs

Finding bugs in parallel programs is often hard. Existing debugging tools 
can't deal with parallel programs very well. 
 
## My Idea  

The main idea of this paper is to simulate parallel evaluation in a sequential way. 
The debugger state is therefore completely observable at any point, and can be treated as just data: 
modified by pure functions, serialized, sent to a collegue, etc.

This particular debugger is building on the idea that to debug parallel programs, it is valuable to be able to 
step through their forward evaluation, then step back a few steps and pick a different execution path. For instance,
scheduling one thread before another.

The goal is to define a debugger that can step forward and backward in time, allowing for 
complete rewinding (rolling) of threads, variable assignments and the full program.

A future goal - that influenced the implementation - is that we want to introduce session types to this language and model. 
Session types are specifications for the interactions of a process. For instance

    A: send "Ping"
    B: send "Pong"

Thread `A` must send (it cannot perform a receive) the value "Ping" on some channel before it terminates. 
Likewise, thread `B` must send the value "Pong".

## My idea works

To look at how this might function we will define 

* a simple reversible language 
* forward and backward steps for a single thread
* running and scheduling multiple threads
* implementing debugging operations

The practical running example in this paper will be implementing this debugger for the MicroOz language.
MicroOz is a small language for expressing parallel computation. It uses a stack for its instructions.
Its instruction set is expressed as the following data type.

```haskell
data Program 
    -- execute two programs sequentially
    = Sequence Program Program

    -- bind a variable. Value here is an integer, a variable or a new channel
    | Let Identifier (ReversibleLanguage.Value Program) Program

    -- branch on a condition
    | If BoolExp Program Program

    -- spawn a new thread with the given program
    | SpawnThread Program

    -- do nothing
    | Skip

    -- apply function arguments
    | Apply Identifier (List Identifier)

    -- Send channel payload: send the payload over the channel
    | Send Identifier Identifier
```

Recent research has shown that every non-reversible language can be made reversible [Orchard et al.](), but 
the MicroOz language has a defined backward semantics. For every forward step, we keep track of the step
we took and enough data to reverse it.

```haskell
data History 
    = Skipped
    | Composed 
    | Sent Identifier Identifier
    | Received Identifier Identifier
    | CreatedVariable Identifier
    | CreatedChannel Identifier
    | CalledProcedure Identifier (List Identifier)
    | SpawnedThread PID
    | BranchedOn BoolExp Bool Program 
    deriving (Eq, Show)
```

### Moving Forward 

The first primitive we need is to move one thread forward. So essentially: 

```haskell
forwardThread :: Context -> Program -> (History, Context)

data Context a = 
    Context 
        { _threads :: Map PID Int
        , _variableCount :: Int
        , _bindings :: Map Identifier a
        , _channels :: Map Identifier (Queue.Queue String Identifier)
        }
    deriving (Show)
```

Given an instruction and some context (containing variable bindings and other environment information), produce a `History` and an updated context.
The idiom `s -> (a, s)` has some special algebraic structure. It is often written as `State`: 

```
-- define a newtype wrapper around the type `s -> (a, s)`
newtype State s a = State (s -> (a, s))
```

The most important characteristic is its `bind` or `>>=`: Multiple state transformation functions can be chained into one, so for instance this will work as expected, giving unique identifiers.

```haskell

uniqueIdentifier :: State Context String
uniqueIdentifier = do
    -- aquire the state, a value of type Context
    context :: Context <- State.get

    -- convert the variable count to String and prepend "variable_"
    let result = "variable_" ++ show (_variableCount context)

    -- increment the _variableCount in the context, overwrite the state with it
    put (context { _variableCount = _variableCount + 1 })

    -- return the result
    return result

threeNewVariableNames = do
    one <- uniqueIdentifier
    two <- uniqueIdentifier
    three <- uniqueIdentifier
```

The above snippet uses do-notation. This is syntactic sugar for the bind operator. 
The functions bind `>>=` and `return` (and some laws on how they interact) 
correspond to a mathematical concept called a "Monad".  Therefore `State` is sometimes called the "State Monad".

We can rewrite our function above as

```haskell
forwardThread :: Program -> State Context History 
```

**Exceptions** 

Sometimes the evaluation of a program cannot proceed: we've hit an error. There can be many reasons 
like a type mismatch or the use of an undefined variable. 

In Haskell it is recommended to account for errors in your types. This makes sure the error 
needs to be handled at some point. 

The basic type we can use is the `Either` sum type.

```
Either error value = Left error | Right value 
```

Value of this type are either tagged values (with `Left`) of type `error`, or tagged values (with `Right`) of 
type value. To get at the `value`, you have to perform a case split and handle the possible error value
too.

Like state, `Either` has lawful definitions for bind and `return`. 
To integrate the binds and returns of `Either` and `State` with each other, we use
Monad Transformers, written as `<monad name>T` by convention. 

```haskell
data Error = 
    UndefinedVariable Identifier

forwardThread :: Program -> StateT Context (Either Error) History
```

We can now write helpers like so

```haskell
lookupVariable :: Identifier -> StateT Context (Either Error) Value
lookupVariable identifier = do 
    context <- State.get
    case Map.lookup identifier (_bindings context) of 
        Nothing -> 
            Except.throwError (UndefinedVariable identifier)

        Just value -> 
            return value
```

**Effects** 

Effects in this context are changes that a thread wants to happen in the world. Because 
we have a very constrained type signature giving only the information needed to move 
a single thread forward, and because haskell is pure, we can't directly mutate other threads. 
Instead, we can return a `Msg` to signal that something should happen.

In the forward case, there are two effects: items can be pushed on the stack, and a new thread can be spawned. 
The `forwardThread` function signals these actions to its caller with a `Msg`. 

```haskell
data ForwardMsg
    = SpawnThread (Thread History Program) 
    | PushOnStack Program

forwardThread :: Program -> StateT Context (Either Error) (History, List ForwardMsg)
```

Finally, for various bookkeeping requirements, this function needs a unique identifier. 
For now that is just a pid `newtype PID = PID (List Int)`, but with the introduction of session types 
this will become the current actor.

### Backward 

A similar derivation can be made for the backward function, producing:

```haskell
data BackwardMsg
    = RollChild PID 
    | RollReceives 
    | RollSends 

backwardThread :: PID -> History -> List Program -> StateT Context (Either Error) (List Program, List BackwardMsg)
```

The backward function also has some effects (to undo actions that the current thread depends on) and can throw errors. 
A difference is that this function gets the full program stack, because it needs to pattern match on that stack. 

### Multiple Threads

We can now wrap the primitives into functions that keep track of whether progress was actually made. 

```haskell
data Thread = Thread PID (List History) (List Program) 

-- a type alias
type Execution a = StateT (Context Value, ThreadState History Program) (Either Error) a

data Progress work 
    = Done 
    | Step work 

forwardThread  :: Thread -> Execution (Progress Thread, List ForwardMsg)
backwardThread :: Thread -> Execution (Progress Thread, List BackwardMsg)
```

This information is important because we want to move forward by exactly one step. When a thread 
did not make progress and we don't detect it, we might get into infinite loops. 

**Thread Pool** 

A thread pool in our case is a collection of dictionaries (`Map`) for the four states that a thread can be in. 

```haskell
data Pool thread = 
    Pool
        { active :: Map PID thread
        , done :: Map PID thread
        , blocked :: Map PID thread
        , uninitialized :: Map PID thread
        }
```

We borrow the scheduling algorithm from [A Monad for Deterministic Parallelism](https://simonmar.github.io/bib/papers/monad-par.pdf). 
In the forward case, this algorithm will first exhaust the active set, then wake up blocked threads and finally activate uninitialized threads. 
The backward variant will first roll active threads, then blocked ones and finally done ones.

Next it is useful to have one thread "active", so we define 

```haskell
ThreadState
    = Stuck Pool
    | Running Thread Pool 
```

**Applying Effects** 

We can define the functions 

```haskell
handleForwardMsg :: ForwardMsg -> Pool Thread -> Execution (Pool Thread)
handleBackwardMsg :: BackwardMsg -> Pool Thread -> Execution (Pool Thread) 
```

### Channels 

Channels are a distinctive feature of MicroOz: they make the language multi-threaded. 

The Channel module is also the module that has changed the most during development. It is really 
hard to have threads interact over a channel in a principled and reversible way (hence of course the introduction of session types).

```haskell
data Channel a = Channel 
    { histories :: List ChannelHistory 
    , items :: List (Item a)
    }

data ChannelHistory 
    = Pushed PID
    | Popped PID

data Item a = Item 
    { sender :: PID
    , receiver :: PID
    , value :: a 
    }
```

On this data structure we define push and pop (corresponding to send and receive), and 
also helpers to determine whether a thread may roll a send or receive, and if not what threads/actions 
have to be rolled first. 

### Debugger Primitives 

With the above functions, we can construct many debugging options

* move one step forward
* move one step backward
* move a particular thread forward or backward
* evaluate active threads until the next instruction is not a let-instruction. 

Of particular interest is "send/receive normal form": When in this form, a thread is either exhausted
or its next action is a send or a receive. Equivalently, every instructution that doesn't change
the session type state is evaluated.  


### Problems 

I'ts not possible to detect deadlock at the moment: the scheduler will just reschedule all the blocked threads infinitely. 

It is unclear to what extend this code should be general. 

It is quite easy and recommended (in Haskell) that one writes higly polymorphic code. Not only does code reuse get easier, but the 
lack of concreteness means that you can't make assumptions.

On the other hand, this highly general code can become a real mess. 

I feel like at the moment the code is still a bit too general, but because most of the core logic (the µOz semantics) are still 
in flux that seems inevitable.


## Prior Work 

This system is similar to CaReDeb in that it 

* allows for debugging of MicroOz programs, stepping forward and backward, being able to unroll variables, threads and full programs.

But improves on it in the following ways.

* purity 

    The debugger state is completely observable at all times. 

* generality 

    This system can be easily adopted to function for a different language/instruction set: just implement the typeclass and the rest works.

* composability

    The system exposes some basic primitives, on top of which new debugger features can be built.

    The primitives come in two pairs. The first one is `forward` and `backward`. They are used for moving the whole program state or individual threads forward or backward. 
    They also support a filtering function, which allows to target only threads satisfying a predicate to be evaluated. 
    In the case of MicroOz this functionality is convenient to do all initial variable bindings (there are a lot in complex programs) in one go.  

    The second pair is `reschedule` - picking the next active (or when they run out, blocked) thread - and `scheduleThread` which picks a specific thread 
    based on its process ID. `reschedule` is used when the next thread to be worked on doesn't really matter, `scheduleThread` is for fine-grained control of the 
    evaluation order.


A minor hurdle was that I had to write my own parser. It's built with Parsec, so it is pretty solid in terms of performance, but using the same 
grammar as CaReDeb would have been nice.


## Future work 

**Session Types** 

This work has already begun

The syntax is extended to support assigning a session type to each thread. A session type is a specification of the interactions that a thread may have, in this case
when and what it may send or receive on channels. Every time a thread want's to perform some action on a channel, its session type is checked and advanced when the action 
is allowed, or the thread blocks when the action is not allowed.

Further fiddling with the semantics of the language is likely

**Visual Debugging**

Stepping through the program 

We're still looking into how best to visualize the execution and possible execution paths whilst the debugger is running. 
It is impossible to do this a-priori because there is no strict ordering of the instructions.

## experience 

finding abstractions that work well is really hard. 

Haskell is a language where, when the abstraction is good, your life is great. The haskell ecosystem has many great abstractions. 
But for this project I've often had to come up with my own, with varying success. 



