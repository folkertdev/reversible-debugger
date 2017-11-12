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


## My idea works

The practical running example in this paper will be implementing this debugger for the MicroOz language.
MicroOz is a small language for expressing parallel computation. It uses a stack for its instructions.
Its instruction set can be captured in the following data type.

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

    -- assert a condition
    | Assert BoolExp
```

The execution needs an environment to store variable bindings. 
The evaluation uses renaming, so we also need to be able to generate unique variable names.


```haskell
data Context a = 
    Context 
        { _threads :: Map PID Int
        , _variableCount :: Int
        , _bindings :: Map Identifier a
        , _channels :: Map Identifier (Queue.Queue Identifier)
        }
    deriving (Show)
```

Finally we need a type that shadows the instruction set, and keeps enough information to 
undo any forward step

```
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
    | AssertedOn BoolExp
    deriving (Eq, Show)
```

We can now define our computations 

```haskell
data Thread a = Thread PID (List (History a)) (List a)  

-- a type alias
-- context contains the value associated with program
type Interpreter value a = State (Context value) a

data Progress work 
    = Done 
    | Step work 
    | Blocked work
    | Branched work work

forwardThread  :: Thread Program -> Interpreter (Value Program) (Progress  (Thread Program))
backwardThread :: Thread Program -> Interpreter (Value Program) (Progress  (Thread Program))
```

The accompanying code also does error handling, but we'll omit that for now. The `State` types 
makes a value of type `Context value` available for reading and writing in a pure way. The progress 
type is used to indicate to the caller how it should proceed. 

### Generalizing 

The references to the `Program` type in the previous snippet can be removed, allowing 
for a fully abstract representation of what a type needs to be or implement in order to be 
reversed.

```haskell
class ReversibleLanguage a where 
    -- associated type for values
    data Value a :: * 

    -- associated type for history
    data History a :: * 

    -- individual thread primitives
    forwardThread  :: Thread a -> Interpreter (Value a) (Progress  (Thread a))
    backwardThread :: Thread a -> Interpreter (Value a) (Progress  (Thread a))

    -- the instruction that spawns a new thread
    spawn :: a -> a

    -- functions used in moving backward to inspect an instruction 
    spawned :: History a -> Maybe PID
    createdVariable :: History a -> Maybe Identifier
```

We need 

* an associated data type for the values in our program
* an associated data type for the history of our program
* primitives for moving threads forward and backward
* the spawning instruction (to handle the `Branched work work` case of progress)
* history instruction inspection functions: 
    - to undo a spawned thread, we need to undo the whole program the thread ran first
    - to undo a variable, we need to remove all its occurences first

### Multiple threads

With these primitives we can build a scheduler and functions that moves a program with many threads one step forward or backward. 

Again we'll need to keep track of some context:

```haskell
type Threads a = Map PID (Thread a)

data ThreadState a = 
    ThreadState
        { active :: Threads a
        , inactive :: Threads a
        , blocked :: Threads a
        , filtered :: Threads a
        } 
```

With these types we can define `reschedule` which picks the next thread to be exectuted (pop from active threads, when empty try to wake up blocked threads), and `scheduleThread` which 
tries to schedule a particular thread and errors when it is inactive, filtered, or non-existent.

```haskell
reschedule :: ThreadState program -> Either (ThreadState program) (Thread program, ThreadState program)

scheduleThread :: ReversibleLanguage program 
    => PID 
    -> Thread program 
    -> ThreadState program 
    -> Either ThreadScheduleError (Thread program, ThreadState program) 
```

With the above primitives we can define two stepping functions, that
evaluate the program in one thread with one instruction.

```
schedule :: ReversibleLanguage program 
    => (Thread program -> Bool)
    -> Thread program 
    -> ThreadState program
    -> Interpreter (Value program) (Either (ThreadState program) (Thread program, ThreadState program))


unschedule :: ReversibleLanguage program 
    => (Thread program -> Bool)
    -> Thread program 
    -> ThreadState program
    -> Interpreter (Value program) (Either (ThreadState program) (Thread program, ThreadState program))

```

The `(Thread program -> Bool)` argument is for filtering. This is a convenience which allows the program to be evaluated up to some point. 
An example is to evaluate all let-bindings in all active threads: there can be many of them and typically they are not very interesting, so we 
can skip them all in one go.

With this we can define the main functions of our debugger

```haskell
-- either there is a current thread or not 
-- if there is no current thread the program is done
type ExecutionState a = Either  (ThreadState a) (Thread a, ThreadState a)

init :: ReversibleLanguage program => Thread program -> Interpreter (Value program) (ExecutionState program)
init thread = 
    schedule (const True) thread (ThreadState Map.empty Map.empty Map.empty Map.empty)

forward :: ReversibleLanguage program => Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program)
forward = 
    schedule (const True) 

backward :: ReversibleLanguage program => Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program)
backward thread@(Thread name history program) =
    unschedule (const True) thread 
```

With the above functions, we can generate many debugging options

* move one step forward
* move one step backward
* move a particular thread forward or backward
* evaluate active threads until the next instruction is not a let-instruction. 


### Problems 

I'ts not possible to detect deadlock at the moment: the scheduler will just reschedule all the blocked threads infinitely. 


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

* improve debugging experience 
* implement different `ReversibleLanguage` instances




