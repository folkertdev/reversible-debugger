---
documentclass: llncs
title: Reversible Session-Based Concurrency in Haskell\thanks{F.\ de Vries is a BSc student.}
author:  Folkert de Vries\inst{1} \and Jorge A. Pérez\inst{1}\orcidID{0000-0002-1452-6180}
institute: University of Groningen, The Netherlands
authorrunning: F.\ de Vries and J.\ A.\ Pérez
# email: "darth.vader@hs-augsburg.de"
abstract: | 
    Under a reversible semantics, computation steps can be undone. 
    For message-passing, concurrent programs, reversing computation steps is a challenging and delicate task; one typically aims at formal semantics which are \emph{causally-consistent}. 
    Prior work has addressed this challenge in the context of a process model of multiparty protocols (choreographies) following a so-called \emph{monitors-as-memories} approach.
    In this paper, we describe our ongoing efforts aimed at implementing this operational semantics in Haskell. 
    \keywords{Reversible computation \and Message-passing concurrency \and Session Types \and Haskell.}
usepackage: graphicx,hyperref, xcolor,xspace,amsmath,amsfonts,stmaryrd,amssymb,enumerate, mathpartir, fancyvrb
header-includes:
    - \newcommand{\hideFromPandoc}[1]{#1}
    - \hideFromPandoc{
        \let\Begin\begin
        \let\End\end
      }
    - \input{macrosCR}
...

# process calculi 


### the pi-calculus

We must first of all define what concurrent computation is. We use a model called the pi-calculus. 

The pi-calculus is to concurrent computation much like the lambda calculus is for sequental computing: 
A simple model that is convenient for constructing proofs. The pi-calculus is defined as 

\begin{align*}
P, Q, R ::= \, & \overline{x} \langle y \rangle.P \,\,\, \, \, &\text{Send the value }y\text{ over channel }x\text{, then run }P \\
|\,\,\, & x(y).P \,\,\, \, \, & \text{Receive on channel }x\text{, bind the result to }y\text{, then run }P \\
|\,\,\, & P|Q \,\,\, \, \, \, \, \, \, &\text{Run }P\text{ and }Q\text{ simultaneously} \\
|\,\,\, & (\nu x)P  \,\,\, &\text{Create a new channel }x\text{ and run }P \\
|\,\,\, & !P \,\,\, &\text{Repeatedly spawn copies of }P \\
|\,\,\, & 0 & \text{Terminate the process} \\
|\,\,\, & P + Q & \text{(Optionally) Nondeterministic choice}
\end{align*}

Reduction can occur when there is a send and a receive over the same channel in parallel. 

$$\overline{x}\langle z \rangle.P\, |\, x(y).Q \rightarrow P | Q[z/y]$$

The PPDP paper makes a few generalizations to this calculus. 

We still have sending, receiving and parallel, and allow recursion and termination. Channel creation has been removed for simplicity: we'll use only 
one globally available channel. This channel is implemented as a queue which means that sends are non-blocking. 

In addition, we introduce non-deterministic choice. This is where instead of a value, a label is sent from a selector to an offerer. Both parties will pick the 
branch that the label corresponds to. We'll see why this extension is useful in the next section. 

Finally, we allow the sending of thunks - functions that take `Unit` as their argument and return a process term (i.e. a piece of program that can be executed). 
The fact that we can send programs - and not just values - around means that our calculus is a higher-order calculus.

The definition of functions in the PPDP paper is a bit strange: there are two ways of recursion - on the process level and on the value level. 
In the implementation the former has been removed.

## Implementation 

The calculus used in PPDP is given by 

\begin{figure}
\begin{align*}
P,Q \bnfis & 
    \bout{u}{V}{P}  \sbnfbar  \binp{u}{x}{P} 
\sbnfbar \bsel{u}{\lbl_i. P_i}_{i\in I} \sbnfbar \bbra{u}{\lbl_i:P_i}_{i \in I}
 \\
 & 
 \sbnfbar  P \Par Q \sbnfbar  {\rvar{X} \sbnfbar \recp{X}{P}} 
            \sbnfbar  
 {\appl{V}{u}}  
\sbnfbar \news{n} P \sbnfbar \inact
\end{align*}
\end{figure}

As mentioned we omit the process-level recursion. 

```haskell
type Participant = String
type Identifier = String

type Program value = Fix (ProgramF value) 

data ProgramF value next 
    -- transaction primitives
    = Send 
        { owner :: Participant
        , value :: value
        , continuation :: next 
        }
    | Receive 
        { owner :: Participant
        , variableName :: Identifier
        , continuation :: next  
        }

    -- choice primitives
    | Offer Participant (List (String, next))
    | Select Participant (List (String, value, next))

    -- other constructors 
    | Parallel next next 
    | Application Identifier value
    | NoOp
    deriving (Functor) 
```

We also need values in our language. In the paper they are defined as

\begin{figure}
\begin{align*}
u,w  \bnfis& n \sbnfbar x,y,z
\qquad \quad
n,n' \bnfis a,b \sbnfbar \ep{s}{p}
\\
 {v},  {v}'  \bnfis &  \true \sbnfbar \false \sbnfbar \cdots
\\
V,W \bnfis & {a,b} \sbnfbar  x,y,z \sbnfbar  v, v' \sbnfbar {\abs{x}{P}}
\end{align*}
\end{figure}

But to write more interesting examples we've also added some operations on integers and booleans as builtins.

```haskell
data Value 
    = VBool Bool
    | VInt Int
    | VString String
    | VUnit
    | VIntOperator Value IntOperator Value 
    | VComparison Value Ordering Value
    | VFunction Identifier (Program Value)
    | VReference Identifier 
    | VLabel String
```

### session types, global and local

As programmers we would like our programs to work as we expect. With concurrent programs, fitting the whole program in one's head becomes increasingly difficult
as the application becomes larger. 

Data types are a tool to rule out classes of bugs, so the programmer doesn't have to think about them. Type systems range from very loose (dynamic languages like 
javascript and python) to very restrictive (Coq, Agda, Idris). 

In the late 90s, a similar tool was developed for "typing" and checking the interaction between concurrent processes: Session types. 
Session types provide three key properties: 

* **Order** every participant has an ordered list of sends and receives that it must perform
* **Progress** every sent message is eventually received
* **Safety** sender and receiver always agree about the type of the sent value

#### Global Types 

The simplest non-trivial concurrent program has two participants. In this case, the types of the two participants are exactly dual: if the one sends, the
other must receive. However, for the multiparty (3 or more) use case, things aren't so simple.

We need to define globally what transactions occur in our protocol.

```haskell
simple :: GlobalType String
simple = GlobalType.globalType $ do
    transaction "carol" "bob" "Bool"
    transaction "alice" "carol" "int"
```

We can see immediately that safety and progress are guaranteed: we cannot construct a valid global type that breaks these guarantees. 
A more realistic example that we'll use as a running example is the three buyer protocol, defined as follows: 

\begin{align*}
G = ~&  \gtcom{A}{\texttt{V}}{\mathsf{title}}{~~\gtcom{\mathtt{V}}{\{A,B\}}{\mathsf{price}}{\\[-0.5mm] 
& \quad\gtcom{A}{B}{\mathsf{share}}{~~\gtcom{B}{\{A,\mathtt{V}\}}{\mathsf{OK}}{
\\[-0.5mm]
& \quad\quad \gtcom{B}{C}{\mathsf{share}}{~~\gtcom{B}{C}{\textcolor{blue}{\thunkt}}{
\\[-0.5mm]
& \quad\qquad \gtcom{B}{\mathtt{V}}{\mathsf{address}}{~~\gtcom{\mathtt{V}}{B}{\mathsf{date}}{\gend}}}}}}}}
\end{align*}

In this protocol, Alice and Bob communicate with the Vendor about the purchase of some item. Near the end of the protocol, Bob has to leave and 
transfers the remainder of his protocol to Carol. She will also be sent the code - a **thunk process** $\textcolor{blue}{\thunkt}$ - to complete Bob's protocol, and finish the protocol in his name by evaluating the sent thunk. 

The full definition of global types in the haskell implementation is given by


```haskell
type GlobalType participant u = Fix (GlobalTypeF participant u)

data GlobalTypeF participant u next
    = Transaction 
        { from :: participant
        , to :: participant
        , tipe :: u
        , continuation ::  next 
        } 
    | Choice 
        { from :: participant
        , to :: participant
        , options :: Map String next 
        }
    | End
    | RecursionPoint next
    | RecursionVariable
    | Weaken next
    deriving (Functor)
```

We can now see why choice is useful: it allows us to branch on the session type level. 
For instance, one branch can terminate the protocol, and the other can start from the beginning. 

The final three constructors are required for supporting nested recursion. A `RecursionPoint` is a point in the protocol that we can later jump back to. 
A `RecursionVariable` triggers jumping to a previously encountered `RecursionPoint`. By default it will jump to the closest and most-recently encountered `RecursionPoint`, but `WeakenRecursion` makes it jump one `RecursionPoint` higher, encountering 2 weakens will jump 2 levels higher etc.

### Local Types 

A global type can contain inherent parallelism: the order of its steps is not fully defined. Checking for correctness against a global
type is therefore quite hard. The solution is to project the global type onto its participants, creating a local type. 

The local type is an ordered list of steps that the participant must execute to comply with the protocol. Thus the third property **Order** is satisfied
for local types. 

The projection is mostly straightforward, except for choice. Because we allow recursion, a branch of a choice may recurse back to the beginning. When 
this occurs, all participants have to jump back to the beginning, so every choice must be communicated to all participants. 

```haskell
-- some latex snipped with the full projection rules
```

### reversiblilty

The third component of the system is reversibility. The idea here is that we can move back to previous program states, reversing forward steps. 

Reversibility is useful in a concurrent setting because many concurrent systems are transactional: there is a list of steps that we want to 
treat as an atomic action. That means that either we succeed, or we want to not have done anything at all. So if we fail in the middle of the steps, 
we can't just abort: we need to put the system back into its initial state. 

The naive way to achieve reversal is to store a snapshot of the initial state. The memory consumption of this method is a deal-breaker. 
For instance, the system may interact with a large database. Keeping many copies of the database around is inconvenient and may not even be 
physically possible. 

A nicer approach is to keep track of our forward steps, and store just enough information to reverse those steps. Conceptually, we're leaving behind 
a trail of breadcrumbs so we can always find our way back. 

The challenge, then, is to find the minimal amount of information that we need to store for every instruction.
Broadly, we need to track information about two things: the type and the process. 

For the type we define a new data type called `TypeContext`. It 

On the process level there are four things that we need to track: 

* used variable names in receives
* unused branches
* function applications
* messages on the channel

### Abstraction passing is protocol delegation

Effectively this means that instructuions need to keep track of whose protocol they should move forward. 
The `owner` field stores this information.

### combining 

With all the definitions encoded, we can now define forward and backward evaluation of our system. 
Our aim is to implement 

```haskell
type Session a = StateT ExecutionState (Except Error) a

forward  :: Location -> Session ()
backward :: Location -> Session ()
```

Where 

* `ExecutionState` stores the state of the world
* `Error` can be thrown when something fails
* `Location` defines places where code is ran concurrently (threads or machines)

#### Type checking 

we store all the information about a participant in a type called `Monitor`. 

The `TypeContext` and `LocalType` are stored as a tuple. Really, this gives a curser into the local type, where everything to the left is the past 
and everything to the right is the future. 

The next two fields are for keeping track of recursion in the local type. the `recursiveVariableNumber` is an index into the `recursionPoints` list: when 
a `RecursionVariable` is encountered we look at that index to find the new future local type.

Then follow two fields used for reversibility: the stack of used variable names and the store of function applications.
Finally there is a variable store with the currently defined bindings. 

With this data structure in place, we can define `ExecutionState`. It contains some counters for generating unique variable names, a monitor for every participant and
a program for every location. Additionally every location has a default participant and a stack for unchosen branches. 

The message queue is global and thus also lives in the `ExecutionState`. Finally we need a way of peeking into values, to see whether they are functions and if so, to
extract their bodies for application. 


### Benefits of pure functional programming

It has consistently been the case that sticking closer to the formal model gives better code. The abilities that Haskell gives for directly 
specifying formal statements is invaluable. The main killer feature is algebraic data types (ATDs) also known as tagged unions or sum types. 

Observe the formal definition and the haskell data type for global types.
\begin{align*}
    G, G'  \bnfis & \gtcom{p}{q}{U}{G} %\bnfbar 
    \sbnfbar
    \gtcho{p}{q}{\lbl_i}{G_i} %\\
    \sbnfbar %& 
    \mu X. G \sbnfbar X \sbnfbar \gend \\
\end{align*}

```haskell
data GlobalTypeF u next = 
    Transaction {..} | Choice {..}  | R next | V | End | Wk next
```

They correspond directly. Moreover, we know that these are all the ways to construct a value of type `GlobalTypeF` and can 
exhaustively match on all the cases. Functional languages have had these features for a very long time. In recent years 
they have also made their way into non-functional languages (Rust, Swift, Kotlin).  

Secondly, purity and immutability are very valuable in implementing and testing reversibility. The type system can actually guarantee 
that we've not forgotten to revert anything. 

In a pure language, given functions `f :: a -> b` and `g :: b -> a` to prove that f and g are inverses it is enough to prove that 
`f . g = identity && g . f = identity`. In an impure language, we also have to consider the outside world, some context C.

`f (C[x]) = C'[y] => g (C'[y]) = C[x]`

Because we don't need to consider a context here, checking that reversibility works is as simple as comparing initial and final states.

## conclusion

we've given an encoding of the PPDP semantics in the haskell programming language

### Notes on haskell notation and syntax 

A commonly used idiom in our code is to factor out recursion from a data structure, using the `Fix` and `Monad.Free` types.
Both require the data type to be an instance of `Functor`: The type is of the shape `f a` - like `List a` or `Maybe a`, and there exists a mapping function `fmap :: (a -> b) -> (f a -> f b)`. 

Fix requires the data type to have a natural leaf: a constructor that does not contain an `a`. `Free` on the other hand lets us choose some other type for the leaves.

## Fix 

The `Fix` data type is the fixed point type. 

```haskell
data Fix f = Fix (f (Fix f))
```

It allows us to express a type of the shape `f (f (f (f (..))))` concisely. For the 
values of this type to be finite, the `f` must have a constructor that does 
not recurse to be a leaf. Take for instance this simple expression language

```haskell
data Expr
    = Literal Int
    | Add Expr Expr 
```

`Literal` is the only constructor that can occur as a leaf, and `Add` is the only node.
Using `Fix` We can equivalently write 

```haskell
data ExprF next
    = Literal Int
    | Add next next 

simple :: Fix ExprF
simple = Fix (Literal 42)

complex :: Fix ExprF
complex = 
    Fix (Add (Fix (Literal 40)) (Fix (Literal 2)))
```

By decoupling the recursion from the content, we can write functions that deal with only one level 
of the tree and apply them to the full tree. For instance evaluation of the above expression can be written as 

```haskell
evaluate :: Fix ExprF -> Int 
evaluate = 
    Fix.cata $ \expr -> 
        case expr of 
            Literal v -> 
                v

            Add a b -> 
                a + b
```

The `cata` function - a catamorphism also known as a fold or reduce - applies evaluate from the bottom up. In the code we write, we 
only need to make local decisions and don't have to write the plumbing to get the recursion right. 

## Wrapping ProgramF in Monad.Free 

The Free monad is very similar to `Fix`, but allows us to use a different type for the leaves, and 
enables us to use do-notation. Writing expressions with `Fix` can be quite messy, free monads 
allow us to write examples much more succinctly. 

Free is defined as

```haskell
data Free f a
    = Pure a 
    | Free (f (Free f a))
```

and as the name suggests it has a Monad instance. 

This then makes it possible to define a functor that represents instructions, define some helpers and then use do-notation to write our actual programs.

```haskell
data StackF a next 
    = Push a next
    | Pop (a -> next)
    | End 
    deriving (Functor)

type Stack a = Free StackF a
     
program :: Stack Int
program = do 
    push 5 
    push 4 
    a <- pop
    b <- pop
    push (a + b)

push :: a -> Stack a ()
push v = liftFree (Push v ()) 

pop :: Stack a a 
pop = liftFree (Pop identity)
```

### State and StateT

`State` is a wrapper around a function of type `s -> (a, s)`

```haskell
newtype State s a = State { unState :: s -> (a, s) } 
```

Intuitively, we can compose functions of this kind.

```haskell
f :: s -> (a,   s)
g ::       a -> s -> (b, s)
```

And this is exactly what monadic bind for state.
```
andThen :: State s a -> (a -> State s b) -> State s b
andThen (State first) tagger = 
    State $ \s -> 
        let (value, newState) = first s
            State second = tagger value
        in
            second newState 

new :: a -> State s a
new value = State (\s -> (value, s))

instance Monad (State s) where
    (>>=) = andThen
    return = new
```

When we want to combine monads, for instance to have both state and error reporting, we must use monad transformers. 
The transformer is needed because monads don't naturally combine: `m1 (m2 a)` may not have a law-abiding monad instance.


```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }  

instance MonadTrans (StateT s) where 
    lift :: (Monad m) => m a -> StateT s m a
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
```

The `MonadTrans` typeclass defines the `lift` function that wraps a monadic value into the transformer. Next we define an instance 
because we can say "given a monad `m`, `StateT s m` is a law-abiding monad.


### Except
