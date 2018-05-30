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





## Global Type 

\begin{align*}
    G, G'  \bnfis & \gtcom{p}{q}{U}{G} %\bnfbar 
    \sbnfbar
    \gtcho{p}{q}{\lbl_i}{G_i} %\\
    \sbnfbar %& 
    \mu X. G \sbnfbar X \sbnfbar \gend \\
\end{align*}

```haskell
type GlobalType u = Fix (GlobalTypeF u)

data GlobalTypeF u next
    = Transaction 
        { from :: Participant
        , to :: Participant
        , tipe :: u
        , continuation ::  next 
        } 
    | Choice 
        { from :: Participant
        , to :: Participant
        , options :: Map String next 
        }
    | R next
    | V
    | Wk next
    | End
    deriving (Show, Functor)
```

## Local Type 

\begin{align*}
    U, U'  \bnfis & \bool \sbnfbar \nat \sbnfbar \cdots %\bnfbar T 
    \sbnfbar \shot{T} \\
    T, T'  \bnfis & \ltout{p}{U}{T} \sbnfbar \ltinp{p}{U}{T} %\\
  \sbnfbar %& 
  \ltsel{p}{\lbl_i}{T_i}{i}{I} \sbnfbar \ltbra{p}{\lbl_i}{T_i}{i}{I}  
 %\lsend{p}{\lbl_i}{U_i}{T_i} \bnfbar \lrecv{q}{\lbl_i}{U_i}{T_i} \bnfbar 
\sbnfbar  \mu X. T \sbnfbar X \sbnfbar \lend 
\end{align*}


```haskell

data LocalTypeF u f 
    = Transaction (Transaction u f)
    | Choice (Choice u f)
    | Atom (Atom f)

data Transaction u f 
    = TSend 
        { owner :: Participant , receiver :: Participant
        , tipe :: u
        , continuation :: f 
        } 
    | TReceive 
        { owner :: Participant , sender :: Participant
        , tipe ::  u
        , continuation :: f 
        , names :: Maybe (Identifier, Identifier)
        } 

data Choice u f 
    = COffer 
        { owner :: Participant
        , selector :: Participant
        , options :: Map String (LocalType u) 
        }
    | CSelect 
        { owner :: Participant
        , offerer :: Participant
        , options :: Map String (LocalType u) 
        }


data Atom f = R f | V | Wk f | End


```

## Local Type with History 

This is where I'm less sure and it looks like the paper version is simpler than what I have.


## Values 


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

```haskell
data Value 
    = VBool Bool
    | VInt Int
    | VString String
    | VIntOperator Value IntOperator Value 
    | VComparison Value Ordering Value
    | VUnit
    | VFunction Identifier (Program Value)
    | VReference Identifier 
    | VLabel String
```

## Process/Program

The formal definition is given by

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

**Question:** Function application is `Value -> Name -> Process`.  Is there a reason the argument is only a name, and not a `Value`?

Which is encoded as this data type

```haskell
data ProgramF value next 
    -- passing messages
    = Send { owner :: Participant, value :: value, continuation :: next }
    | Receive { owner :: Participant, variableName :: Identifier, continuation :: next  }

    -- choice
    | Offer Participant (List (String, next))
    | Select Participant (List (String, value, next))

    | Parallel next next 
    | Application Participant Identifier value
    | NoOp

    -- recursion omitted, see note

    -- syntactic sugar for better examples
    | Let Participant Identifier value next 
    | IfThenElse Participant value next next
```

The four communication operations, `send`, `receive`, `offer` and `select` are constructors. Likewise, parallel execution, function application and 
unit (the empty program) have their own constructors. 

To create slightly more interesting examples, we've also introduced constructors for let-bindings and if-then-else statements. 
These constructions can be reduced to function applications, so they don't add new semantics. They are purely syntactic sugar, but 
do show how one might extend this language with new constructors.

Name restriction is not needed because we use generated variable names that are guaranteed to be unique. 

because values already allow recursion, process recursion can be implemented with value recursion
and function calls, to the point that we've defined

```haskell
recursive :: (HighLevelProgram a -> HighLevelProgram a) -> HighLevelProgram a
recursive body = do
    thunk <- recursiveFunction $ \self _ ->
        body (applyFunction self VUnit)

    applyFunction thunk VUnit
```

## Type Context 

Now that we've defined session types and programs that can go forward, we need to construct the memory that allows us to go backward. 
The first step is backward types.

Formal definition: 

\begin{definition} 
\added{Let $k, k', \ldots$ denote fresh name identifiers.} We define 
\textit{type contexts} as (local) types with one hole, denoted ``$\bullet$'':
\begin{figure}
\begin{align*}
\ctx{T},\ctx{S}   \bnfis  &\bullet 
    \sbnfbar q\btsel{\lbl_w:\ctx{T} \;;\; \lbl_i:S_i}_{i \in I\setminus w}
    \sbnfbar q\btbra{\lbl_w:\ctx{T} \, , \, \lbl_i:S_i}_{i \in I\setminus w}
 \\
  &\sbnfbar   \alpha.\ctx{T} \sbnfbar    k.\ctx{T} \sbnfbar (\loc,\loc_1,\loc_2).\ctx{T} 
 \end{align*}
\end{figure}
\end{definition} 

Data type

```haskell
data TypeContextF a f 
    = Hole 
    | LocalType (LocalTypeF a ()) f 
    | Selected 
        { owner :: Participant
        , offerer :: Participant 
        , selection :: Zipper (String, LocalType a)
        , continuation :: f 
        }
    | Offered 
        { owner :: Participant
        , selector :: Participant 
        , picked :: Zipper (String, LocalType a)
        , continuation :: f 
        }
    | Application Participant Identifier f 
    | Spawning Location Location Location f

    -- sugar
    | Branched { continuation :: f }
    | Assignment { owner :: Participant, continuation :: f }
```

The `Zipper` data type stores the labels and types for each option in-order. A zipper is essentially a triplet `(List a, a, List a)`. 
The order is important in the implementation because every option comes with a condition, and the first option that evaluates to `True` is picked.

The `Branched` and `Assignment` constructors are empty tags - because those operations don't influence the type. 

The `TypeContext` doesn't store any program/value information, only type information

## Reversing programs

There are a couple of ways that information is stored 

**Free Variable Stack**

Bit of a misnomer, because it's actualy a stack of used variable names. When reversing a `receive` or `let`-binding, we use it to get the 
name the programer originally used and the name that internally was assigned to the value. 

**Program Stack**

A stack used to store pieces of program that aren't evaluated: The remaining options in a `select` or `offer`, or the other case in an `ifThenElse`.

**Application History**

A map that stores the function and the argument of a function application. 

**History Queue**

Whenever an element is `receive`d, the element isn't acually removed from the message queue, but moved to the history queue. 

## Monitor

```haskell
data Monitor value tipe = 
    Monitor 
        { _localType :: LocalTypeState tipe
        , _usedVariables :: List Binding 
        , _store :: Map Identifier value 
        , _recursiveVariableNumber :: Int
        , _recursionPoints :: List (LocalType tipe)
        , _applicationHistory :: Map Identifier (value, value)
        }

data Binding = 
    Binding { _visibleName :: Identifier, _internalName :: Identifier } 
```

The monitor uses a type context, where the hole is substituted with a local type. To store the structure with the hole, and the value that shoudl go in the hole, 
we use a 2-tuple to combine them.

The PPDP paper omits reduction rules for recursive local types, but in the actual implementation we need to keep track of the 
recursion points, and what the type from that point would be, so we can later return to them. The application history is also moved into
the monitor. 

The free (i.e. used) variable list is stored in the monitor. Our variables are globally unique, so technically we could store this list globally, but 
we've chosen to follow the PPDP implementation here.

## Global State 

Finally we need some global state that contains all the programs and types and the queue and such.

```haskell
data ExecutionState value = 
    ExecutionState 
        { variableCount :: Int
        , locationCount :: Int
        , applicationCount :: Int
        , participants :: Map Participant (Monitor value String)
        , locations :: Map Location (Participant, List OtherOptions, Program value)
        , queue :: Queue value
        , isFunction :: value -> Maybe (Identifier, Program value)
        }

data OtherOptions  
    = OtherSelections (Zipper (String, Value, Program Value))
    | OtherOffers (Zipper (String, Program Value))
    | OtherBranch Value Bool (Program Value)
    deriving (Show, Eq)
```

## 

type RunningFunction = ( Value, Value, Location )

type RunningFunctions = Map K RunningFunction

data Message = Value Value | Label String

data Tag = Empty | Full

type QueueHalf = List (Participant, Participant, Message)

data Alpha u = Send u | Receive u

data LocalType u = End | SendReceive (Alpha u) (T u) | Select Label (T u) | Offer Label (T u)

data LocalHistoryType u
    = Before T 
    | After T 
    | AfterSendsAndReceives (List Alpha) (T u)
    | Select (Label, (T u)) (Map Label (LocalHistoryType u))
    | Offer (Label, (T u)) (Map Label (LocalHistoryType u))

-- we've removed recursion from the process calculus, so we can drop the free variable list 

data Monitor u 
    = Full ( TypeContext u, LocalType u, Map Identifier Value) 
    | Empty ( TypeContext u, LocalType u, Map Identifier Value)

type K = Identifier 

data TypeContext u 
    = Hole 
    | Offer (Label, TypeContext u) (Map Label (T u)
    | Select (Label, TypeContext u) (Map Label (T u)
    | SendOrReceive (Alpha u)
    | Application K (TypeContext u)
    | Spawned (Location, Location, Location) (TypeContext u)


# Questions 

* We discussed that function recursion is equivalent to Process recursion. 
    With that, I think the free variable store can be dropped from the monitor. Is that correct

* Have we ever discussed configurations? What do they add 

* Similarly, the evaluation and the general contexts. Are they useful in practice or 
only used for proofs? 

* In section 2.2.2: "We require auxiliary definitions for **contexts**, stores, and type contexts."
    Which context is meant there (general or evaluation). Is this important?

* I think I've mixed up or merged "type contexts" and "local types with history". Not sure what's going on, but 
    in the definition of monitors `H` is used, defined (in fig. 4) as "local types with history". But then in the 
    semantics, the H is replaced by T[S] , with T being a "local type context" and S a normal local type (no history). 

    Where did that `H` go? It never seems to be used in the semantics

* I can't find any machinery for recursive local types. When passing a µ, you need to remember that point (and the remaining type) to later return to it. 
    Does this happen anywhere? 

* With the PPDP semantics, can choice ever do something interesting? I feel like there needs to be a way to have values at runtime influence the choice made,
    but there is no mechanism for that.

* We should go over how function creation/calling works. I'd like to turn let-bindings and ifThenElse into function applications, but I'm not sure whether we can. 

* guarded vs non-guarded choice. 
