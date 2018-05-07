---
documentclass: llncs
title: The power of dark silicon
author: Darth Vader
institute: the research facility
email: "darth.vader@hs-augsburg.de"
keywords: hope, luke, ewoks
abstract: | 
    Sit amet mauris. Curabitur a quam. Aliquam neque. Nam nunc nunc,
    lacinia sed, varius quis, iaculis eget, ante. Nulla dictum justo eu lacus.
    Phasellus sit amet quam. Nullam sodales. Cras non magna eu est consectetuer
    faucibus. Donec tempor lobortis turpis. Sed tellus velit, ullamcorper ac,
    fringilla vitae, sodales nec, purus. Morbi aliquet risus in mi.

...



# Session Types 

Session types describe a protocol between two or more actors. 
The session type describes the interactions that an actor may have (when it can send, when it can receive) and the 
types of values it can send or expect to receive. 

We will look at dynamic session types in particular. Of course it would be nice if all our programs are statically checked for correctness, 
but in the real world, we often want to combine systems written in different languages at different times. 
Therefore, dynamic session types have a lot of value in practice. 

The session types come in two forms: The global type, and the local type. 

The global type contains all the interactions in the program. 


```haskell
a = "Alice"
b = "Bob" 

data MyType 
    = Address
    | ZipCode

globalType :: GlobalType MyType
globalType = do
    transaction A B ZipCode
    transaction B A Address
```

The local type can then be projected to its participants - in this case `A` and `B`.
This projection contains only the interactions that involve a particular actor. 

```haskell
aType = do
    send B ZipCode
    receive B Address

bType = do
    receive A ZipCode
    send A Address
```

Our types can also contain choice 

```haskell
```

And (nested) recursion

```haskell
```

# A language for the types 

To build a prototype for the session types described previously, we also need a value language.

The language is a simple pi-calculus with some added sugar to make interesting examples easier to write 
and to show how the reversing (see below) is implemented for them: 

```haskell
type Participant = String
type Identifier = String

data ProgramF value next 
    -- communication primitives
    = Send { owner :: Participant, value :: value, continuation :: next }
    | Receive { owner :: Participant, variableName :: Identifier, continuation :: next  }

    -- choice primitives
    | Offer Participant (List (String, next))
    | Select Participant (List (String, value, next))

    -- other constructors to make interesting examples
    | Parallel next next 
    | Application Identifier value
    | Let Identifier value next 
    | IfThenElse value next next
    | Literal value 
    | NoOp
    deriving (Eq, Show, Functor)

-- TODO check this definition
type Program = Fix (ProgramF Value) 


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
    deriving (Eq, Show)
```

The above implementations are written in a syntactic sugar called do-notation. Via an interpretation from a high-level description of the syntax
that uses a free monad over a functor (the fact that the result is a monad allows us to use do-notation) the above examples can be compiled to a `Fix (ProgramF value)` 

## Extracting recursion: Fix and Free 

the `Fix` and `Free` data types are two ways of factoring out recursion from a data type definition. 
Both require the data type to be an instance of `Functor`: The type is of the shape `f a` - like `List a` or `Maybe a`, and there exists a mapping function `fmap :: (a -> b) -> (f a -> f b)`. 

Fix requires the data type to have a natural leaf: a constructor that does not contain an `a`. `Free` on the other hand lets us choose some other type for the leaves.

## Fix 

Take for instance this simple expression language

```haskell
data Expr
    = Literal Int
    | Add Expr Expr 
```

We can equivalently write 

```haskell
data ExprF next
    = Literal Int
    | Add next next 

type Expr = Fix ExprF
```

The `Fix` data type is the fixed point type. 

```haskell
data Fix (f :: * -> *) = Fix (f (Fix f))

simple :: Fix ExprF
simple = Fix (Literal 42)

complex :: Fix ExprF
complex = 
    Fix (Add (Fix (Literal 40)) (Fix (Literal 2)))
```

The advantage that this transformation gives us that it is easy to define functions that only deal with one level of the data,  
and use them on the whole structure. For instance evaluation of the above expression can be written as 

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

## The Free Monad 

Free monads bring two improvements 

* They allow us to provide a unit 
* They allow us to use do-notation 

As can be seen in the previous section, writing expressions with `Fix` everywhere is messsy. 

```haskell
data Free (f :: * -> *) a
    = Pure a 
    | Free (f (Free f a))
```

```haskell
data StackF a next 
    = Push a next
    | Pop (a -> next)
    | End 

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

end :: Stack a a 
end = liftFree End

foldFree :: Functor f => (f a -> a -> a) -> Free f a -> a -> a 
foldFree step instruction default = 
    case instruction of 
        Pure v -> 
            
            
evaluate :: Stack a -> State (List a) ()
evaluate instruction = 
    case instruction of 
        Free (Push value rest) -> do
            State.modify (\stack -> value : stack)
            evaluate rest

        Free (Pop continuation) -> do
            stack <- State.get 
            case stack of 
                [] -> error "empty stack"
                x:xs -> do
                    State.put xs
                    continuation x
                    
        Pure value -> 
            return ()
```

The idea then is to use a free monad on our `ProgramF` data type to be able to build a nice DSL out of it. 

We define


```haskell
newtype HighLevelProgram a = 
    HighLevelProgram (StateT (Location, Participant, Int) (Free (ProgramF Value)) a)
        deriving (Functor, Applicative, Monad, MonadState (Location, Participant, Int))
```
    
The state contains the location, owner and a variable counter to generate new unique variable names. 
We can now implement the primitives

```haskell
uniqueVariableName :: HighLevelProgram Identifier
uniqueVariableName = do
    (location, participant, n) <- State.get
    State.put (location, participant, n + 1)
    return $ "var" ++ show n

send :: Value -> HighLevelProgram ()
send value = do
    (_, participant, _) <- State.get
    HighLevelProgram $ lift $ liftFree (Send participant value ())  

receive :: HighLevelProgram Value
receive = do 
    (_, participant, _) <- State.get
    variableName <- uniqueVariableName 
    HighLevelProgram $ lift $ liftFree (Receive participant variableName ())
    return (VReference variableName)

terminate :: HighLevelProgram a
terminate = HighLevelProgram (lift $ Free NoOp)
```

With the local types given above, we can now write correct implementations

```haskell
aType = do
    send B ZipCode
    receive B Address

alice = do 
    let zipcode = VString "4242AB"
    send zipcode
    address <- receive
    terminate

bType = do
    receive A ZipCode
    send A Address

bob = do
    zipcode <- receive
    let address = "mûnewei 42"
    send address
```


# Reversibility

Every local type case needs an "inverse" that contains enough information to undo the action. 
Additionally, the language's instructions (variable binding, if statements, function application, etc.) also need to be reversible. 
