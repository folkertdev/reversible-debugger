Free monads are a mechanism to conveniently construct domain-specific languages. 
Because Haskell provides special syntax for working with monads (do-notation), Free monads are a common technique. 

The core idea is that we define a set of instructions. `Free` constructs a tree out of these instructions, which can then be interpreted and evaluated. 
The `Monad` instance for `Free` makes it possible to use do-notation.

Free is defined as 
```haskell	
data Free f a	
    = Pure a 	
    | Free (f (Free f a))	
```	

A practical example is a stack-based calculator:
```haskell	
data Operation next  
    = Push Int next
    | Pop (Maybe Int -> next) 
    | End
    deriving (Functor)  

type Program next = Free Operation next
type TerminatingProgram = Free Operation Void
```

Now when we take the free monad on the `Operation` functor, this generates a list (degenerate tree) of instructions. 
In a lazy language, this list is not guaranteed to be finite. If the list is not finite, evaluating it will cause a stack overflow. 
We use the `Void` type to ensure the tree is finite with an `End` at the very bottom. An alternative approach is to use 
existential quantification, but that requires enabeling a language extension. 

The `Void` type is the type with zero values. A value of the type `Free Operation Void` by definition cannot end in the shape `Pure _`, because it requires a value of type `Void` (which don't exist).
It also cannot end in `Push _ _` or `Pop _` because these values contain or produce a `next` argument. 
Therefore the final operation must be `End`, and thus values of type `Free Operation Void` are finite and can be safely interpreted. 

We can defined some wrappers around the constructors for convenience, and use them to write programs with do-notation.
```haskell
push :: Int -> Program ()
push v = liftF (Push v ())   

pop :: Program (Maybe Int) 
pop = liftF (Pop id)

terminate :: TerminatingProgram  
terminate = liftF End

program :: TerminatingProgram  
program = do    
    push 5  
    push 4  
    Just a <- pop    
    Just b <- pop    
    push (a + b)    
    terminate
```

Finally, we expose a function to evaluate the structure we've built (but only if it is finite). Typically, a `Free` monad is transformed into some other monad, which in turn is evaluated. 
Here we interpred first to `State`, and then evaluate that.

```haskell
interpret :: TerminatingProgram -> State [Int] () 
interpret instruction = 
    case instruction of
        Pure _ -> 
            -- cannot occur
            return () 

        Free End -> 
            return () 

        Free (Push a rest) -> do 
            State.modify (\state -> a : state)
            evaluator rest

        Free (Pop toNext) -> do
            state <- State.get
            case state of 
                x:xs -> do
                    State.put xs
                    evaluator (toNext (Just x))

                [] -> 
                    evaluator (toNext Nothing)

evaluate :: TerminatingProgram -> [Int] 
evaluate = flip execState [] . interpret 
```

Many of the `ProgramF` constructors require an `owner`, we can thread the owner through a block with a wrapper around `Free`. 
We use `StateT` containing the owner and a counter to generate unique variable names.

```haskell
newtype HighLevelProgram a = 
    HighLevelProgram (StateT (Participant, Int) (Free (ProgramF Value)) a)
        deriving (Functor, Applicative, Monad
        , MonadState (Participant, Int), MonadFree (ProgramF Value))

uniqueVariableName :: HighLevelProgram String
uniqueVariableName = do
    (participant, n) <- State.get
    State.put (participant, n + 1)
    return $ "var" ++ show n

send :: Value -> HighLevelProgram ()
send value = do
    (participant, _) <- State.get
    liftF (Send participant value ())  

receive :: HighLevelProgram Value
receive = do 
    (participant, _) <- State.get
    variableName <- uniqueVariableName 
    liftF (Receive participant variableName ())
    return (VReference variableName)

terminate :: HighLevelProgram a
terminate = liftF NoOp

compile :: Participant -> HighLevelProgram Void -> Program Value
compile participant (HighLevelProgram program) = do
    runStateT program (participant, 0) 
```

We can now implement the `Vendor` from the ThreeBuyer example as: 
```haskell
vendor :: H.HighLevelProgram a
vendor = do
    t <- H.receive
    H.send (price t)
    H.send (price t)
    ...
    terminate
```

```haskell
globalType1 = do
    message A V Title
    message V B Price
    message V A Price
    message A B Share

globalType2 = do
    message A V Title
    message V A Price
    message V B Price
    message A B Share
```
