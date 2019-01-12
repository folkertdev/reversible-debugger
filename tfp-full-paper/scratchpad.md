```haskell
newtype HighLevelProgram a = HighLevelProgram 
        (StateT (Participant, Int) (Free (ProgramF Value)) a)
        deriving ( Functor, Applicative, Monad
                 , MonadState (Participant, Int)
                 , MonadFree (ProgramF Value))
```
