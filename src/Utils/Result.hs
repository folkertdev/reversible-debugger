module Utils.Result where 

import Control.Monad.Except as Except

type Result = Either

withDefault :: a -> Result err a -> a
withDefault def m = withDefaultMap def id m


withDefaultMap :: b -> (a -> b) -> Result err a -> b
withDefaultMap def f m = 
    case m of 
        Left _ -> def
        Right v -> f v


mapError :: (err1 -> err2) -> Either err1 v -> Either err2 v
mapError f (Left x) = Left (f x)
mapError _ (Right v) = Right v

map :: (a -> b) -> Result x a -> Result x b
map = fmap


lift :: (MonadError error m) => Either error a -> m a
lift (Left e) = Except.throwError e
lift (Right v) = return v

unwrap :: Result a a -> a
unwrap (Left v) = v
unwrap (Right v) = v
