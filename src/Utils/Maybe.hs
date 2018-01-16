module Utils.Maybe where 

withDefault :: a -> Maybe a -> a
withDefault def m = withDefaultMap def id m


withDefaultMap :: b -> (a -> b) -> Maybe a -> b
withDefaultMap def f m = 
    case m of 
        Nothing -> def
        Just v -> f v


map :: (a -> b) -> Maybe a -> Maybe b
map = fmap
