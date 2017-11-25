module Utils where 

import Data.Map as Map (Map, toList, null)
import Data.Monoid ((<>)) 
import Data.List

showMap :: (Show k, Show v) => String -> Map k v -> String
showMap title values = 
    title 
        <> ": "
        <> 
            (if Map.null values then
                showBindings values
            else
             "\n"
                <> showBindings values
            )
        <> "\n"

showBindings :: (Show k, Show v) => Map k v -> String
showBindings bindings = 
    let showBinding k v = show k <> " => " <> show v 
    in
        if Map.null bindings then
            "\t{}"
        else
            "\t{ " <> intercalate "\n\t, " (map (uncurry showBinding) $ Map.toList bindings) <> "\n\t}"

