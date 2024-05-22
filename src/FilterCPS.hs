module FilterCPS (filterCPS) where

filterCPS :: (a -> Bool) -> [a] -> ([a] -> r) -> r
filterCPS _ [] k = k []
filterCPS predicate (x:xs) k =
    filterCPS predicate xs $ \result ->
        if predicate x
            then k (x:result)
            else k result
