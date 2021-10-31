module Reader where

addStuff :: Int -> Int
addStuff = 
    (*2)  >>= (\a -> 
    (+10) >>= (\b ->
        return (a+b)))
