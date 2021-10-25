module Writer where
import Control.Monad.Writer ( MonadWriter(writer), Writer, tell )
import Data.Monoid ()

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number:" ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)

multWithLog' :: Writer [String] Int
multWithLog' = logNumber 3 >>= (\a ->
                logNumber 5 >>= (\b ->
                return (a*b)))

multWithLog'' :: Writer [String] Int
multWithLog'' = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (f . g)

instance Monoid (DiffList a) where
    mempty = DiffList ([] ++)

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
    tell ["0"]
finalCountDown' x = do
    finalCountDown' (x-1)
    tell [show x]
