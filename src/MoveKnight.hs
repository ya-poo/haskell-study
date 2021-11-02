module MoveKnight where
import Control.Monad (guard, (<=<))

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1),
                    (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

traceKnightIn3 :: KnightPos -> KnightPos -> [[KnightPos]]
traceKnightIn3 start end = do
    first <- moveKnight start
    second <- moveKnight first
    third <- moveKnight second
    guard (third == end)
    return [start, first, second, third]

inMany :: Int -> KnightPos -> [KnightPos]
inMany x = foldr (<=<) return (replicate x moveKnight)
