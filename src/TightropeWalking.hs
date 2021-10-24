module TightropeWalking where

type Birds = Int 
type Pole = (Birds, Birds)

balance :: Birds
balance = 4

calcPole :: Pole -> Maybe Pole
calcPole (left, right)
    | abs (left - right) < balance = Just (left, right)
    | otherwise                    = Nothing

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) = calcPole (left + n, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) = calcPole (left, right + n)

banana :: Pole -> Maybe Pole
banana _ = Nothing

tightropeWalking :: Maybe Pole
tightropeWalking = return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

routine :: Maybe Pole
routine = do
    start <- return (0, 0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

