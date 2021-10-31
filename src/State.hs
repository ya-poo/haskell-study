{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module State where
import Control.Monad.State (State, state, MonadState (get, put))

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get 
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]

stackyStack' :: State Stack ()
stackyStack' = get >>= (\stackNow ->
        if stackNow == [1,2,3]
            then put [8,3,1]
            else put [9,2,1]
    )
