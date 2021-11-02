module ReversePolishNotation where
import Control.Monad.List (foldM)

solve :: IO()
solve = do
    expression <- getLine
    print (solveRPN expression)

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words

foldingFunction :: [Double] -> String -> [Double]
foldingFunction (x:y:ys) "*" = (y * x):ys
foldingFunction (x:y:ys) "+" = (y + x):ys
foldingFunction (x:y:ys) "-" = (y - x):ys
foldingFunction xs numberString = read numberString:xs

solveRPN' :: String -> Maybe Double
solveRPN' st = do
    [result] <- foldM foldingFunction' [] (words (st))
    return result

foldingFunction' :: [Double] -> String -> Maybe [Double]
foldingFunction' (x:y:ys) "*" = return ((y * x):ys)
foldingFunction' (x:y:ys) "+" = return ((y + x):ys)
foldingFunction' (x:y:ys) "-" = return ((y - x):ys)
foldingFunction' xs numberString = fmap (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing 


f :: Int -> Int
f = foldr (.) id [(+8), (*100), (+1)]
