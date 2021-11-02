module Probability where

import Data.Ratio ( (%) )
import GHC.Show (Show)
import Data.Functor ()
import Control.Monad ()
import Control.Applicative (Applicative)

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative Prob where
    pure x = Prob [(x, 1%1)]
    Prob fs <*> Prob xs = Prob [(f x, p1*p2) | (f, p1) <- fs, (x, p2) <- xs]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

instance Monad Prob where
    m >>= f = flatten (fmap f m)
