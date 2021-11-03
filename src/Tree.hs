{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Tree where

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node x (treeInsert x left) right
    | x > a  = Node x left (treeInsert x right)


-- ch.15

data Direction = L | R deriving (Show)

changeToP :: [Direction] -> Tree Char -> Tree Char 
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs' a = [Crumb a]

goLeft' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goLeft' (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goRight' (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

type Zipper a = (Tree a, Breadcrumbs' a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (EmptyTree, bs) = (EmptyTree, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)
