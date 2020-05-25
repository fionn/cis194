#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

module BinaryTree where

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
    where
        insert :: a -> Tree a -> Tree a
        insert x Leaf = Node 0 Leaf x Leaf
        insert x (Node _ left val right)
            | treeHeight left <= treeHeight right = 
                let new_left = insert x left
                in Node (treeHeight new_left + 1) (insert x left) val right
            | otherwise = 
                let new_right = insert x right
                in Node (treeHeight new_right + 1) left val (insert x right)

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node height _ _ _) = height

main :: IO ()
main = do
    print $ foldTree "abcdefghij"
