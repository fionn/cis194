#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

module MoreFolds where

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a -> (f a :)) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g v -> g (f v x)) id xs base

main :: IO ()
main = do
    print $ xor [True, True, False]
    print $ map' (/=True) [True, True, False]
    print $ foldl (/=) True [True, True, False]
