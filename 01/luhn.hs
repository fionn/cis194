#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

module LuhnAlgorithm where

toDigits :: Int -> [Int]
toDigits n
    | n > 0     = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []

-- I don't use this function
--toDigitsRev :: Int -> [Int]
--toDigitsRev = reverse . toDigits

doubleEveryOtherBackwards :: [Int] -> [Int]
doubleEveryOtherBackwards (x:y:xs) = x : (2 * y) : doubleEveryOtherBackwards xs
doubleEveryOtherBackwards xs = xs

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther = reverse . doubleEveryOtherBackwards . reverse

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs)
    | x > 9     = (sumDigits . toDigits) x + sumDigits xs
    | otherwise = x + sumDigits xs

validate :: Int -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

main :: IO ()
main = do
    putStr "4012888888881881: "
    print $ validate 4012888888881881
