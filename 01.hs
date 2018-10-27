#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

-- Luhn algorithm

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

-- Tower of Hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi n base target temp
    | n == 0    = []
    | n == 1    = [(base, target)]
    | otherwise = hanoi (n - 1) base temp target
                  ++ hanoi 1 base target temp
                  ++ hanoi (n - 1) temp target base

-- main

main :: IO ()
main = do
    putStrLn "Luhn algorithm:"
    print $ validate 4012888888881881
    putStrLn "Towers of Hanoi (3 pegs, 4 discs):"
    print $ hanoi 4 "a" "b" "c"
