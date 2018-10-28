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

hanoi4 :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
    | n < 2     = hanoi n a b c
    | otherwise = hanoi4 (n - k) a d b c
                  ++ hanoi k a b c
                  ++ hanoi4 (n - k) d b a c
      where k = optimalPartition n

quadraticRoot :: Float -> Float -> Float -> Float
quadraticRoot a b c = (-b + sqrt (b * b - 4 * a * c)) / (2 * a)

optimalPartition :: Int -> Int
optimalPartition = floor . quadraticRoot 1 1 . fromIntegral . negate . (* 2)

-- main

main :: IO ()
main = do
    putStrLn "Luhn algorithm:"
    print $ validate 4012888888881881
    putStrLn "Towers of Hanoi, 3 pegs, 4 discs:"
    print $ hanoi 4 "a" "b" "c"
    putStrLn "Takes 32767 moves for 15 pegs:"
    print $ 32767 == length (hanoi 15 "a" "b" "c")
    putStrLn "Towers of Hanoi, 4 pegs, 4 discs:"
    print $ hanoi4 4 "a" "b" "c" "d"
    putStrLn "Takes 129 moves for 15 pegs:"
    print $ 129 == length (hanoi4 15 "a" "b" "c" "d")
