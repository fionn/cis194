#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

module Hanoi where

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

main :: IO ()
main = do
    putStrLn "Towers of Hanoi, 3 pegs, 4 discs:"
    print $ hanoi 4 "a" "b" "c"
    putStrLn "Takes 32767 moves for 15 pegs:"
    print $ 32767 == length (hanoi 15 "a" "b" "c")
    putStrLn "Towers of Hanoi, 4 pegs, 4 discs:"
    print $ hanoi4 4 "a" "b" "c" "d"
    putStrLn "Takes 129 moves for 15 pegs:"
    print $ 129 == length (hanoi4 15 "a" "b" "c" "d")
