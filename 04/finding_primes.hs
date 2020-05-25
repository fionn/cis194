#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

module FindingPrimes where

sieveSundaram :: Integer -> [Integer]
sieveSundaram n
              | n < 1 = []
              | otherwise =
                 let k = (n - 1) `div` 2 in
                    let t = takeWhile (< k) $ concatMap (\x -> map (\y -> x + y + (x * y * 2)) [1 .. x]) [1 .. k] in
                        (2 : map (\x -> (2 * x) + 1) (filter (`notElem` t) [1 .. k]))

main :: IO ()
main = do
    print $ sieveSundaram 300
