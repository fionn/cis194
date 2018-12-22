#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

module Golf where

everyN :: [a] -> Int -> [a]
everyN xs n = case drop (n - 1) xs of
    y:ys -> y : everyN ys n
    []   -> []

skips :: [a] -> [[a]]
skips xs = [everyN xs i | i <- [1 .. length xs]]

-- Alternative hopscotch function
--skips xs =  zipWith skip [1 ..] (replicate (length xs) xs)

--skip :: Int -> [a] -> [a]
--skip n l = [snd x | x <- zip [1 ..] l, fst x `mod` n == 0]

localMaxima :: [Int] -> [Int]
localMaxima (x:y:z:zs)
    | x < y && y > z = y : localMaxima (y:z:zs)
    | otherwise      = localMaxima(y:z:zs)
localMaxima _ = []

histSpot :: Int -> Int -> [Int] -> String
histSpot x y l = if length (filter (x ==) l) > y then "*" else " "

maxHist :: [Int] -> Int
maxHist l = maximum [length (filter (x ==) l) | x <- [0..9] ]

histogram :: [Int] -> String
histogram l = concat [concat [histSpot x y l | x <- [0 .. 9]]
                             ++ "\n" | y <- reverse [0 .. maxHist l - 1]] 
                     ++ "==========" ++ "\n0123456789\n"

testHopscotch :: Bool
testHopscotch = skips "ABCD" == ["ABCD", "BD", "C", "D"] 
                && skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
                && skips [1 :: Int] == [[1]]
                && skips [True, False] == [[True, False], [False]]
                && null (skips [])

testLocalMaxima :: Bool
testLocalMaxima = localMaxima [2, 9, 5, 6, 1] == [9, 6]
                  && localMaxima [2, 3, 4, 1, 5] == [4]
                  && null (localMaxima [1, 2, 3, 4, 5])

main :: IO ()
main = do
    putStr "hopscotch: "
    print testHopscotch
    putStr "local maxima: "
    print testLocalMaxima
    putStrLn "Histogram:"
    putStr $ histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9]
