#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = let line = words str in case line of
    ("I":ts:m)   -> LogMessage Info (read ts :: Int) (unwords m)
    ("W":ts:m)   -> LogMessage Warning (read ts :: Int) (unwords m)
    ("E":s:ts:m) -> LogMessage (Error (read s :: Int)) (read ts :: Int) (unwords m)
    _            -> Unknown (unwords line)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node left m'@(LogMessage _ ts' _) right)
    | ts <= ts' = Node (insert m left) m' right
    | otherwise = Node left m' (insert m right)
insert m (Node left (Unknown _) right) = Node left m right

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

isSevere :: Int -> LogMessage -> Bool
isSevere n (LogMessage (Error s) _ _) = n <= s
isSevere _ _ = False

messageFromLog :: LogMessage -> String
messageFromLog (LogMessage _ _ m) = m
messageFromLog (Unknown x) = x

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map messageFromLog . filter (isSevere 50) . inOrder . build

main :: IO ()
main = do
    errors <- readFile "error.log"
    print $ (whatWentWrong . parse) errors
