#!/usr/bin/env runhaskell
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{- HLINT ignore "Use isNothing" -}

module Calc where

import qualified ExprT
import Parser
import qualified StackVM
import qualified Data.Map as M

eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add n m) = eval n + eval m
eval (ExprT.Mul n m) = eval n * eval m

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp ExprT.Lit ExprT.Add ExprT.Mul s

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT.ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT.ExprT -> ExprT.ExprT
reify = id

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7 . flip mod 7
    add (Mod7 a) (Mod7 b) = lit (mod a 7 + mod b 7)
    mul (Mod7 a) (Mod7 b) = lit (mod a 7 * mod b 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr StackVM.Program where
    lit x = [StackVM.PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = const . Just
    add x y = \m -> (+) <$> x m <*> y m
    mul x y = \m -> (*) <$> x m <*> y m

withVars :: [(String, Integer)]
    -> (M.Map String Integer -> Maybe Integer)
    -> Maybe Integer
withVars vs exp_new = exp_new $ M.fromList vs

main :: IO ()
main = do
    print $ eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) == 20
    print $ evalStr "(2 + 3) * 4" == Just 20
    print $ eval (reify $ mul (add (lit 2) (lit 3)) (lit 4)) == 20

    print $ (testExp :: Maybe Integer) == Just (-7)
    print $ (testExp :: Maybe Bool) == Just True
    print $ (testExp :: Maybe MinMax) == Just (MinMax 5)
    print $ (testExp :: Maybe Mod7) == Just (Mod7 0)
    print $ show (StackVM.stackVM <$> compile "(2 + 3) * 4") == "Just (Right (IVal 20))"

    print $ withVars [("x", 6)] (add (lit 3) (var "x")) == Just 9
    print $ withVars [("x", 6)] (add (lit 3) (var "y")) == Nothing
    print $ withVars [("x", 6), ("y", 3)] (mul (var "x") (add (var "y") (var "x"))) == Just 54
