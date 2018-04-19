module MonadicCalc where

import           Control.Monad (liftM2)

data Expr = Const Int
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Pow Expr Expr deriving (Show)

data ArithmeticError = DivisionByZero | NegativeExponentiation deriving (Eq, Show)

myDiv :: Int -> Int -> Either ArithmeticError Int
myDiv _ 0 = Left DivisionByZero
myDiv a b = Right (a `div` b)

myPow :: Int -> Int -> Either ArithmeticError Int
myPow a b  = if b < 0
             then Left NegativeExponentiation
             else Right (a ^ b)

--Task 1
eval :: Expr -> Either ArithmeticError Int
eval (Const a) = Right a
eval (Add a b) = liftM2 (+) (eval a) (eval b)
eval (Sub a b) = liftM2 (-) (eval a) (eval b)
eval (Mul a b) = liftM2 (*) (eval a) (eval b)
eval (Div a b) = eval a >>= (\a'-> eval b >>= \b' -> myDiv a' b')
eval (Pow a b) = eval a >>= (\a'-> eval b >>= \b' -> myPow a' b')

--Task 2
bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = bin (n-1) >>= \b -> [0:b, 1:b]