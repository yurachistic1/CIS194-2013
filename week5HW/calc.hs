{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Calc where 

import ExprT
import Parser
import  qualified StackVM as SVM

-- Part One --

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add n1 n2) = eval n1 + eval n2
eval (Mul n1 n2) = eval n1 * eval n2

-- Part Two -- 

evalStr :: String -> Maybe Integer
evalStr str = 
    case (parseExp Lit Add Mul str) of 
        Just a  -> Just (eval a)
        Nothing -> Nothing

-- Part Three --

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a


instance Expr ExprT where
    lit e       = Lit e
    add e1 e2   = Add e1 e2
    mul e1 e2   = Mul e1 e2


reify :: ExprT -> ExprT
reify = id

--Part Four --

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit e     = e
    add e1 e2 = e1 + e2
    mul e1 e2 = e1 * e2

instance Expr Bool where 
    lit e       = if e <= 0 then False else True
    add e1 e2   = e1 || e2
    mul e1 e2   = e1 && e2

instance Expr MinMax where 
    lit e                     = MinMax e
    add (MinMax a) (MinMax b) = MinMax $ max a b
    mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
    lit e                 = Mod7 $ mod e 7
    add (Mod7 a) (Mod7 b) = Mod7 $ mod (a + b) 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a * b) 7


testExp :: Expr a => Maybe a
testExp     = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Part Five -- 

instance Expr SVM.Program where
    lit e = [ SVM.PushI e ]
    add x y = x ++ y ++ [SVM.Add]
    mul x y = x ++ y ++ [SVM.Mul]

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul