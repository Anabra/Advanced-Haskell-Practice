module Syntax where

type Name = String

data Lit
  = LBool Bool
  | LInt Int
  deriving (Eq, Show, Ord)

data Var = Var Name deriving (Eq, Show, Ord)

data Expr 
  -- atoms
  = Lit Lit
  | Variable Var
  -- arithmetic
  | Plus Expr Expr
  | Mul Expr Expr
  -- boolean
  | And Expr Expr
  | Not Expr
  deriving (Eq, Ord, Show)

