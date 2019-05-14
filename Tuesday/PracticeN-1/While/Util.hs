module Util where

import Data.List
import Data.Function (on)

import Syntax

data Operator = OpMul | OpPlus | OpMinus | OpConcat | OpAnd | OpEq | OpLEq
  deriving (Eq, Ord, Show)

precedence :: Operator -> Int
precedence OpMul    = 7
precedence OpPlus   = 6
precedence OpMinus  = 6
precedence OpConcat = 5
precedence OpAnd    = 3
precedence OpEq     = 4
precedence OpLEq    = 4

ap :: Operator -> Expr -> Expr -> Expr
ap OpMul    = Mul
ap OpPlus   = Plus
ap OpMinus  = Minus
ap OpConcat = Concat
ap OpAnd    = And
ap OpEq     = Eq
ap OpLEq    = LEq

data AltList a b = ACons a (AltList b a) | Nil
  deriving (Eq, Ord, Show)

firsts :: AltList a b -> [a]
firsts (ACons x xs) = x : seconds xs
firsts Nil = []

seconds :: AltList a b -> [b]
seconds (ACons x xs) = firsts xs
seconds Nil = []

-- only works for AltLists with at least 2 elements
maxPosBy :: (b -> b -> Ordering) -> AltList a b -> Int
maxPosBy cmp xs
  | ys <- seconds xs
  , iys <- zip [0..] ys
  , (ix,_) <- maximumBy (cmp `on` snd) iys
  = 2*ix