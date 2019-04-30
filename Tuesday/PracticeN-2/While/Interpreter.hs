module Interpreter where

import Control.Monad.State
import Data.Map (Map(..))
import qualified Data.Map as Map

import Syntax

data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type Eval a = State (Map Var RTVal) a

evalLit :: Lit -> Eval RTVal
evalLit lit = return $ RTLit lit

stateAddPlusOne :: State Int ()
stateAddPlusOne = do
  n <- get
  put (n+1)
-- stateAddPlusOne = get >>= (\n -> put (n+1))

{-
  m1 >>= (\x -> m2[x])

  x <- m1
  m2[x]
-}

evalVar :: Var -> Eval RTVal
evalVar v = do 
  vars <- get
  let mVal = Map.lookup v vars
  case mVal of 
    Just val -> return val 
    Nothing  -> error $ "Undefined variable: " ++ show v

evalExpr :: Expr -> Eval RTVal
evalExpr (Plus lhs rhs) = do 
  lVal <- evalExpr lhs 
  rVal <- evalExpr rhs
  case (lVal, rVal) of 
    (RTLit (LInt n), RTLit (LInt m)) -> n + m
    _ -> error "Type mismatch"

evalWhile :: Statement -> Eval ()
evalWhile (Assign v expr) = do 
  val <- evalExpr expr
  vars <- get
  let vars' = Map.insert v val vars
  put vars'
evalWhile (Seq p q) = do 
  evalWhile p
  evalWhile q