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

evalVar :: Var -> Eval RTVal
evalVar v = do 
  vars <- get
  let mVal = Map.lookup v vars
  case mVal of 
    Just rtVal -> return rtVal
    Nothing    -> error $ "Not defined variable: " ++ show v

evalExpr :: Expr -> Eval RTVal 
evalExpr (ELit lit) = evalLit lit
evalExpr (EVar var) = evalVar var
evalExpr (Plus lhs rhs) = do 
  lhs' <- evalExpr lhs 
  rhs' <- evalExpr rhs
  case (lhs', rhs') of 
    (RTLit (LInt n), RTLit (LInt m)) -> return 
                                      . RTLit 
                                      . LInt 
                                      $ (n+m)
    _ -> error $ "Type mismatch between expressions: " 
                 ++ show lhs ++ " and " ++ show rhs  

evalWhile :: Statement -> Eval ()
evalWhile (Seq p q) = evalWhile p >> evalWhile q
evalWhile (Assign v expr) = do 
  val <- evalExpr expr
  modify $ Map.insert v val

  {-
  vars <- get
  let vars' = Map.insert v val vars
  put vars'
  -}