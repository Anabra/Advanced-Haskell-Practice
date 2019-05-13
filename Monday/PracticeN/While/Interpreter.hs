module Interpreter where

import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.State
import Data.Map (Map(..))
import qualified Data.Map as Map

import Syntax

data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type Eval' a = ExceptT String (StateT (Map Var RTVal) Identity) a 
type Eval  a = StateT (Map Var RTVal) (ExceptT String Identity) a
{-
runStateT  :: StateT s m a  -> s -> m (a,s) 
runState   :: State s a     -> s ->   (a,s)

runExceptT :: ExceptT e m a -> m (Either e a) 
runExcept  :: Except e a    ->   (Either e a) 
-}
runEval' :: Eval' a -> Map Var RTVal -> (Either String a, Map Var RTVal)
-- x := runExceptT m :: State (Map Var RTVal) (Either String a)  
-- runState s x :: (Either String a, Map Var RTVal)
runEval' m s = runState (runExceptT m) s
runEval  :: Eval a -> Map Var RTVal -> Either String (a, Map Var RTVal)
-- x := runStateT m s :: Except String (a, Map Var RTVal)
-- runExcept x :: Either String (a, Map Var RTVal)
runEval  m s = runExcept (runStateT m s)

evalLit :: Lit -> Eval RTVal
evalLit lit = return $ RTLit lit

evalVar :: Var -> Eval RTVal
evalVar v = do 
  vars <- get
  let mVal = Map.lookup v vars
  case mVal of 
    Just rtVal -> return rtVal
    Nothing    -> lift $ throwE $ "Not defined variable: " ++ show v

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
    _ -> lift $ throwE $ "Type mismatch between expressions: " 
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