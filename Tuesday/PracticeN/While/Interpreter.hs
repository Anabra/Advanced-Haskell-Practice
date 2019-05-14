module Interpreter where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Map (Map(..))
import qualified Data.Map as Map

import Syntax

data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type Eval' a = ExceptT String (StateT (Map Var RTVal) Identity) a
type Eval  a = StateT (Map Var RTVal) (ExceptT String Identity) a

{-
runExcept  :: Except  e   a ->   (Either e a)
runExceptT :: ExceptT e m a -> m (Either e a)

runState  :: State  s   a -> s ->   (a,s)
runStateT :: StateT s m a -> s -> m (a,s)
-}

runEval' :: Eval' a -> Map Var RTVal -> (Either String a, Map Var RTVal)
-- y := runExceptT m :: StateT (Map Var RTVal) Identity (Either String a)
-- runState y s :: (Either String a, Map Var RTVal)
runEval' m s = runState (runExceptT m) s

runEval :: Eval a -> Map Var RTVal -> Either String (a, Map Var RTVal)
-- x := runStateT m s :: ExceptT String Identity (a, Map Var RTVal)
-- runExcept x :: Either String (a, Map Var RTVal)
runEval m s = runExcept (runStateT m s)

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
    Nothing  -> lift $ throwE $ "Undefined variable: " ++ show v

evalExpr :: Expr -> Eval RTVal
evalExpr (Plus lhs rhs) = do 
  lVal <- evalExpr lhs 
  rVal <- evalExpr rhs
  case (lVal, rVal) of 
    (RTLit (LInt n), RTLit (LInt m)) -> return $ RTLit (LInt $ n + m)
    _ -> lift $ throwE "Type mismatch"

evalWhile :: Statement -> Eval ()
evalWhile (Assign v expr) = do 
  val <- evalExpr expr
  vars <- get
  let vars' = Map.insert v val vars
  put vars'
evalWhile (Seq p q) = do 
  evalWhile p
  evalWhile q

{-
data Identity a = Identity a
State s a ~ StateT s Identity a

get :: StateT s m s 
put :: s -> StateT s m ()

throwE :: e -> ExceptT e m a
catchE :: (e -> ExceptT e' m a) -> ExceptT e' m a

class MonadTrans (t :: * -> * -> *) where
  lift :: Monad m => m a -> t m a

  ExceptT e Id a -> StateT s (ExceptT e Id) a 

-- throwE :: e -> m' e a
class MonadExcept (t (m' e) :: * -> *) where 
  throwE :: ( MonadExcept (m' e)
            , MonadTrans t
            ) => e -> t (m' e) a
  throwE = lift . throwE
-}