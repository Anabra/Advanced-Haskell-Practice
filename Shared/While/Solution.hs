{-# LANGUAGE RecordWildCards, InstanceSigs #-}
module Solution where

import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative

import Data.Function (on)

import Control.Monad.Identity hiding (ap)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Error.Class
import Data.Map (Map(..))
import qualified Data.Map as Map

------------------------------------ Syntax ----------------------------------

data Lit
  = LBool Bool
  | LInt Int
  deriving (Eq, Ord, Show)

type Name = String

newtype Var = Var Name
  deriving (Eq, Ord, Show)

data Expr
  -- atoms
  = ELit Lit
  | EVar Var
  -- arithmetic
  | Plus Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  -- logical
  | And Expr Expr
  | Eq Expr Expr
  | LEq Expr Expr
  | Not Expr
  deriving (Eq, Ord, Show)

data Statement
  = Seq Statement Statement
  | If Expr Statement Statement
  | While Expr Statement
  | Assign Var Expr
  deriving (Eq, Ord, Show)

---------------------------------- ParserBase ----------------------------------

newtype Parser a = P { runParser :: String -> [(a, String)] }

evalParser :: Parser a -> String -> Maybe a
evalParser p = fmap fst
             . listToMaybe
             . filter (null . snd)
             . runParser p

instance Functor Parser where
  fmap f (P p) = P (fmap (fmap (mapFst f)) p)
    where mapFst f (x,y) = (f x, y)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = P (\s -> [(a,s)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (P pF) (P pX) = P (\s -> [ (f x,s'') | (f,s') <- pF s, (x,s'') <- pX s'])

  (*>) :: Parser a -> Parser b -> Parser b
  (*>) q p = (\_ y -> y) <$> q <*> p

  (<*) :: Parser a -> Parser b -> Parser a
  (<*) q p = (\x _ -> x) <$> q <*> p

-- monoid on applicative functors
instance Alternative Parser where
  empty :: Parser a
  empty = P (const [])

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (P p) (P q) = P (\s -> p s ++ q s)

  some :: Parser a -> Parser [a]
  some p = (:) <$> p <*> many p

  many :: Parser a -> Parser [a]
  many p = some p <|> pure []

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
(<||>) (P p) (P q) = P $ \s -> let r = p s in
    if null r then q s else r

-- applictive constraint
instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (P p) g = P $ \s ->
    concat [ runParser (g x) s' | (x,s') <- p s ]

char :: Char -> Parser Char
char c = matches (== c)

anyChar :: Parser Char
anyChar = matches (const True)

matches :: (Char -> Bool) -> Parser Char
matches p = P $ \s ->
  case s of
    (x:xs) | p x -> [(x,xs)]
    _ -> []

digitC :: Parser Char
digitC = matches isDigit

letter :: Parser Char
letter = matches isAlpha

alphaNum :: Parser Char
alphaNum = matches isAlphaNum

-- motivation for Functor instance
digit :: Parser Int
digit = fmap digitToInt digitC

-- motivation for Applicative instance
tuple :: Parser a -> Parser b -> Parser (a,b)
tuple p q = (,) <$> (char '(' *> p <* char ',') <*> (q <* char ')')

-- motivation for Alternative instance
list :: Parser a -> Parser [a]
list p = char '[' *> (((:) <$> p <*> list') <|> pure []) <* char ']' where
  list' = many (char ',' *> p)

-- motivation 2 for Applicative
token' :: String -> Parser String
token' (c:cs) = (:) <$> char c <*> token' cs
token' []     = pure ""

token :: String -> Parser String
token s = lexeme (token' s)

nat :: Parser Int
nat = foldl' (\acc cur -> acc*10 + cur) 0 <$> some digit

-- parse one whitespace
ws :: Parser ()
ws = matches isSpace *> pure ()

-- parse that p parses then all whitespaces
lexeme :: Parser a -> Parser a
lexeme p = p <* many ws

between :: String -> String -> Parser a -> Parser a
between before after p =
  token before *> lexeme p <* token after

--between "asd" "qwe" digit -> "asd   5  qwe  "

parens :: Parser a -> Parser a
parens = between "(" ")"

-- run parser p n times
times :: Int -> Parser a -> Parser [a]
times 0 p = pure []
times n p = (:) <$> p <*> times (n-1) p

-- times 3 digit "123" -> Just [1,2,3]
-- times 3 anyChar "abc" -> Just ['a','b','c']
-- times 3 digit "12" -> Nothing (cannot parse anything)
-- times 2 digit "123" -> ([1,2], "3")
-- times 0 digit "" -> Just []
-- times 0 digit "asd" -> ([],"asd")

nCoords :: Parser [(Int,Int)]
nCoords = lexeme nat >>= (\n -> n `times` lexeme (tuple nat nat))

-- nCoords "2(12,3)(3,12)" -> Just [(12,3), (3,12)]
-- nCoords "2 (12,3) (3,12)" -> Just [(12,3), (3,12)]
-- nCoords "2\n(12,3)\n(3,12)" -> Just [(12,3), (3,12)]

---------------------------------- ParserWhile ----------------------------------

bool :: Parser Bool
bool = token "true"  *> pure True
   <|> token "false" *> pure False

lit :: Parser Lit
lit = LBool <$> lexeme bool
  <|> LInt <$> lexeme nat

identifier :: Parser String
identifier = (:) <$> letter <*> many alphaNum

var :: Parser Var
var = Var <$> lexeme identifier

op :: String -> Parser String
op s = token s


expr :: Parser Expr
expr = lexeme exprNoLRec
   <|> reorderExprChain <$> exprChainE

exprNoLRec :: Parser Expr
exprNoLRec = ELit <$> lit
        <||> EVar <$> var
        <||> parens expr
        <||> Not <$> (op "!" *> expr)

statement :: Parser Statement
statement = statementNoLRec
         <|> Seq <$> (statementNoLRec <* op ";")
                 <*> lexeme statement

statementNoLRec :: Parser Statement
statementNoLRec
   = If <$> (token "if"   *> lexeme expr)
        <*> statement
        <*> (token "else" *> lexeme statement)
        <*  (token "endif")
   <||> Assign <$> (var <* op ":=") <*> expr
   <||> While <$> (token "while" *> lexeme expr)
             <*> statement
             <*  (token "endwhile")

operator :: Parser Operator
operator = op "*" *> pure OpMul
      <||> op "+" *> pure OpPlus
      <||> op "-" *> pure OpMinus
      <||> op "&&" *> pure OpAnd
      <||> op "==" *> pure OpEq
      <||> op "<=" *> pure OpLEq

-- won't allow zero-length expression chains
exprChainE :: Parser (AltList Expr Operator)
exprChainE = ACons <$> exprNoLRec <*> exprChainO

exprChainO :: Parser (AltList Operator Expr)
exprChainO = ACons <$> operator <*> exprChainE
        <||> pure Nil

reorderExprChainE :: AltList Expr Operator -> Expr
reorderExprChainE (ACons e Nil) = e
reorderExprChainE (ACons e xs) = reorderExprChainO xs e

reorderExprChainO :: AltList Operator Expr -> Expr -> Expr
reorderExprChainO (ACons op xs) e = ap op e (reorderExprChainE xs)

reorderExprChain :: AltList Expr Operator -> Expr
reorderExprChain Nil = error "Should not have come here"
reorderExprChain xs@(ACons e Nil) = e
reorderExprChain xs
  | pos <- maxPosBy (compare `on` precedence) xs
  , ys <- applyAtPos pos xs
  = reorderExprChain ys

applyAtPos :: Int -> AltList Expr Operator -> AltList Expr Operator
applyAtPos 0 (ACons lhs (ACons op (ACons rhs xs))) = ACons (ap op lhs rhs) xs
applyAtPos n (ACons lhs (ACons op xs)) = (ACons lhs (ACons op (applyAtPos (n-2) xs)))
applyAtPos _ xs = xs

------------------------------------ Util ----------------------------------

data Operator = OpMul | OpPlus | OpMinus | OpAnd | OpEq | OpLEq
  deriving (Eq, Ord, Show)

precedence :: Operator -> Int
precedence OpMul   = 7
precedence OpPlus  = 6
precedence OpMinus = 6
precedence OpAnd   = 3
precedence OpEq    = 4
precedence OpLEq   = 4

ap :: Operator -> Expr -> Expr -> Expr
ap OpMul   = Mul
ap OpPlus  = Plus
ap OpMinus = Minus
ap OpAnd   = And
ap OpEq    = Eq
ap OpLEq   = LEq

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

----------------------------------- Interpreter ----------------------------------

data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type VarMapping = Map Var RTVal

type Eval a = StateT VarMapping (ExceptT String Identity) a

runEval :: Eval a -> VarMapping -> Either String (a, VarMapping)
runEval m s = runExcept (runStateT m s)

evalLit :: Lit -> Eval RTVal
evalLit lit = return $ RTLit lit

evalVar :: Var -> Eval RTVal
evalVar v = do
  vars <- get
  let mVal = Map.lookup v vars
  case mVal of
    Just val -> return val
    Nothing  -> throwError $ "Undefined variable: " ++ show v

evalBinOp :: (Expr -> Eval a) ->
             (Expr -> Eval b) ->
             (c -> RTVal) ->
             (a -> b -> c) ->
             (Expr -> Expr -> Eval RTVal)
evalBinOp evalLhs evalRhs mkRetVal op lhs rhs = do
  lhs' <- evalLhs lhs
  rhs' <- evalRhs rhs
  let result = lhs' `op` rhs'
  return $ mkRetVal result

evalInt :: Expr -> Eval Int
evalInt e = do
  e' <- evalExpr e
  case e' of
    RTLit (LInt n) -> return n
    _ -> throwError $ show e ++ " does not evaluate to an Integer"

evalBool :: Expr -> Eval Bool
evalBool e = do
  e' <- evalExpr e
  case e' of
    RTLit (LBool b) -> return b
    _ -> throwError $ show e ++ " does not evaluate to a Boolean"

mkRTInt :: Int -> RTVal
mkRTInt = RTLit . LInt

mkRTBool :: Bool -> RTVal
mkRTBool = RTLit . LBool

evalUnaryOp :: (Expr -> Eval a) ->
               (b -> RTVal) ->
               (a -> b) ->
               (Expr -> Eval RTVal)
evalUnaryOp evalArg mkRetVal op arg =
  evalBinOp evalArg evalArg mkRetVal (const <$> op) arg arg
  -- const <$> op is similar to: \lhs rhs -> op lhs

evalExpr :: Expr -> Eval RTVal
evalExpr (ELit l) = evalLit l
evalExpr (EVar v) = evalVar v
evalExpr (Plus lhs rhs) = evalBinOp evalInt evalInt mkRTInt (+) lhs rhs
evalExpr (Minus lhs rhs) = evalBinOp evalInt evalInt mkRTInt (-) lhs rhs
evalExpr (Mul lhs rhs) = evalBinOp evalInt evalInt mkRTInt (*) lhs rhs
evalExpr (And lhs rhs) = evalBinOp evalBool evalBool mkRTBool (&&) lhs rhs
evalExpr (LEq lhs rhs) = evalBinOp evalInt evalInt mkRTBool (<=) lhs rhs
evalExpr (Not arg) = evalUnaryOp evalBool mkRTBool (not) arg

evalWhile :: Statement -> Eval ()
evalWhile (Assign v expr) = do
  val <- evalExpr expr
  vars <- get
  let vars' = Map.insert v val vars
  put vars'
evalWhile (Seq p q) = do
  evalWhile p
  evalWhile q
evalWhile (If cond thenBr elseBr) = do
  b <- evalExpr cond
  case b of
    RTLit (LBool True)  -> evalWhile thenBr
    RTLit (LBool False) -> evalWhile elseBr
evalWhile loop@(While cond body) = do
  b <- evalExpr cond
  case b of
    RTLit (LBool True)  -> evalWhile body >> evalWhile loop
    RTLit (LBool False) -> pure ()

