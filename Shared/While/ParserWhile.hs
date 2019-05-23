{-# LANGUAGE LambdaCase, MultiWayIf #-}

module ParserWhile where

import Data.Char (isDigit, digitToInt, isSpace, isAlpha, isAlphaNum)
import Data.Function (on)
import Data.Functor (($>))
import Data.Maybe (isJust, fromJust)
import Control.Monad (MonadPlus(..), guard, liftM2, liftM3)

import Util
import Syntax
import ParserBase

bool :: Parser Bool
bool =  (token "true"  $> True)
    <|> (token "false" $> False)

lit :: Parser Lit
lit = LBool <$> lexeme bool
  <|> LInt <$> lexeme nat

identifier :: Parser String
identifier = (:) <$> letter <*> many alphaNum

var :: Parser Var
var = Var <$> lexeme identifier

op :: String -> Parser String
op = token

expr :: Parser Expr
expr = lexeme exprNoLRec
   <|> reorderExprChain <$> exprChainE

exprNoLRec :: Parser Expr
exprNoLRec = peek >>= \c -> if
  | isDigit c -> ELit <$> lit
  | isAlpha c -> EVar <$> var
  | '(' == c  -> parens expr
  | '!' == c  -> Not <$> (anyChar >> expr)
  | otherwise -> mzero

statement :: Parser Statement
statement = statementNoLRec
         <|> Seq <$> (statementNoLRec <* op ";")
                 <*> lexeme statement

statementNoLRec :: Parser Statement
statementNoLRec = lexeme (untilC isAlphaNum) >>= \str -> case str of
  "if"    -> liftM3 If    (lexeme expr) statement (op "else" >> lexeme statement <* op "endif")  
  "while" -> liftM2 While (lexeme expr) (statement <* token "endwhile")
  _     -> do 
    let v = evalParser var str
    guard (isJust v)
    Assign (fromJust v) <$> (token ":=" >> expr)


operator :: Parser Operator
operator = anyChar >>= \case 
  '*' -> pure OpMul
  '+' -> pure OpPlus
  '-' -> pure OpMinus
  '&' -> char '&' >> pure OpAnd
  '=' -> char '=' >> pure OpEq
  '<' -> peek >>= \case
    '=' -> anyChar $> OpLEq
    _   -> pure OpLT
  _   -> mzero

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
applyAtPos n (ACons lhs (ACons op xs)) = ACons lhs (ACons op (applyAtPos (n-2) xs))
applyAtPos _ xs = xs