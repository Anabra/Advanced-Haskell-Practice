module ParserWhile where

import Data.Function (on)

import Util
import Syntax
import ParserBase

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