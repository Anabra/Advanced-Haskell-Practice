module ParserWhile where

import Syntax
import ParserBase

bool :: Parser Bool
bool = token "true"  *> pure True
   <|> token "false" *> pure False

lit :: Parser Lit
lit = LBool <$> bool
  <|> LInt <$> nat

------------------------------- until this line ----------------------------------------------


identifier :: Parser String
identifier = (:) <$> letter <*> many alphaNum

var :: Parser Var
var = Var <$> identifier

op :: String -> Parser String
op s = many ws *> token s


expr :: Parser Expr
expr = exprNoLRec
   <|> Mul   <$> exprNoLRec <* op "*" <*> expr
   <|> Plus  <$> exprNoLRec <* op "+" <*> expr
   <|> Minus <$> exprNoLRec <* op "-" <*> expr
   <|> And   <$> exprNoLRec <* op "&&" <*> expr
   <|> Eq    <$> exprNoLRec <* op "==" <*> expr
   <|> LEq   <$> exprNoLRec <* op "<=" <*> expr

exprNoLRec :: Parser Expr
exprNoLRec = ELit <$> lit
         <|> EVar <$> var
         <|> parens expr
         <|> Not <$> (op "!" *> expr)

statement :: Parser Statement
statement = statementNoLRec
         <|> Seq <$> (statementNoLRec <* op ";")
                 <*> lexeme statement

statementNoLRec :: Parser Statement
statementNoLRec
   = If <$> (token "if"   *> lexeme expr) 
        <*> (token "then" *> lexeme statement)
        <*> (token "else" *> lexeme statement)
        <* (token "endif")
 <|> Assign <$> (var <* op ":=") <*> expr
 <|> While <$> (token "while" *> lexeme expr) 
           <*> statement
           <* (token "endwhile")
