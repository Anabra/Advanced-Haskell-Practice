{-# LANGUAGE InstanceSigs #-}
module ParserBase
  ( module ParserBase
  , module Control.Applicative
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative

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