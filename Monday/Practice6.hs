{-# LANGUAGE InstanceSigs #-}
module Practice6
  ( module Practice6
  , module Control.Applicative
  ) where

import Data.Char
import Data.Maybe
import Control.Applicative

newtype Parser a = P { runParser :: (String -> [(a, String)]) }

evalParser :: Parser a -> String -> Maybe a
evalParser p = fmap fst 
             . listToMaybe 
             . filter (null . snd) 
             . runParser p

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P p) = P (\s -> [ (f x, s') | (x,s') <- p s])

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = P (\s -> [(a,s)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (P p) (P q) = P (\s -> [ (f x, s'') | (f,s') <- p s, (x,s'') <- q s'])

  (*>) :: Parser a -> Parser b -> Parser b
  (*>) p q = pure (\_ y -> y) <*> p <*> q

  (<*) ::Parser a -> Parser b -> Parser a
  (<*) p q = pure (\x _ -> x) <*> p <*> q

-- alternative ~ monoid (applicative functor)
-- different from monoid in the sense that it needs (*->*) type
instance Alternative Parser where 
  empty :: Parser a
  empty = P (const [])

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (P p) (P q) = P (\s -> p s ++ q s)

  some :: Parser a -> Parser [a]
  some p = (:) <$> p <*> many p

  many :: Parser a -> Parser [a]
  many p = some p <|> pure []

anyChar :: Parser Char
anyChar = P $ \s ->
    case s of 
    "" -> [] 
    (c:cs) -> [(c, cs)]

char :: Char -> Parser Char
char c = matches (==c)

token :: String -> Parser String
token [] = pure []
token (c:cs) = (:) <$> char c <*> token cs

matches :: (Char -> Bool) -> Parser Char
matches pred = P $ \s ->
    case s of 
    "" -> [] 
    (c:cs) -> if (pred c) 
        then [(c, cs)]
        else []

digitC :: Parser Char
digitC = matches isDigit

digit :: Parser Int
digit = fmap digitToInt digitC

tuple :: Parser a -> Parser b -> Parser (a,b)
tuple p q = (,) <$> (token "(" *> p) <*> (token "," *> q <* token ")")

list :: Parser a -> Parser [a]
list p = token "[" *> 
        (((:) <$> p <*> (list')) 
        <|> pure []) 
        <* token "]" where
  -- list' :: Parser [a]
  list' = many (token "," *> p) 