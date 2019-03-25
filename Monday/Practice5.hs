{-# LANGUAGE DeriveFunctor #-}
module Practice5 where

import Data.List
import Data.Maybe

-- Functor
-- fmap :: (a -> b) -> Maybe a -> Maybe b  
-- Applicative
-- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b  

mPlus :: Maybe Int -> Maybe Int -> Maybe Int
-- fmap :: (a -> b) -> Maybe a -> Maybe b
-- fmap :: (Int -> (Int -> Int)) -> Maybe Int -> Maybe (Int -> Int)
mPlus mX mY = (+) <$> mX <*> mY

newtype MyMaybe a = MM (Maybe a) 
  deriving (Eq, Show, Functor) 

instance Applicative MyMaybe where 
  -- (<*>) :: (MyMaybe (a -> b)) -> MyMaybe a -> MyMaybe b
  (<*>) (MM (Just f)) (MM (Just x)) = MM (Just (f x))
  (<*>) _ _ = MM Nothing

  -- pure :: a -> MyMaybe a
  pure x = MM (Just x)

  -- fmap f fX = pure f <*> fX
  -- pure (f x) = f <$> (pure x)

newtype List a = L [a]
  deriving (Eq, Show, Functor)

instance Applicative List where
  pure x = L [x]
  (<*>) (L lF) (L lX) = L [ f x | f <- lF, x <- lX]

newtype Parser a = P { runParser :: (String -> [(a, String)]) }

anyChar :: Parser Char
anyChar = P $ \s ->
  case s of 
    "" -> [] 
    (c:cs) -> [(c, cs)]

char :: Char -> Parser Char
char c = matches (==c)

matches :: (Char -> Bool) -> Parser Char
matches pred = P $ \s ->
  case s of 
    "" -> [] 
    (c:cs) -> if (pred c) 
      then [(c, cs)]
      else []

-- instance Functor Parser where
--  fmap f p = ???

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

execParser :: Parser a -> String -> Maybe a
execParser p = fmap fst 
             . safeHead 
             . filter (null . snd) 
             . runParser p

