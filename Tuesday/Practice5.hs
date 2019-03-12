{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Practice5 where

import Data.Maybe

{-
g :: a -> (b -> c)
x :: a 
y :: b
fX :: f a
fY :: f b

-- pure application
g $ x $ y

-- fun app in context
fmap :: (a -> (b -> c)) -> f a -> f (b -> c)
g <$> fX <*> fY
-}

mAdd :: Maybe Int -> Maybe Int -> Maybe Int
mAdd mX mY = (+) <$> mX <*> mY

newtype MyMaybe a = MM (Maybe a)
  deriving (Eq, Show, Functor)

instance Applicative MyMaybe where 
  pure :: a -> MyMaybe a
  pure x = MM (Just x)

  (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  (<*>) (MM (Just f)) (MM (Just x)) = (MM (Just (f x)))
  (<*>) _ _ = MM Nothing

newtype List a = L [a]
  deriving (Eq, Show, Functor)

instance Applicative List where 
  pure :: a -> List a
  pure x = L [x] 

  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) (L fs) (L xs) = L [ f x | f <- fs, x <- xs ]

newtype Parser a = P { runParser :: String -> [(a, String)] }

char :: Char -> Parser Char
char c = P $ \s -> 
  case s of 
    "" -> []
    (x:xs) | x == c -> [(c,xs)]
    _ -> []

evalParser :: Parser a -> String -> Maybe a
evalParser p = fmap fst
             . listToMaybe
             . filter (null . snd) 
             . runParser p 