module Practice3 where

import Prelude hiding (Either(..))

import Data.List
import Data.Function

--map :: (a -> b) -> [a] -> [b]

data List a = Nil | Cons a (List a) 
  deriving Show

instance Functor List where 
  fmap f Nil = Nil 
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data Tree a = Leaf 
            | Bin a (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where 
  fmap f Leaf = Leaf
  fmap f (Bin x xs ys) = Bin (f x) (fmap f xs) (fmap f ys)

data RoseTree a = Node { value    :: a
                       , children :: [RoseTree a]
                       }
  deriving (Eq, Show)

instance Functor RoseTree where
  -- fmap :: (a -> b) -> (RoseTree a -> RoseTree b)
  fmap f (Node x xs) = Node (f x) (map (fmap f) xs) 

{-
data Maybe a = Nothing 
             | Just a 
  deriving (Eq, Show)

instance Functor Maybe where 
  fmap f Nothing  = Nothing 
  fmap f (Just x) = Just (f x) 
-}

data Either err a = Left err 
                  | Right a
  deriving (Eq, Show)

instance Functor (Either err) where 
  fmap f (Left x)  = Left x
  fmap f (Right x) = Right (f x)

maybeLookup :: (a -> Bool) -> [a] -> [b] -> Maybe b
maybeLookup p xs ys = fmap (ys !!) 
                    $ findIndex p xs
{-
maybeLookup p xs ys = case mInd of
  Nothing -> Nothing
  Just ind -> Just (ys !! ind)
  where 
    mInd = findIndex p xs
-}

newtype E a = E { appE :: a -> a }

instance Semigroup (E a) where 
  (<>) = E . ((.) `on` appE)