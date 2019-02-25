module Practice3 where

import Data.List

import Prelude hiding (Either(..))

data List a = Nil 
            | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where 
  -- fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
  {- Proofs
  [x] fmap id Nil = Nil
  [x] fmap id (Cons x xs) = Cons (id x) (fmap id xs)
      fmap id (Cons x xs) = Cons x xs

  [x] (fmap f . fmap g) Nil = 
      fmap f (fmap g Nil) = Nil
  [x] fmap f (fmap g (Cons x xs)) = 
      fmap f (Cons (g x) (fmap g xs)) = 
      Cons (f (g x)) (fmap f (fmap g xs)) =
      Cons (f . g $ x) (fmap f . fmap g $ xs)
  -}

data Tree a = Leaf 
            | Bin a (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where 
  fmap f Leaf = Leaf 
  fmap f (Bin x lhs rhs) = Bin (f x) (fmap f lhs) (fmap f rhs) 

data RoseTree a = Node { value    :: a
                       , children :: [RoseTree a]
                       }
  deriving (Eq, Show)

instance Functor RoseTree where 
  fmap f (Node x xs) = Node (f x) (map (fmap f) xs)

{-
data Maybe a = Nothing 
             | Just a 
  deriving (Eq, Show)


instance Functor Maybe where 
  fmap f Nothing  = Nothing 
  fmap f (Just x) = Just (f x)
-}

data Either a b = Left a
                | Right b 
  deriving (Eq, Show)

instance Functor (Either a) where 
  fmap f (Right x) = Right (f x)
  fmap f (Left x)  = Left x

maybeLookup :: (a -> Bool) -> [a] -> [b] -> Maybe b
maybeLookup p xs ys = 
  fmap (ys !!) (findIndex p xs)
{-
maybeLookup p xs ys = case mInd of
  Nothing -> Nothing
  Just ix -> Just (ys !! ix)
  where mInd = findIndex p xs
-}
