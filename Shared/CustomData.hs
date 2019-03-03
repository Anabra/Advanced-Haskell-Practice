module CustomData where

import Prelude hiding (Maybe(..), Either(..))

data Maybe a = Nothing
  | Just a
  deriving (Eq, Show)

data Either a b = Left a
                | Right b
  deriving (Eq, Ord, Show)

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x)  = Left x