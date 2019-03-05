{-# LANGUAGE InstanceSigs #-}
module Practice4 where

newtype Fun a b = Fun { app :: a -> b }

-- confunctor for range restricted functions
instance Functor (Fun x) where 
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> (x -> a) -> (x -> b)
  fmap :: (a -> b) -> (Fun x a) -> (Fun x b)
  fmap f (Fun funG) = Fun (f . funG)

data InfiniTree k a = Leaf 
                    | Node a (k -> InfiniTree k a)
                   
nestedIx :: InfiniTree k a -> [k] -> Maybe a
nestedIx Leaf _ = Nothing 
nestedIx (Node _ f) (k:ks) = nestedIx (f k) ks
nestedIx (Node x _) [] = Just x

data XYZ = A

data Void

void :: Void 
void = void -- undefined

type Maybe'    a = InfiniTree Void a
type List'     a = InfiniTree () a
type BinTree'  a = InfiniTree Bool a
type RoseTree' a = InfiniTree Int a

leaf :: k -> InfiniTree k a
leaf = const Leaf

mOne :: Maybe' Int
mOne = Node 1 leaf

oneTwoThree :: List' Int
oneTwoThree = Node 1 $ const $ Node 2 $ const $ Node 3 leaf

three :: Maybe Int 
three = nestedIx oneTwoThree [(),()]

binOneTwoThree :: BinTree' Int
binOneTwoThree = Node 1 $ \b -> if b then (Node 2 leaf) else (Node 3 leaf)

two :: Maybe Int
two = nestedIx binOneTwoThree [True]

instance Functor (InfiniTree k) where 
  fmap f Leaf = Leaf 
  fmap f (Node x g) = Node (f x) (fmap (fmap f) g)

-- Maybe (a -> b) -> Maybe a -> Maybe b 
{-
f :: Int -> Int -> Int
x, y :: Int
f x y
mX, mY :: Maybe Int
f <$> mX <*> mY

f <$> mX :: Maybe (Int -> Int)
f <$> mX <*> mY
-}