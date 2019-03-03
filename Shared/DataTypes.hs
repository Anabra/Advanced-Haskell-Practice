module DataTypes where

-- Definitions

data Nat = Zero
         | Suc Nat
  deriving (Eq, Ord, Show)

data List a = Nil
            | Cons a (List a)
  deriving (Eq, Ord, Show)

data BinTree a = Leaf
               | Bin a (BinTree a) (BinTree a)
  deriving (Eq, Show)

data RoseTree a = Node { value    :: a
                       , children :: [RoseTree a]
                       }
  deriving (Eq, Show)


-- Instances

instance Functor BinTree where
  fmap f Leaf = Leaf
  fmap f (Bin x lhs rhs) = Bin (f x) (fmap f lhs) (fmap f rhs)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Functor RoseTree where
  fmap f (Node x xs) = Node (f x) (map (fmap f) xs)
