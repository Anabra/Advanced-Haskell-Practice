module Practice1 where

import Prelude hiding (Either(..))

-- type synonym
type CoordSyn = (Int,Int)

-- newtype, no runtime overhead, tags will disappear
newtype CoordNew = Coord (Int, Int)

-- Algebraic Data types
-- union (+)
-- descartes product (*)
-- unit

data BoolOrInt = B Bool 
               | I Int
  deriving Show

-- data BoolAndInt = BI Bool Int
-- instead of above, record syntax
data BoolAndInt = BI { _bool :: Bool
                     , _int  :: Int
                     }
                | IB { _int  :: Int 
                     , _bool :: Bool
                     }
  deriving Show

getBool :: BoolAndInt -> Bool
getBool = _bool

getInt :: BoolAndInt -> Int
getInt (BI _ i) = i
getInt (IB i _) = i


data Pair a b = Pair { _fst :: a 
                     , _snd :: b
                     }
  deriving Show 

-- constrained instance
instance (Eq a, Eq b) => Eq (Pair a b) where
  (==) (Pair a1 b1) (Pair a2 b2) = a1 == a2 && b1 == b2

data Maybe a = Nothing 
             | Just a
  deriving Show

-- []  :: [a] 
-- (:) :: a -> [a] -> [a]
data List a = Nil 
            | Cons a (List a)
  deriving (Show, Eq)

data Union a b   = Left a | Right b
data Product a b = Product a b
data Unit        = Unit

instance (Eq a, Eq b) => Eq (Union a b) where 
  (==) (Left x) (Left y) = x == y
  (==) (Right x) (Right y) = x == y
  (==) _ _ = False

instance (Eq a, Eq b) => Eq (Product a b) where
  (==) (Product a1 b1) (Product a2 b2) = a1 == a2 && b1 == b2

instance Eq Unit where 
  (==) x y = True

newtype GList a = GL (Union Unit (Product a (GList a)))

-- constrained instance
{-
instance Eq a => Eq (List a) where
  (==) Nil Nil = True
  (==) Nil _   = False
  (==) _   Nil = False
  (==) (Cons x xs) (Cons y ys) = x == y && xs == ys
-}



data Exp = Add Exp Exp 
         | Mul Exp Exp 
         | Atom Int
  deriving Show

-- lhs, rhs :: Exp
evalExp :: Exp -> Int
evalExp (Atom i) = i
evalExp (Add lhs rhs) = evalExp lhs + evalExp rhs 
evalExp (Mul lhs rhs) = evalExp lhs * evalExp rhs 


data BinTree a = Leaf
               | Bin a (BinTree a) (BinTree a)