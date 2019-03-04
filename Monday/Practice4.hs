module Practice4 where 

newtype Fun a b = Fun { app :: a -> b }

instance Functor (Fun a) where 
  -- fmap :: (x -> y) -> f x -> f y
  -- f x ~ a -> x
  -- f y ~ a -> y
  -- (x -> y) . (a -> x) ~ (a -> y)
  fmap f (Fun funG) = Fun (f . funG)

data InfiniTree k a = Leaf 
                    | Node a (k -> InfiniTree k a)
            
nestedIx :: InfiniTree k a -> [k] -> Maybe a
nestedIx Leaf _ = Nothing
nestedIx (Node _ f) (k:ks) = nestedIx (f k) ks 
nestedIx (Node x _) [] = Just x 

leaf :: k -> InfiniTree k a
leaf = const Leaf

maybeOne :: Maybe' Int
maybeOne = Node 1 leaf

one :: Maybe Int
one = nestedIx maybeOne []

oneTwoThree :: List' Int
oneTwoThree = Node 1 $ const $ Node 2 $ const $ Node 3 $ leaf

-- three :: Maybe Int
three = nestedIx oneTwoThree [(),()]

binOneTwoThree :: BinTree' Int --InfiniTree Bool Int
binOneTwoThree = Node 1 $ \b -> if b then (Node 2 leaf) else (Node 3 leaf) 

two = nestedIx binOneTwoThree [True]

instance Functor (InfiniTree k) where
  fmap f Leaf = Leaf
  -- fmap f (Node x g) = Node (f x) (\key -> fmap f (g key))
  -- fmap f (Node x g) = Node (f x) (fmap f . g)
  fmap f (Node x g) = Node (f x) (fmap (fmap f) g)

data Void

type Maybe'    a = InfiniTree Void a
type List'     a = InfiniTree () a
type BinTree'  a = InfiniTree Bool a
type RoseTree' a = InfiniTree Int a
