{-# LANGUAGE InstanceSigs #-}
module State where 

newtype State s a = S { runState :: s -> (a,s) }

instance Functor (State s) where 
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (S g) = S $ \s -> let (x,s') = g s in (f x, s')

instance Applicative (State s) where 
  pure :: a -> State s a
  pure x = S (\s -> (x,s))

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (S sF) (S g) = S _