{-# LANGUAGE InstanceSigs #-}
module State where 

newtype State s a = S { runState :: s -> (a,s) }

instance Functor (State s) where 
  fmap :: (a -> b) -> State s a -> State s b
  fmap = undefined

instance Applicative (State s) where 
  pure :: a -> State s a
  pure = undefined

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) = undefined