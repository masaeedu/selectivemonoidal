module Alternative where

import Data.Void

class Functor f => Alt f
  where
  union :: (f a, f b) -> f (Either a b)

class Alt f => Alternative f
  where
  nil :: () -> f Void

empty :: Alternative f => f a
empty = absurd <$> nil ()

(<|>) :: Alt f => f a -> f a -> f a
(<|>) x y = either id id <$> union (x, y)
