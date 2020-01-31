module Applicative where

import Prelude hiding (Applicative(..), zip)
import qualified Prelude as P (Applicative(..))

import Data.Functor.Const

-- Represents a context with static parallelism
class Functor f => Apply f
  where
  zip :: (f a, f b) -> f (a, b)

  default zip :: P.Applicative f => (f a, f b) -> f (a, b)
  zip (fa, fb) = (,) <$> fa P.<*> fb

-- Doing all of no tasks
class Apply f => Applicative f
  where
  husk :: () -> f ()

  default husk :: P.Applicative f => () -> f ()
  husk = P.pure

(<*>) :: Apply f => f (a -> b) -> f a -> f b
(<*>) fab fa = fmap (uncurry ($)) $ zip (fab, fa)

infixl 4 <*>

(*>) :: Apply f => f a -> f b -> f b
(*>) fa fb = flip const <$> fa <*> fb

liftA2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
liftA2 abc fa fb = abc <$> fa <*> fb

pure :: Applicative f => a -> f a
pure a = a <$ husk ()

instance Apply IO
instance Applicative IO

instance Monoid m => Apply (Const m)
instance Monoid m => Applicative (Const m)
