module Selective where

import Prelude hiding (Applicative(..))
import qualified Prelude as P (Applicative(..))

import Data.Bool (bool)
import Applicative (Apply(..), Applicative(..), pure, (<*>))
import Decisive (Decide(..))

-- Represents a context with choice (whether static or dynamic)
class Functor f => Select f
  where
  branch :: f (Either a b) -> f (a -> c) -> f (b -> c) -> f c

type Selective f = (Select f, Applicative f)

-- If we support static parallelism (including doing nothing), and support some form of choice
-- (static or dynamic), we can choose between applying a transformation and doing nothing
select :: Selective f => f (Either a b) -> f (a -> b) -> f b
select b d = branch b d (pure id)

(<*?) :: Selective f => f (Either a b) -> f (a -> b) -> f b
(<*?) = select

infixl 4 <*?

-- This can be written in terms of only the ability to choose
ifS :: Select f => f Bool -> f a -> f a -> f a
ifS x t e = branch (bool (Right ()) (Left ()) <$> x) (const <$> t) (const <$> e)

-- This requires both the ability to chooes
whenS :: Selective f => f Bool -> f () -> f ()
whenS x y = bool (Right ()) (Left ()) <$> x <*? (const <$> y)

-- Contexts that support both static choice and static parallelism can be used for branching computations.
-- These computations support static analysis.
newtype Static f a = Static { getStatic :: f a }
  deriving (Functor, Decide, Apply)

instance (Decide f, Apply f) => Select (Static f)
  where
  branch b x y = Static $ continue (decide $ getStatic b) (getStatic x) (getStatic y)
    where
    continue (Left  fa) fac _   = flip ($) <$> fa <*> fac
    continue (Right fb) _   fbc = flip ($) <$> fb <*> fbc

-- Contexts that only support dynamic choice can also be used for branching computations.
-- However, these computations don't support static analysis.
newtype Dynamic f a = Dynamic { getDynamic :: f a }
  deriving (Functor, P.Applicative, Monad)

instance Monad f => Select (Dynamic f)
  where
  branch fab fac fbc = fab >>= either (\a -> fmap ($ a) fac) (\b -> fmap ($ b) fbc)

-- Contexts that only support static parallelism can't branch at all, so the only way they
-- could implement `select` would be by performing all three computations and discarding the
-- result of one. IMO this should not be a lawful implementation of `Select`
newtype Force f a = Force { getForce :: f a }
  deriving (Functor, Apply)

instance Apply f => Select (Force f)
  where
  branch fab fac fbc = either <$> fac <*> fbc <*> fab
