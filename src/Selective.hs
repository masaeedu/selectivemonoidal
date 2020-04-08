{-# LANGUAGE ImpredicativeTypes, TupleSections #-}
module Selective where

import Prelude hiding (Applicative(..))
import qualified Prelude as P (Applicative(..))

import Data.Bool (bool)
import Data.Function ((&))

import Alternative (Alt(..), (<|>))
import Applicative (Apply(..), Applicative(..), pure, (<*>), liftA2)
import Decisive (Decide(..))

import Data.Coerce (coerce)

newtype Fork f a b = Fork { runFork :: f (Either a b) }
  deriving Functor

type Select f = forall a. (Functor f, Apply (Fork f a))
type Selective f = (Applicative f, Select f)

-- {{{ COMBINATORS
swapE :: Either a b -> Either b a
swapE = either Right Left

swapF :: Functor f => Fork f a b -> Fork f b a
swapF (Fork fab) = Fork $ swapE <$> fab

lift :: Functor f => f a -> Fork f x a
lift = Fork . fmap Right

merge :: Functor f => Fork f a a -> f a
merge = fmap (either id id) . runFork

branch :: Select f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch feab fac fbc =
  let
    fecb = (&) <$> swapF (Fork feab) <*> lift fac
    fecc = (&) <$> swapF fecb <*> lift fbc
  in
  merge fecc

selectR :: Selective f => f (Either a b) -> f (b -> a) -> f a
selectR v = branch v $ pure id

selectL :: Selective f => f (Either a b) -> f (a -> b) -> f b
selectL v = flip (branch v) $ pure id

(<*?) :: Selective f => f (Either a b) -> f (a -> b) -> f b
(<*?) = selectL

infixl 4 <*?

-- This can be written in terms of only the ability to choose
ifS :: Select f => f Bool -> f a -> f a -> f a
ifS x t e = branch (bool (Right ()) (Left ()) <$> x) (const <$> t) (const <$> e)

-- This requires both the ability to choose and the ability to perform both actions
whenS :: Selective f => f Bool -> f () -> f ()
whenS x y = bool (Right ()) (Left ()) <$> x <*? (const <$> y)

-- }}}

-- {{{ STATIC SELECT

-- Contexts that support both static choice and static parallelism can be used for branching computations.
-- These computations support static analysis.
newtype Static f a b = Static { getStatic :: Fork f a b }
  deriving Functor

instance (Decide f, Alt f, Apply f) => Apply (Static f a)
  where
  zip (Static (Fork fa), Static (Fork fb)) = Static $ Fork $ go (decide fa) (decide fb)
    where
    go (Left x) (Left y) = Left <$> x <|> y
    go (Left x) (Right _) = Left <$> x
    go (Right _) (Left x) = Left <$> x
    go (Right x) (Right y) = fmap Right $ (,) <$> x <*> y

-- }}}

-- {{{ DYNAMIC SELECT

-- Contexts that only support dynamic choice can also be used for branching computations.
-- However, these computations don't support static analysis.
newtype Dynamic f a = Dynamic { getDynamic :: f a }
  deriving (Functor, Apply, Applicative, P.Applicative, Monad)

instance Monad f => Apply (Fork (Dynamic f) a)
  where
  zip (Fork (Dynamic fa), fb)= Fork $ Dynamic $ fa >>= either (return . Left) (\x -> coerce $ fmap (x,) $ fb)

-- }}}

-- {{{ NOT REALLY SELECT

-- Contexts that only support static parallelism can't branch at all, so the only way they
-- could implement `select` would be by performing all three computations and discarding the
-- result of one.
newtype Force f a b = Force { getForce :: Fork f a b }
  deriving (Functor)

-- IMO this should not be a lawful implementation of `Select`
instance Apply f => Apply (Force f a)
  where
  zip (Force (Fork x), Force (Fork y)) = Force $ Fork $ liftA2 (liftA2 (,)) x y

-- }}}}
