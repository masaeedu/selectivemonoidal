{-# LANGUAGE ImpredicativeTypes, TupleSections #-}
module Selective where

import Prelude hiding (Applicative(..), zip)

import Data.Bool (bool)
import Data.Function ((&))
import Data.Bifunctor.Biff (Biff(..))

import Alternative (Alt(..), (<|>))
import Applicative (Apply(..), Applicative(..), pure, (<*>), liftA2)
import Decisive (Decide(..))

import Data.Coerce (coerce)

type (+) = Either
type (×) = (,)

newtype Outside f a b = Outside { runOutside :: f (a + b) }
  deriving Functor

type Select f = forall a. (Functor f, Apply (Outside f a))
type Selective f = (Applicative f, Select f)

-- {{{ COMBINATORS
swapE :: a + b -> b + a
swapE = either Right Left

swapF :: Functor f => Outside f a b -> Outside f b a
swapF (Outside fab) = Outside $ swapE <$> fab

lift :: Functor f => f a -> Outside f x a
lift = Outside . fmap Right

mergeE :: a + a -> a
mergeE = either id id

merge :: Functor f => Outside f a a -> f a
merge = fmap mergeE . runOutside

branch :: Select f => f (a + b) -> f (a -> c) -> f (b -> c) -> f c
branch feab fac fbc =
  let
    fecb = (&) <$> swapF (Outside feab) <*> lift fac
    fecc = (&) <$> swapF fecb <*> lift fbc
  in
  merge fecc

selectR :: Selective f => f (a + b) -> f (b -> a) -> f a
selectR v = branch v $ pure id

selectL :: Selective f => f (a + b) -> f (a -> b) -> f b
selectL v = flip (branch v) $ pure id

(<*?) :: Selective f => f (a + b) -> f (a -> b) -> f b
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
newtype Static f a b = Static { getStatic :: Outside f a b }
  deriving Functor

type Inside = Biff

instance (Alt f, Apply g) => Apply (Inside (+) f g a)
  where
  zip (Biff x, Biff y) = Biff $ go x y
    where
    go (Left  x) (Right _) = Left $ x
    go (Right _) (Left  y) = Left $ y
    go (Left  x) (Left  y) = Left $ x <|> y
    go (Right x) (Right y) = Right $ zip (x, y)

exit :: Functor f => Inside (+) f f a b -> Outside f a b
exit (Biff (Left  fa)) = Outside $ Left <$> fa
exit (Biff (Right fb)) = Outside $ Right <$> fb

instance (Decide f, Alt f, Apply f) => Apply (Static f a)
  where
  zip (Static (Outside fa), Static (Outside fb)) = Static $ exit $ zip (Biff $ decide fa, Biff $ decide fb)

-- }}}

-- {{{ DYNAMIC SELECT

-- Contexts that only support dynamic choice can also be used for branching computations.
-- However, these computations don't support static analysis.
newtype Dynamic f a = Dynamic { getDynamic :: f a }
  deriving Functor

instance Monad f => Apply (Outside (Dynamic f) a)
  where
  zip (Outside (Dynamic fa), fb)= Outside $ Dynamic $ fa >>= either (return . Left) (\x -> coerce $ fmap (x,) $ fb)

-- }}}

-- {{{ NOT REALLY SELECT

-- Contexts that only support static parallelism can't branch at all, so the only way they
-- could implement `select` would be by performing all three computations and discarding the
-- result of one.
newtype Force f a b = Force { getForce :: Outside f a b }
  deriving (Functor)

-- IMO this should not be a lawful implementation of `Select`
instance Apply f => Apply (Force f a)
  where
  zip (Force (Outside x), Force (Outside y)) = Force $ Outside $ liftA2 (liftA2 (,)) x y

-- }}}}
