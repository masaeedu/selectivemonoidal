{-# LANGUAGE RankNTypes, UndecidableInstances, DeriveFunctor, DerivingVia #-}

module Main where

import Data.Void (Void)
import Data.Bool (bool)
import Control.Applicative (liftA2)
import Data.Functor.Const (Const(..))
import Data.Function ((&))

-- Patterns

-- A decisive functor is an oplax monoidal functor from Hask under Either to Hask under Either
-- Equivalently, it's the opposite of a lax monoidal functor from Hask^op under Either to Hask^op under Either

class Functor f => Decide f
  where
  decide :: f (Either a b) -> Either (f a) (f b)

class Decide f => Decisive f
  where
  guarantee :: f Void -> Void

-- Basically a decisive functor is just an applicative functor in "backwards Haskell". So e.g. just as @Applicative@ is a superclass of @Monad@, @Decisive@ is a superclass of @Comonad@.
-- See https://fplab.bitbucket.io/posts/2007-07-08-decisive-functors.html

-- Selective doesn't really need to be a class anymore
type Selective f = (Applicative f, Decide f)

-- Convenience function for <*> with the wrapped function as the second effect
(</>) :: Applicative f => f a -> f (a -> b) -> f b
(</>) = liftA2 (&)

branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch = choose . decide
  where
  choose (Left fa)  l _ = fa </> l
  choose (Right fb) _ r = fb </> r

-- Selective combinators
-- Note that based on this implementation a "skip" happens when the @f (Either a b)@ argument happens to be a @Right b@
select :: Selective f => f (Either a b) -> f (a -> b) -> f b
select feab fab = branch feab fab (pure id)

(<*?) :: Selective f => f (Either a b) -> f (a -> b) -> f b
(<*?) = select

infixl 4 <*?

whenS :: Selective f => f Bool -> f () -> f ()
whenS x y = bool (Right ()) (Left ()) <$> x <*? (const <$> y)

ifS :: Selective f => f Bool -> f a -> f a -> f a
ifS x t e = branch (bool (Right ()) (Left ()) <$> x) (const <$> t) (const <$> e)

-- Examples

newtype Over m a = Over { getOver :: m }
  deriving (Show)
  deriving (Functor, Applicative) via (Const m)

instance Decide (Over m)
  where
  decide (Over m) = Left (Over m)

testOver :: IO ()
testOver = do
  print "Testing Over examples from page 8"
  -- example from paper
  print $ ifS (Over "a") (Over "b") (Over "c") *> Over "d" *> whenS (Over "e") (Over "f")

instance Decide (Under m)
  where
  decide (Under m) = Right (Under m)

testUnder :: IO ()
testUnder = do
  print "Testing Under examples from page 8"
  -- example from paper
  print $ ifS (Under "a") (Under "b") (Under "c") *> Under "d" *> whenS (Under "e") (Under "f")

newtype Under m a = Under { getUnder :: m }
  deriving (Show)
  deriving (Functor, Applicative) via Const m

data Validation e a = Failure e | Success a
  deriving (Show, Functor)

instance Semigroup e => Applicative (Validation e)
  where
  pure = Success
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e1 <*> Success _  = Failure e1
  Success _  <*> Failure e2 = Failure e2
  Success f  <*> Success a  = Success (f a)

instance Semigroup e => Decide (Validation e)
  where
  decide (Failure e) = Right (Failure e)
  decide (Success (Left a)) = Left (Success a)
  decide (Success (Right b)) = Right (Success b)

type Radius = Int
type Width = Int
type Height = Int

data Shape = Circle Radius | Rectangle Width Height
  deriving Show

testValidation :: IO ()
testValidation = do
  print "Testing Validation examples from page 9"

  -- example 1 from paper
  print $ shape (Success True)        (Success 1)           (Failure ["width?"])  (Failure ["height?"])
  print $ shape (Success False)       (Failure ["radius?"]) (Success 2)           (Success 3)
  print $ shape (Success False)       (Success 1)           (Failure ["width?"])  (Failure ["height?"])
  print $ shape (Failure ["choice?"]) (Failure ["radius?"]) (Success 2)           (Failure ["height?"])

  -- example 2 from paper
  let s1 = shape (Failure ["choice 1?"]) (Success 1) (Failure ["width 1?"]) (Success 3)
  let s2 = shape (Success False) (Success 1) (Success 2) (Failure ["height 2?"])
  print $ twoShapes s1 s2

  where
  shape :: Selective f => f Bool -> f Radius -> f Width -> f Height -> f Shape
  shape x r w h = ifS x (Circle <$> r) (Rectangle <$> w <*> h)

  twoShapes :: Applicative f => f Shape -> f Shape -> f (Shape, Shape)
  twoShapes = liftA2 (,)

testUnderVsValidation :: IO ()
testUnderVsValidation = do
  print $ "Testing Under vs Validation comparison examples from page 12"

  -- examples from paper
  print $ whenS (Under "a" *> Under "b") (Under "c")
  print $ Under "a" *> whenS (Under "b") (Under "c")
  print $ Failure "a" *> whenS (Success True) (Failure "b")
  print $ whenS (Failure "a" *> Success True) (Failure "b")

main :: IO ()
main = do
  testOver
  testUnder
  testValidation
  testUnderVsValidation
