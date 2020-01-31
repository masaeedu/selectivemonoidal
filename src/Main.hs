{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude hiding (Applicative(..), zip)
import qualified Prelude as P (Applicative(..))
import Data.Void (Void)
import Data.Bool (bool)
import Data.Functor.Const (Const(..))

-- {{{ CLASSES

-- {{{ DECISIVE

-- Represents a context with static choice
class Functor f => Decide f
  where
  decide :: f (Either a b) -> Either (f a) (f b)

-- Choosing among no options
class Decide f => Decisive f
  where
  force :: f Void -> Void

-- }}}

-- {{{ APPLICATIVE

-- Represents a context with static parallelism
class Functor f => Apply f
  where
  zip :: (f a, f b) -> f (a, b)

-- Doing all of no tasks
class Apply f => Applicative f
  where
  husk :: () -> f ()

(<*>) :: Apply f => f (a -> b) -> f a -> f b
(<*>) fab fa = fmap (uncurry ($)) $ zip (fab, fa)

infixl 4 <*>

(*>) :: Apply f => f a -> f b -> f b
(*>) fa fb = flip const <$> fa <*> fb

liftA2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
liftA2 abc fa fb = abc <$> fa <*> fb

pure :: Applicative f => a -> f a
pure a = a <$ husk ()

instance Semigroup m => Apply (Const m)
  where
  zip (Const x, Const y) = Const (x <> y)

instance Monoid m => Applicative (Const m)
  where
  husk _ = Const mempty

-- }}}

-- {{{ SELECTIVE

-- Represents a context with choice (whether static or dynamic)
class Functor f => Select f
  where
  branch :: f (Either a b) -> f (a -> c) -> f (b -> c) -> f c

type Selective f = (Select f, Applicative f)

-- Notice how we can't implement `branch` in terms of only `Apply`!
-- A notion of choice demands more than `Apply` in one way or another...

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

-- }}}

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

-- }}}


-- {{{ EXAMPLES

-- {{{ OVER

newtype Over m a = Over { getOver :: m }
  deriving Show
  deriving (Functor, Apply, Applicative) via Const m

instance Decide (Over m)
  where
  decide (Over m) = Left (Over m)

instance Semigroup m => Select (Over m)
  where
  branch b x y = getStatic $ branch (Static b) (Static x) (Static y)

testOver :: IO ()
testOver = do
  print "Testing Over examples from page 8"
  -- example from paper
  print $ ifS (Over "a") (Over "b") (Over "c") *> Over "d" *> whenS (Over "e") (Over "f")

-- }}}

-- {{{ UNDER

newtype Under m a = Under { getUnder :: m }
  deriving Show
  deriving (Functor, Apply, Applicative) via Const m

instance Decide (Under m)
  where
  decide (Under m) = Right (Under m)

instance Semigroup m => Select (Under m)
  where
  branch b x y = getStatic $ branch (Static b) (Static x) (Static y)

testUnder :: IO ()
testUnder = do
  print "Testing Under examples from page 8"
  -- example from paper
  print $ ifS (Under "a") (Under "b") (Under "c") *> Under "d" *> whenS (Under "e") (Under "f")

-- }}}

-- {{{ VALIDATION

data Validation e a = Failure e | Success a
  deriving (Show, Functor)

instance Semigroup e => Apply (Validation e)
  where
  zip (Failure e, Failure f) = Failure (e <> f)
  zip (Failure e, Success _) = Failure e
  zip (Success _, Failure f) = Failure f
  zip (Success a, Success b ) = Success (a, b)

instance Semigroup e => Applicative (Validation e)
  where
  husk _ = Success ()

instance Semigroup e => Decide (Validation e)
  where
  decide (Failure e) = Right (Failure e)
  decide (Success (Left a)) = Left (Success a)
  decide (Success (Right b)) = Right (Success b)

instance Semigroup e => Select (Validation e)
  where
  branch b x y = getStatic $ branch (Static b) (Static x) (Static y)

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
  shape :: (Apply f, Select f) => f Bool -> f Radius -> f Width -> f Height -> f Shape
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

-- }}}

-- }}}

main :: IO ()
main = do
  testOver
  testUnder
  testValidation
  testUnderVsValidation
