module Main where

import Prelude hiding (Applicative(..), zip)

import Data.Functor.Const (Const(..))

import Decisive (Decide(..))
import Applicative (Apply(..), Applicative(..), liftA2, (<*>), (*>))
import Alternative (Alt(..))
import Selective (Select, Static(..), Fork(..), ifS, whenS)

-- {{{ EXAMPLES

-- {{{ OVER

newtype Over m a = Over { getOver :: m }
  deriving Show
  deriving (Functor, Apply, Applicative) via Const m

instance Decide (Over m)
  where
  decide (Over m) = Right (Over m)

instance Alt (Over m)
  where
  union (fa, _) = Left <$> fa

deriving via (Static (Over a) b) instance Monoid a => Apply (Fork (Over a) b)

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

instance Alt (Under m)
  where
  union (fa, _) = Left <$> fa

instance Decide (Under m)
  where
  decide (Under m) = Left (Under m)

deriving via (Static (Under m) a) instance Monoid m => Apply (Fork (Under m) a)

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

instance Alt (Validation e)
  where
  union (fa, _) = Left <$> fa

instance Semigroup e => Applicative (Validation e)
  where
  husk _ = Success ()

instance Semigroup e => Decide (Validation e)
  where
  decide (Failure e)         = Left (Failure e)
  decide (Success (Left a))  = Left (Success a)
  decide (Success (Right b)) = Right (Success b)

deriving via (Static (Validation e) a) instance Semigroup e => Apply (Fork (Validation e) a)

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
