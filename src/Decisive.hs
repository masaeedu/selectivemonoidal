module Decisive where

import Data.Void (Void)

-- Represents a context with static choice
class Functor f => Decide f
  where
  decide :: f (Either a b) -> Either (f a) (f b)

-- Choosing among no options
class Decide f => Decisive f
  where
  force :: f Void -> Void
