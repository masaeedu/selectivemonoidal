# selectivemonoidal

## Overview
Selective functors are a new abstraction first outlined by Andrey Mokhov [here](https://blogs.ncl.ac.uk/andreymokhov/selective/) and later fleshed out in a paper [here](https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf).

The code in this repository (see `Main.hs`) attempts to demonstrate that the proposition

> The functor `F` is a selective functor

can be teased apart into two separate propositions:

> 1. `F` is an applicative functor
> 2. `F` is a decisive functor

While it may appear that these "decisive" functors are a new abstraction, they actually arise from the same phenomenon as applicative functors: they are both particular kinds of lax monoidal functors. Conor McBride (who after all introduced us to the familiar `Applicative` family of lax monoidal functors) offhandedly invented `Decisive` functors in [this blogpost](https://fplab.bitbucket.io/posts/2007-07-08-decisive-functors.html) from 2007.

## Explanation

### Substituting `Selective`
The short version of the story is that if we define the following class:
```haskell
class Functor f => Decide f
  where
  decide :: f (Either a b) -> Either (f a) (f b)

class Decide f => Decisive f
  where
  guarantee :: f Void -> Void
```

then we can substitute the `Selective` typeclass with the following constraint synonym:

```haskell
type Selective f = (Applicative f, Decide f)
```

As a matter of fact, all we really need is an `Apply` class analogous to `Decide`, and not the full `Applicative` (i.e. we don't need `pure`). However this hasn't really percolated into the class hierarchy of the base library yet, so we demand the extra `pure` and call it a day.

Using this constraint we can define the `branch` minimal definition discussed in Section 6.1 of the selective functors paper:

```haskell
branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
```

Given that `select` can be defined in terms of `branch`, all the other combinators follow (see `Main.hs`).

## What is a lax monoidal functor?

A lax monoidal functor between arbitrary categories can be modeled by by this pseudo-Haskell:

```haskell
class (Category (→₁), Category (→₂)) => Functor (→₁) (→₂) f
  where
  map :: (a →₁ b) -> (f a →₂ f b)

data Iso (→) a b = Iso { fwd :: a → b, bwd :: b → a }

-- Not going to get into the horrorshow of trying to define Bifunctor using product
-- categories in Haskell, please indulge me while I pull Bifunctor out of thin air

class (Category (→), Bifunctor (→) (⊗)) => SemigroupalCategory (→) (⊗)
  where
  assoc :: Iso (→) (a ⊗ (b ⊗ c)) ((a ⊗ b) ⊗ c)

class (SemigroupalCategory (→) (⊗)) => MonoidalCategory (→) (⊗) i
  where
  lunit :: Iso (→) (i ⊗ a) a
  runit :: Iso (→) (a ⊗ i) a

class
  (SemigroupalCategory (→₁) (⊗₁), SemigroupalCategory (→₂) (⊗₂), Functor (→₁) (→₂) f)
  =>
  SemigroupalFunctor (→₁) (⊗₁) (→₂) (⊗₂) f
  where
  zip :: f a ⊗₂ f b →₂ f (a ⊗₁ b)

class
  (MonoidalCategory (→₁) (⊗₁) (i₁), MonoidalCategory (→₂) (⊗₂) (i₂), SemigroupalFunctor (→₁) (→₂) f)
  =>
  MonoidalFunctor (→₁) (⊗₁) (i₁) (→₂) (⊗₂) (i₂) f
  where
  pure :: i₂ →₂ f i₁
```

So that then:

```haskell
type Applicative = MonoidalFunctor (->) (,) () (->) (,) ()
type Decisive = MonoidalFunctor (Op (->)) Either Void (Op (->)) Either Void
```

TODO: Explain (op)lax monoidal functors
