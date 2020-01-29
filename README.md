# selectivemonoidal

## Overview
Selective functors are a new abstraction first outlined by Andrey Mokhov [here](https://blogs.ncl.ac.uk/andreymokhov/selective/) and later fleshed out in a paper [here](https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf).

The code in this repository (see `Main.hs`) attempts to demonstrate that the proposition

> The functor `F` is a selective functor

can be teased apart into two separate propositions:

> 1. `F` is an applicative functor
> 2. `F` is a decisive functor

While it may appear that these "decisive" functors are a new abstraction, they are actually arise from the same phenomenon as applicative functors: they are both particular kinds of lax monoidal functors. So for example Conor McBride (who after all introduced us to the familiar `Applicative` family of lax monoidal functors) offhandedly invented `Decisive` functors in [this blogpost](https://fplab.bitbucket.io/posts/2007-07-08-decisive-functors.html) from 2007.

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

## Why are you reinventing all these abstractions?
I consider this repo to be part of a program of demonstrating that many abstractions we are interested in for practical Haskell programming (applicatives, alternatives, filterables, alignables, selectives, monad transformers, ???), can be modeled quite nicely as a single concept: monoidal functors between various monoidal categories.

I hope this will increase awareness of monoidal categories and monoidal functors as useful concepts among Haskell programmers, so that people working on different problems can "see" a monoidal functor when it's there, similarly to how the now ubiquitous abstraction of a monad is easily "seen" in problem domains by Haskell programmers.

## Ok, but what is a monoidal functor
TODO: Write a blog post to explain
