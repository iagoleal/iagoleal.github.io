---
title: Representing Functions that do not Forget
keywords: [haskell, functional-programming]
date: 2022-09-01
---

What is the most basic container type a language can have?
Some people may answer vectors, others would go with hash tables,
but in this post I am arguing in favor of _functions_.
Yes, functions.
Even though they aren't generally seem as a data structure per se,
we will see that most containers are in fact a way to represent a function
with a given storage layout.
To illustrate this "functions are containers" idea,
let's take a look at an application
that tightly couples both concepts: memoization.

_Memoization_ is a programming technique
where instead of letting a function calculate
the same value whenever it is called,
we instead store the already calculated value somewhere
and just do a lookup instead of redoing the entire calculation.

A large inspiration to this post comes from the great introduction to memoization in Haskell in the
[Haskell wiki](https://wiki.haskell.org/Memoization).

> {-# LANGUAGE DeriveFunctor,       TypeFamilies        #-}
> {-# LANGUAGE ScopedTypeVariables, RankNTypes          #-}
> {-# LANGUAGE TypeApplications,    AllowAmbiguousTypes #-}
> import Numeric.Natural

Sequences are functions that do not forget
------------------------------------------

An important fact that is normally briefly alluded
in any mathematics book and immediately forgotten by (almost) every reader
is that whenever you see a subindex such as $x_n$,
what is in fact being denoted is a function application $x(n)$.[^fortran]
Now consider the datatype of infinite streams
as in [the previous post](/posts/calculus-symbolic-ode):

> data Stream a = a :> Stream a
>   deriving Functor
>
> infixr 5 :>

[^fortran]: I must comment, perhaps to the abhorrence of my Haskell readers, that I really enjoy Fortran.
One of the reasons is that it denotes vector indexing using the same syntax as function application:
$x(n)$.

This type models sequences of type `a`,
the kind of thing a mathematician would denote as $\{x_n\}_{n \in \mathbb{N}}$.
Oh look, there's a subindex there!
Our previous discussion tells us that we should be able to interpret a `Stream a`
as a function `Natural -> a`.
This is done by indexing: we turn a stream `xs` into the function
that takes `n : Natural` to the nth element of `xs`.
The definition is recursive, as one might expect:

> -- Access the nth value stored in a Stream
> streamIndex :: Stream a -> (Natural -> a)
> streamIndex (x :> _)  0 = x
> streamIndex (_ :> xs) n = streamIndex xs (n-1)

Conversely, given any `f : Natural -> a`,
we can form a stream by applying $f$ to each natural number
to form something like `[f 0, f 1, f 2, f 3,..., f n,...]`.
Since `Stream` is a functor,
we achieve this by mapping $f$ into the stream of natural numbers:

> -- Take f to [f 0, f 1, f 2, f 3, ...]
> streamTabulate :: (Natural -> a) -> Stream a
> streamTabulate f = fmap f naturals where
>   naturals = 0 :> fmap (+1) naturals

These function are inverse to one another:

    streamIndex    . streamTabulate = id
    streamTabulate . streamIndex    = id

Meaning that we thus have a natural isomorphism `Stream a ≃ Natural -> a`.
This is a strong assertion and means that, mathematically speaking,
Streams and functions from the Naturals
are essentially the same the same thing.
We are doing Haskell in here, however, not pure math.
And in a programming language meant to run in a real computer,
not only in the realm of ideas, we also must take into account something more:
how are those types laid out into memory?

In the case of functions, they are compiled to chunks of instructions
that calculate some value.
Specially, if you have some rather expensive function `f : Natural -> a`
and have to calculate `f n` in many different parts of your program for the same `n`,
all work will have to be redone each time to get that sweet value of type `a`.[^sharing]
Streams, on the other hand, are lazy infinite lists
and, because of Haskell's
[call-by-need evaluation strategy](https://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_need),
its components remain saved into memory for reuse.[^gc]

[^sharing]: In fact, not all work must be necessarily redone.
If we are calculating `f n` twice in the same context,
the compiler may be smart enough to do some sharing and only calculate it once.
Of course, this works in Haskell thanks to the magic of referential transparency.

[^gc]: Well, at least until the next gc pass if it is not in the top-level.

This last paragraph is the heart of memoization in Haskell:
one does not memoize functions,
one turns functions into data and the language automatically memoizes the data.
I don't know about you, but I find this very cool indeed.



Memoizing with recursion
------------------------

Thus, we will follow them and use the Fibonacci sequence
as a recurring example thorughout this post.
The Fibonacci numbers are a well-known example where
memoization can make a function go from exponential to polynomial complexity.
Here is its usual recursive definition:

> fibRec :: Num a => Natural -> a
> fibRec 0 = 0
> fibRec 1 = 1
> fibRec n = fibRec (n-1) + fibRec (n-2)

Although elegant, this definition is _extremely slow_.
Running `fibRec 100` on ghci already took much longer
than I was disposed to wait...

```dot
digraph "Sequence as function" {
  rankdir=LR;
  size="8,5"

  node [shape     = rectangle
        style     = "solid,filled"
        color     = black
        fixedsize = shape
        fillcolor = invis];

  subgraph cluster_ldots {
    rank = same;
    style = invis;
    ldots [fontsize=20 color = "#00000000" fillcolor= "#00000000" label = ". . ."];
  }

  0 [label = "f 0"];
  1 [label = "f 1"];
  2 [label = "f 2"];
  3 [label = "f 3"];

  0 -> 1 -> 2 -> 3 -> ldots;
}
```

> fibStep :: Num a => (Natural -> a) -> (Natural -> a)
> fibStep f 0 = 0
> fibStep f 1 = 1
> fibStep f n = f (n-1) + f (n-2)

> fix f = let x = f x in x

> fibNaive = fix fibStep  -- same as fibRec


A Call for Representation
-------------------------

Very well, What is a function `k -> a` after all?
The textbook definition says it is a rule
that for each element of type `k` associates an unique element of type `a`.
The previous examples have shown us
that there are data structures
which, in the sense above, behave a lot like functions.
For example, consider `xs : Stream a`:
for each natural number `n`,
it associates a unique element `indexStream xs n` of type `a`.
Conversely, given a function `f : Natural -> a`,
we can create a `Stream a` storing all of `f`'s applications.
Thus, in a sense,

    Stream a ≃ Natural -> a

> class Functor f => Representable f where
>  type Key f :: *
>  tabulate   :: ((Key f) -> a) -> f a
>  index      :: f a -> ((Key f) -> a)

> memoize :: forall f a. Representable f => ((Key f -> a) -> (Key f -> a)) -> (Key f -> a)
> memoize g = (index @f . fix) (tabulate . g . index)

> instance Representable ((->) k) where
>  type Key ((->) k) = k
>  index    = id
>  tabulate = id

> instance Representable Stream where
>  type Key Stream = Natural
>  index    = streamIndex
>  tabulate = streamTabulate

> fibSmart :: Num a => Natural -> a
> fibSmart = memoize @Stream fibStep

> fibNaive' :: Num a => Natural -> a
> fibNaive' = memoize @((->) Natural) fibStep


Speeding up the Fibonacci even more
-----------------------------------

> fibTree :: Num a => Natural -> a
> fibTree = memoize @Tree fibStep


> data Tree a = Node a (Tree a) (Tree a)
>   deriving Functor

> data Eveness = Even | Odd

> evenness :: Integral a => a -> Eveness
> evenness n = if odd n then Odd else Even

> instance Representable Tree where
>  type Key Tree = Natural
>  tabulate f = fmap f nats where
>    nats = Node 0
>                (fmap (\ n -> 2*n + 1) nats)
>                (fmap (\ n -> 2*(n+1)) nats)
>
>  index (Node a _ _) 0 = a
>  index (Node _ l r) n = case evenness n of
>     Odd  -> index l (div n 2)
>     Even -> index r (div n 2 - 1)
