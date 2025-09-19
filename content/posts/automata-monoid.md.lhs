---
title: Monoids Let You Run Automata in Parallel
keywords: [haskell, automata]
date: 2025-05-18
description:
---

\def\A{\mathcal{A}}

If you read this blog, you probably know how often I post about finite automata (FA).
They are in an expressivity sweet stop
where they're simple enough to run fast while still solving a lot of real problems.
Just look around and notice how often you see regex engines all over the programming world.

My only grapple with finite automata is how inherently sequential
their execution model is.
To recognize a string, an FA consumes it one character after another
while extracting the relevant information from it and storing in an internal state.
This execution is fair and square but, unfortunately,
not very amenable to today's age of multiple cores and GPUs.

In a [previous post](/posts/automata-tensor-networks/),
I explored how to use tensor networks to turn a finite automaton into a quantum system.
As a consequence, language recognition became a bunch of small matrix multiplications and tensor contractions,
operations that are _very amenable_ to parallelization.
Today let's investigate this same idea but focusing on the parallelization instead of the quantum stuff.
We will also switch from tensors to monoids,
making it well-suited to a Haskell implementation.

> import Data.Foldable
> import Data.Monoid
> import Control.Monad
> import Control.Parallel
> import Data.Map (Map)
> import qualified Data.Map.Strict as Map

Also, we will use a lot of "finite types".
So, let's be precise about what we mean by it.
For this post, it means that we can use it as an index as well as order and enumerate all its elements.
We achieve this via some standard typeclasses.

> type Finite s = (Ix s, Bounded s, Enum s)

And their main operation is conjuring an ordered list of all their elements.

> elems :: Finite s => [s]
> elems = [minBound..maxBound]
>
> bounds :: Finite s => (s, s)
> bounds = (minBound, maxBound)


Monoid Machines
===============

Kleene's Theorem states a bunch of equivalent ways to define a regular language:
DFAs, NFAs, regular expressions, regular grammars etc.
Recently, while reading a paper by @pin_finite_1995,
I discovered yet another characterization.

:::{.Theorem data-title="Kleene for Monoids"}
A language is regular if and only if
there is a finite monoid recognizing it.
:::

Of course, for this to make sense,
we need to define what it means for a monoid to recognize a language.
Also from that same paper we have that.

:::Definition
A monoid $M$ recognizes a language $L$ over alphabet $\A$
if there is a monoid homomorphism $\phi \colon A^\star \to M$
and a subset $F \subset M$ such that
$$ w \in L \iff \phi(w) \in P.$$
:::

Let's unpack this definition a bit before proceeding to why it is useful.
The function $\phi$ turns words over the alphabet into elements of a monoid.
Think about the previous discussion of how an FA takes an initial string into a state,
it is somewhat similar.
The homomorphism requirement means that it does not matter how we form these words,
only the characters constituting it.
In fact, since $\A^\star$ is the free monoid over $\A$,
there is a unique mapping $g \colon \A \to M$
such that $\phi = \mathrm{fmap}\ g$.
We can view this $g$ as the monoid generators.

Now its time for some code!
From the previous discussion, we define a _Monoid Machine_ over an alphabet
as a choice of generators and an accepting subset,
represented as a predicate:

> data MonoidMachine m a = MonoidMachine
>   { generators :: a -> m
>   , accepting  :: m -> Bool
>   }

Similarly to automata,
one can use a monoid machine to check whether a string
is part of its target language.
All it takes is to apply the monoid homomorphism,
fold over it and check whether the final result is acceptable.

> accept :: Monoid x => MonoidMachine x a -> [a] -> Bool
> accept m = accepting m . foldMap' (generators m)

The main advantage of this approach is that our `foldMap` is not constrained
to read the input from start to finish.
We can, thus, do it in parallel chunks,
yield an algorithm similar to the one by @matos_monoid_2006.

> acceptPar :: Monoid x => Int -> MonoidMachine x a -> [a] -> Bool
> acceptPar chunkSize m = accepting m . foldMapPar chunkSize (generators m)

Also keep in mind that how many chunks you should use
depends on how complicated your monoid is.
As everything related to parallelism, it is somewhat of an art.

Example: Binary Division by 3
-----------------------------

> data Z3 = N0 | N1 | N2

> instance Semigroup Z3 where
>  N0 <> x  = x
>  x  <> N0 = x
>  N1 <> N1 = N2
>  N1 <> N2 = N0
>  N2 <> x  = x <> N2

> instance Monoid Z3 where
>  mempty = N0



From Monoids to Automata and Back Again
=======================================

Consider a deterministic finite automaton. or DFA for short.

> data DFA s a = DFA s (a -> s -> s) (s -> Bool)

We know that they have the same computational power as finite monoids
but, at least for me, it wasn't obvious at first sight.
The idea of the proof is to provide conversion rules
between these machines preserving the recognized languages.

To go from Monoids to DFA,
we construct a machine that does the monoid transitions
sequentially from the first to the last character.
Its states are the monoid's underlying set
and the transition consist of contracting the current state with the new generator read.
We can keep the same subset as accepting.

> monToDFA :: Monoid m => MonoidMachine m a -> DFA m a
> monToDFA mm = DFA mempty act (accepting mm)
>  where act a m = m <> generators mm a

Notice that the DFA is finite as long as the monoid `m` is finite.

For the other direction,
we use DFA's transition monoid --- a trick somewaht similar to difference lists.
The transition can be viewed as a transformation `a -> (s -> s)`
where `s -> s` has a natural monoid structure from composition.
Thankfully all this lifting already comes bundled with Haskell in the `Endo` type.
Finally, to check if an endomorphism is accepted,
we test whether it takes the initial states to a final one.

> dfaToMon :: DFA s a -> MonoidMachine (Endo s) a
> dfaToMon (DFA q0 next final) = MonoidMachine (Endo . next) accepting
>  where accepting (Endo f) = final (f q0)

Again, the endomorphism type `s -> s` is finite if and only if `s` is itself finite.

Although these functions are not inverses,
you can check that they preserve the recognized language, which is enough for us.


Maps, Matrices, and Parallelism
===============================

For a regular language specified as a DFA, `dfaToMon`
supposedly lets us recognize it in parallel using its monoid of endomorphisms.

> asEndo :: DFA s a -> Bool
> asEndo dfa = recognize (dfaToMon dfa)

Now go on and run it in an example. I'll wait.
And to be fair, I'll a lot, because the method above is pretty slow.
The problem is that the monoid multiplication is too slow.
All it is doing is composing a thunk of functions that will only actually be executed
when checking the final state.
We need a more strict representation --- something looking more like data than code.

The description of `Data.Map.Map k v` in the [containers](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Map-Strict.html)
package is as a finite partial map from `k` to `v`.
This means we can use a `Map` instead of an `Endo` to represent the same monoid!
Let's write it as a new type as well as defining how to memoize a finite function with it.

> newtype FinEndo s = FinEndo (Map.Map s s)
>
> finendo :: Finite s => (s -> s) -> FinEndo s
> finendo f = FinEndo $ Map.fromList [(s, f s) | s <- elems]

For the monoid instance,
everything is basically premade.
All it takes is to assemble the blocks.

> instance Finite s => Semigroup (FinEndo s) where
>  (FinEndo f) <> (FinEndo g) = FinEndo (Map.compose f g)
>
> instance Finite s => Monoid (FinEndo s) where
>  mempty = finendo id

Now we can turn a DFA into a monoid
for which all compositions are calculated instead of thunked.

> asMap :: Finite s => DFA s a -> MonoidMachine (FinEndo s) a
> asMap (DFA q0 t final) = MonoidMachine gen check
>   where
>    gen a = finendo (t a)
>    check (FinEndo m) = final (m Map.! q0)

What about Matrices?
--------------------

The `Map` construction works well
but, to be fair, I could not go home without building the most classical monoid
associated with an automaton: its transition matrices!
The idea is to look at a finite function as a graph (or a relation)
and use its adjacency matrices

$$ T^a_{s s'} = \begin{cases} 1,& t(a, s) = s' \\ 0,& \text{otherwise}. \end{cases}$$

Magically (or not), function composition becomes matrix multiplication in this setting.


It works for NFAs too!
======================
