---
title: Parallel Parsing of Regular Languages with Monoids
keywords: [haskell, automata]
date: 2025-05-18
description:
---

\def\A{\mathcal{A}}

If you read this blog, you probably know how often I post about finite automata (FA).
They are in a expressivity sweet stop
where they're simple enough to run fast while still solving a lot of real problems.
Just look around and notice how often you see regex engines all over the programming world.

My only grapple with finite automata is how inherently sequential
their execution model is.
To recognize a string, a FA consumes it one character after another
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
Think about the previous discussion of how a FA takes an initial string into a state,
it is somewhat similar.
The homomorphism requirement means that it does not matter how we form these words,
only the characters constituting it.
In fact, since $\A^\star$ is the free monoid over $\A$,
there is a unique mapping $g \colon \A \to M$
such that $\phi = \mathrm{fmap}\ g$.
We can view this $g$ as the monoid generators.

Now its time for some code!
From the previous discussion, we define a _Monoid Machine_ over an alphabet
as a choice of generators and a accepting subset,
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
> accept m = accepting m . foldMap (generators m)

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
>   where act a m = m <> generators mm a

Notice that the DFA is finite as long as the monoid `m` is finite.

For the other direction,
we use DFA's transition monoid --- a trick somewaht similar to difference lists.
The transition can be viewed as a transformation `a -> (s -> s)`
where `s -> s` has a natural monoid structure from composition.
Thankfully all this lifting alreadys comes bundled with Haskell in the `Endo` type.
Finally, to check if an endomorphism is accepted,
we test whether it takes the initial states to a final one.

> dfaToMon :: DFA s a -> MonoidMachine (Endo s) a
> dfaToMon (DFA q0 next final) = MonoidMachine (Endo . next) accepting
>  where accepting (Endo f) = final (f q0)

Again, the endomorphism type `s -> s` is finite if and only if `s` is itself finite.

Altough these functions are not inverses,
you can check that they preserve the recognized language, which is enough for us.


Maps, Matrices, and Parallelism
===============================






~~~
  @article{bojanczyk_algorithms_2012,
    title    = {Algorithms for regular languages that use algebra},
    volume   = {41},
    issn     = {0163-5808},
    url      = {https://dl.acm.org/doi/10.1145/2350036.2350038},
    doi      = {10.1145/2350036.2350038},
    abstract = {This paper argues that an algebraic approach to regular languages, such as using monoids, can yield efﬁcient algorithms on strings and trees.},
    language = {en},
    number   = {2},
    urldate  = {2025-05-08},
    journal  = {ACM SIGMOD Record},
    author   = {Bojańczyk, Mikołaj},
    month    = aug,
    year     = {2012},
    keywords = {read},
    pages    = {5--14},
  }


  @incollection{pin_finite_1995,
    address    = {Dordrecht},
    title      = {Finite {Semigroups} and {Recognizable} {Languages}: {An} {Introduction}},
    isbn       = {978-94-010-4067-9 978-94-011-0149-3},
    shorttitle = {Finite {Semigroups} and {Recognizable} {Languages}},
    url        = {http://link.springer.com/10.1007/978-94-011-0149-3_1},
    language   = {en},
    urldate    = {2025-05-06},
    booktitle  = {Semigroups, {Formal} {Languages} and {Groups}},
    publisher  = {Springer Netherlands},
    author     = {Pin, Jean-Eric},
    editor     = {Fountain, John},
    year       = {1995},
    doi        = {10.1007/978-94-011-0149-3_1},
    keywords   = {read-stack},
    pages      = {1--32},
  }

@article{matos_monoid_nodate,
  title    = {Monoid machines: a {O}(log n) parser for regular languages},
  abstract = {A new method for parsing regular languages is presented; for each regular language L a monoid (S, ·) is deﬁned; in particular, every letter a of the alphabet is mapped into an element f (a) of S (in general S contains elements that are not images of letters). Parsing a word x = ab · · · c consists essentially in (i) ﬁnding a transition monoid associated with the language (O(1) time) and (ii) compute the monoid product x′ = f (a) · f (b) · · · f (c). We show that this method can be applied to every regular language but not (if only ﬁnite monoids are allowed) to the class of context-free languages. Consider a ﬁnite automaton A recognizing L; the elements of the monoid SL, which include the set \{f (a) : a ∈ Σ\}, correspond to functions from the set of states of A into the set of states of A. In general the cardinality of the monoid is exponential on the number of states of the minimum deterministic automaton recognizing the language.},
  language = {en},
  url      = {https://www.dcc.fc.up.pt/~acm/semigr.pdf},
  author   = {Matos, Armando B},
}
~~~
