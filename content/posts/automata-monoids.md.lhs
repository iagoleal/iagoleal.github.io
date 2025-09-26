---
title: Monoids Let You Run Automata in Parallel
keywords: [haskell, automata]
date: 2025-09-24
suppress-bibliography: true
description:
  Finite automata are awesome but inherently sequential.
  On the other hand, finite monoid machines solve the same problems
  while being amenable to parallel execution.
---

```{=tex}
\usetikzlibrary{graphs,shapes.geometric}
\usetikzlibrary{fit}

\pgfdeclarelayer{background}
\pgfsetlayers{background,main}
```

\def\A{\mathcal{A}}

If you read this blog, you probably noticed how often I post about finite automata (FA).
They are put in this expressivity sweet stop
of being simple enough to run fast while still modeling well a lot of real problems.
Just look around and notice how often you see regex engines
or control systems assumed to be Markovian.

My only grapple with finite automata is how inherently sequential
their execution model is.
To recognize a string, an FA consumes it one character after another
while extracting the relevant information from it and storing in an internal state.
This execution is fair and square but, unfortunately,
not very amenable to today's age of multiple cores and GPUs.[^figure]

[^figure]: The idea of DFAs turning prefixes into states
while monoids turn chunks into states comes from @bojanczyk_algorithms_2012.

```tikz {tikzlibrary="fit,chains,bending,shapes.misc"}
{ [start chain=ctrl going right, node distance=5mm]

  { [minimum size=0.5cm,
     tmcell/.style={fill,draw=black, rounded corners=1.618},
     every join/.style={{Latex[length=1mm]}-}]

      \def\colors{red!30!blue!50, red!50, green!30, blue!50, blue!50, red!50, green!30, red!30!blue!50, blue!20}

    \foreach [count=\i] \c in \colors {
      \ifnum \i>4
        \node [tmcell,fill=\c, on chain, join] (n\i) {};
      \else
        \node [tmcell,fill=\c, on chain] (n\i) {};
      \fi
    }
  }

\pgfonlayer{background}
  \node [fill=orange!30, thick, draw=orange!50, fit=(n1) (n2) (n3), "state" below] (state) {};
  \node [fit = (n4), "next" below] {};
  \node [opacity=0.4, draw, fit= (n5) (n9), "ignored for now" below] {};
\endpgfonlayer
}
```

For parallelism to work, we need an equivalent machine
that can independently calculate a state for arbitrary string chunks
and then piece them together into a coherent global state.

```tikz {tikzlibrary="fit,chains,bending,shapes.misc"}
{ [ sblock/.style = { fill=orange!20, thick, draw=orange!30}
  , tmcell/.style={fill,draw=black, rounded corners=1.618, minimum size = 0.5cm},
  ]
  \def\colors{red!30!blue!50, red!50, green!30, blue!50, blue!50, red!50, green!30, red!30!blue!50, blue!20}

    \foreach \level in {1,2,3} {
      \foreach [count=\i] \c in \colors {
        \node [tmcell,fill=\c] (word-\level-\i) at ($(\i cm, 0) -{2*(\level - 1)}*(0, 1cm)$) {};
        \ifnum \level=2
          \ifnum \i=4
            \breakforeach
          \fi
        \fi
      }
    }

  \pgfonlayer{background}
    \node [sblock, fit=(word-1-1) (word-1-3)] (s11) {};
    \node [sblock, fit= (word-1-4)] (s12)    {};
    \node [sblock, fit= (word-1-5) (word-1-9)] (s13) {};

    \node [sblock, fit= (word-2-1) (word-2-4)] (s2) {};
    \node [sblock, fit= (word-3-1) (word-3-9)] (s3) {};
  \endpgfonlayer

  % Edges connecting states
  \draw[-Latex] (s11.south) -- (s2.north);
  \draw[-Latex] (s12.south) -- (s2.north);
  \draw[-Latex] (s2.south)  -- (s3.north);
  \draw[-Latex] (s13.south) -- (s3.north);

  % Legend
  \node[above right] at (s11.north west) {prefix state};
  \node[above right] at (s13.north west) {suffix state};
  \node[above right] at (s3.north west)  {complete state};
}
```

In a [previous post](/posts/automata-tensor-networks/),
we explored how to use tensor networks to turn a finite automaton into a quantum system.
As a consequence, language recognition became a bunch of small matrix multiplications and tensor contractions,
operations that are _very amenable_ to parallelization.
Today let's investigate this same idea but focusing on the parallelization instead of the quantum stuff.
We will also switch from tensors to monoids,
making it well-suited for a Haskell implementation.

> {-# LANGUAGE DataKinds,           KindSignatures      #-}
> {-# LANGUAGE PackageImports,      AllowAmbiguousTypes #-}
> import              Numeric.Natural   (Natural)
> import              GHC.TypeNats      (KnownNat, Nat, natVal)
> import              Data.Proxy        (Proxy(Proxy))
> import              Data.Foldable     (foldMap')
> import              Data.Monoid       (Endo (..))
> import "parallel"   Control.Parallel  (par, pseq)
> import "array"      Data.Array        (Ix, Array)
> import "array"      Data.Array.IArray qualified as A
> import "containers" Data.Map.Strict   qualified as Map

Also, we will use a lot of "finite types".
So, let's be precise about what we mean by it.
For this post, it means that we can order and enumerate all its elements.
as well as use it as an index.
This amounts to some standard typeclasses.

> type Finite s = (Ix s, Bounded s, Enum s)
>
> elems :: Finite s => [s]
> elems = [minBound..maxBound]


Monoid Machines
===============

There are many different ways to define a formal languages:
subsets of strings, deterministic and nondeterministic automata, formal grammars etc.
Recently, while reading a paper by @pin_finite_1995,
I discovered yet another characterization with more of an algebraic flavour:
being recognized by a monoid.

:::Definition
A monoid $M$ recognizes a language $L$ over alphabet $\A$
if there is a monoid homomorphism $\phi \colon A^\star \to M$
and a subset $F \subset M$ such that
$$ w \in L \iff \phi(w) \in F.$$
:::

Let's unpack this definition a bit before proceeding to why it is useful.
The function $\phi$ turns words over the alphabet into elements of a monoid.
Think about the previous discussion of how an FA takes an initial string into a state,
it is somewhat similar.
The homomorphism requirement means that it does not matter how we form these words,
only the characters constituting it.
In fact, since $\A^\star$ is the free monoid over $\A$,
there is a unique mapping $g \colon \A \to M$
such that $\phi = \mathrm{foldMap}\ g$.
We can view this $g$ as the monoid generators.

Now its time for some code!
From the previous discussion, we define a _Monoid Machine_ over an alphabet
as a choice of generators and an accepting subset,[^monoid-machine]
represented as a predicate:

> data MonoidMachine m a = MonoidMachine
>   { generators :: a -> m
>   , accepting  :: m -> Bool
>   }

[^monoid-machine]: This name comes from @matos_monoid_2006.

Similarly to automata,
one can use a monoid machine to check whether a string
is part of its target language.
All it takes is to apply the monoid homomorphism,
fold over it and check whether the final result is acceptable.

> recognize :: Monoid x => MonoidMachine x a -> [a] -> Bool
> recognize m = accepting m . foldMap' (generators m)

The main advantage of this approach is that our `foldMap` is not constrained
to read the input from start to finish.
We can, thus, do it in parallel chunks,
yielding an algorithm similar to the one by @matos_monoid_2006.

> recognizePar :: Monoid x => Int -> MonoidMachine x a -> [a] -> Bool
> recognizePar chunkSize m = accepting m . foldMapPar chunkSize (generators m)

Interestingly, I couldn't find a parallel `foldMap` already baked into Haskell.
So, let's implement our own that chunks the list and folds over each piece with a spark.

> foldMapPar :: Monoid m => Int -> (a -> m) -> [a] -> m
> foldMapPar _ _ [] = mempty
> foldMapPar chunkSize f xs = this `par` (this <> rest)
>  where
>    (prefix, suffix) = splitAt chunkSize xs
>    this = foldMap' f prefix
>    rest = foldMapPar chunkSize f suffix

Also keep in mind that how many chunks you should use
depends on how complicated your monoid is.
As everything related to parallelism, it can be somewhat of an art.

Examples
--------

All this theory is fun but it's time for some examples!
We'll start with my all-time favorite monoid: the natural numbers.
They, rather unsurprisingly, recognize languages defined by counting the letters with some property.

Let's say we accept a `String` if it has exactly $k$ vowels.
This is an easy to break task: count the vowels on each block, sum them, and check the final solution.

> exVowels :: Natural -> MonoidMachine Natural Char
> exVowels k = MonoidMachine gen check
>  where
>   gen a   = if isVowel a then 1 else 0
>   check m = m == k
>
> isVowel a = a `elem` ['a', 'e', 'i', 'o', 'u']

With some slight changes on the accepting predicate,
the monoid could check for at least or at most $k$ vowels.
Or perhaps a more interesting example:
is the number of vowels even?

If you are annoyed because $\mathbb{N}$ has an infinite multiplication table,
fear it not.
All these languages can also be recognized by finite monoids
--- next section's main topic.
The evenness check is even an old friend from elementary algebra,
the integers modulo $2$.
Let's take it as our next example.

The monoid $\mathbb{Z}_k$ consists of $k$ elements $\{0,\ldots,k-1\}$.

> newtype ZZ (k :: Nat) = ZZ Natural
> -- | Extract a type-level natural to the term level
> nat :: forall n. KnownNat n => Nat
> nat = natVal (Proxy @n)

And its operation is addition modulo the parameter $k$.

> instance KnownNat k => Semigroup (ZZ k) where
>  (ZZ x) <> (ZZ y) = ZZ $ rem (x + y) (nat @k)
> instance KnownNat k => Monoid (ZZ k) where
>  mempty = ZZ 0

These monoids let you check if a certain predicate happens a multiple of $k$ times.
For example, let's check that a string has an even amount of vowels.

> exEven :: MonoidMachine (ZZ 2) Char
> exEven = MonoidMachine gen check
>  where
>   gen a = ZZ $ if isVowel a then 1 else 0
>   check (ZZ 0) = True
>   check (ZZ _) = False

Notice how the monoid elements act as the machine's state.
They store the remainder at each piece of the string and then
combine it for a total remainder.

Alright, before going back to theory let's wrap this up with
a language whose interpretation is not so obviously parallel as counting.
I hope this also clarify how to do "modeling" with monoid machines.
Consider the language of all words ending with at least two letters `'e'`.
It contains `"free"` and `"lychee"` but not `"monoid"` or `"cheese"`, for example.
What is a finite monoid recognizing it?

First of all, we need to differentiate between the character `'e'` and everything else.
So let's start with the free monoid over two symbols: $b$ for the character of choice
and $a$ for everything else.
This monoid is subject to some equations.
Notice that no matter what their prefix is,
an ending in $a$ is unacceptable while an ending in $bb$ is recognized,
making these monoid elements right-absorbing,
$$xa = a,\quad x(bb) = bb.$$
This signature generates the monoid $M = \{e, a, b, ab, bb\}$
where $e$ is the identity element.

> data M = E | A | B | AB | BB
>  deriving (Eq, Ord, Ix, Bounded, Enum)

For its multiplication table,
we use the natural concatenation while observing that
the equations above imply that `A`, `AB`, and `BB` are right-absorbing.

> instance Monoid M where
>  mempty = E
> instance Semigroup M where
>  E  <> x  = x
>  x  <> E  = x
>  A  <> B  = AB
>  x  <> B  = BB
>  _  <> y  = y

To recognize the language, we use this monoid while accepting only `BB`.

> exTwoLast :: Eq a => a -> MonoidMachine M a
> exTwoLast k = MonoidMachine gen check
>  where
>   gen a = if a == k then B else A
>   check BB = True
>   check _  = False

You can test it on GHCi for the double `'e'` ending.

```ghci
ghci> fmap (recognize (exTwoLast 'e')) ["free", "lychee", "monoid", "cheese"]
[True,True,False,False]
```

Or even use it to test divisibility by 4 in binary strings.
(Note that this test fails for zero though)

> binaryExpansion :: Natural -> [Bool]
> binaryExpansion = reverse . unbase
>  where unbase x
>         | x < 2      = [x == 1]
>         | otherwise  = let (q, r) = quotRem x 2
>                        in (r == 1) : unbase q

```ghci
ghci> filter (recognize (exTwoLast False) . binaryExpansion) [1..40]
[4,8,12,16,20,24,28,32,36,40]
it :: [Natural]
```

From Monoids to Automata and Back Again
=======================================

You may wonder where do monoid machines fit in the Chomsky Hierarchy.
The proper answer, as with automata, is that it depends on the monoid properties.
Finite monoids --- which is the most computationally interesting case ---
recognize precisely the regular languages,
putting them on the same expressivity level of finite automata.

:::{.Theorem data-title="Kleene's for Monoids"}
A language is regular if and only if
there is a finite monoid recognizing it.
:::

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
and the transitions consist of contracting the current state with the new generator read.
We can keep the same accepting subset.

> monToDFA :: Monoid m => MonoidMachine m a -> DFA m a
> monToDFA mm = DFA mempty act (accepting mm)
>  where act a m = m <> generators mm a

Notice that the automaton is finite as long as the monoid `m` is finite.

For the other direction,
we use the DFA's transition monoid --- a trick reminiscent of difference lists.
We view the transition as a transformation `a -> (s -> s)`,
using that `s -> s` has a natural monoid structure from composition.
Thankfully all this lifting already comes bundled with Haskell in the `Endo` type.
Finally, to check if an endomorphism is accepted,
we test whether it takes the initial state to a final one.

> dfaToMon :: DFA s a -> MonoidMachine (Endo s) a
> dfaToMon (DFA q0 next final) = MonoidMachine (Endo . next) check
>  where check (Endo f) = final (f q0)

Again, the endomorphism type `s -> s` is finite if and only if `s` is itself finite.

Although these functions are not inverses,
you can check that they preserve the recognized language, which is enough for us.
It is also worth noticing that `s -> s` has exponentially more elements than `s`.
Nevertheless, the transition monoid is a submonoid of it that can be smaller.
Also, if you are calculating the elements on the fly, it is not necessarily a problem.


Maps, Matrices, and Parallelism
===============================

For a regular language specified as a DFA, `dfaToMon`
supposedly lets us recognize it in parallel using its monoid of endomorphisms.

> endoRecognize :: DFA s a -> [a] -> Bool
> endoRecognize dfa = recognize (dfaToMon dfa)

Now go on and run it in an example. I'll wait.
And to be fair, I'll wait a lot, because the method above is pretty slow.
The problem is that the monoid multiplication is too slow.
All it is doing is composing a thunk of functions whose execution only happens
when checking the final state.
We need a more strict representation --- something looking more like data than code.

The description of `Data.Map.Map k v` in the [containers](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Map-Strict.html)
package is as a finite map from `k` to `v`.
This fits perfectly into our application,
meaning we can swap `Endo` for `Map` in the monoid representation!
Let's write it as a new type as well as defining how to memoize a finite function using it.

> newtype AsMap s = AsMap (Map.Map s s)
>
> tabulate :: Finite s => (s -> s) -> AsMap s
> tabulate f = AsMap $ Map.fromList [(s, f s) | s <- elems]

For the monoid instance,
everything already comes bundled into `Data.Map`.
All it takes is to assemble the blocks.

> instance Finite s => Semigroup (AsMap s) where
>  (AsMap f) <> (AsMap g) = AsMap (Map.compose f g)
>
> instance Finite s => Monoid (AsMap s) where
>  mempty = tabulate id

Now we can turn a DFA into a monoid
for which all compositions are calculated instead of thunked.

> asMap :: Finite s => DFA s a -> MonoidMachine (AsMap s) a
> asMap (DFA q0 next final) = MonoidMachine gen check
>   where
>    gen a = tabulate (next a)
>    check (AsMap m) = final (m Map.! q0)

What about Matrices?
--------------------

The `Map` construction works well
but, to be fair, I could not go home without building the most classical monoid
associated with an automaton: its transition matrices!
The idea is to look at a finite function as a graph (or a relation)
and use its adjacency matrices

$$ T^a_{s s'} = \begin{cases} 1,& t(a, s) = s' \\ 0,& \text{otherwise}. \end{cases}$$

Or in Haskell:

> newtype Mat s t = Mat (Array (s, s) t)
>
> adjacency :: Finite s => (s -> s) -> Mat s Bool
> adjacency f = Mat $ A.accumArray (||) False bounds active
>  where
>   active = [((s, f s), True) | s <- elems]
>   bounds = (minBound, maxBound)

Magically (or not, depending on where you come from),
function composition becomes matrix multiplication.
We just have to take the ["relational" version of it](/posts/algebraic-path#transitive-closures-of-relations),
where sum becomes disjunction and product becomes conjunction.

> instance Finite s => Semigroup (Mat s Bool) where
>  (Mat f) <> (Mat g) = Mat $ A.genArray bounds combine
>   where
>    combine (x, y) = or [f A.! (x, k) && g A.! (k, y) | k <- elems]
>    bounds = (minBound, maxBound)
>
> instance Finite s => Monoid (Mat s Bool) where
>  mempty = adjacency id

Finally, running a DFA becomes a problem of multiplying a lot of binary matrices.
The generators are just the transformation matrices
while the accepting subset consists of those matrices
having a `True` component $A_{f i}$ for any final state $f$ and initial $i$.[^matrix-version]
This formulation is particularly suited for automata with few states
being run over large strings, thus many multiplications of small matrices.

> asMat :: Finite s => DFA s a -> MonoidMachine (Mat s Bool) a
> asMat (DFA q0 transition final) = MonoidMachine gen check
>   where
>    gen a = adjacency (transition a)
>    check (Mat m) = any (\s -> m A.! (s, q0)) (filter final elems)

[^matrix-version]: A more familiar definition may be as $\braket{f | A | i}$ where $f$ and $i$ are the indicator vectors of final and initial states.

I have chosen to use `Data.Array` in this post for simplicity.
But bear in mind that to achieve optimal results,
you will probably want a more robust numerical package such as `hmatrix`, `massiv` or even `accelerate`
to make use of those shiny GPUs.


Parting thoughts on nondeterminism
==================================

Alright, this was fun and all but now it's time to go.
One last point before wrapping this post though:
In contrast with the Map one,
when constructing the matrix monoid
we didn't really use the "deterministic" part of a DFA.
Therefore,
it should be a straightforward adaptation to make it work for nondeterministic automata!
All you have to do is alter `adjacency` to be a `s -> [s]` function
that constructs the matrix one column at a time.
Something like this should do:

> adjacencyNFA :: Finite s => (s -> [s]) -> Mat s Bool
> adjacencyNFA f = Mat $ A.accumArray (||) False bounds active
>  where
>   active = [((s, s'), True) | s <- elems, s' <- f s] -- This line changes!
>   bounds = (minBound, maxBound)

In fact,
all the automata from the [A Fistful of Automata](/posts/automata-monads/) post
admit a matrix representation as some kind of "generalized relation".
Probabilistic automata yield stochastic matrices and quantum automata yield unitary matrices.
I don't really know the condition on the monad `m` for `s -> m s` to be "matrixifiable"
but many practical examples seem to be.
Well, perhaps this will be a topic for a later post.

Good bye and have fun contracting your monoids!

Acknowledgements
================

Thanks to Gustavo Freire for not just listening while I babbled this post's first sketch
during a Friday night Uber ride, but also putting my handwaves in check,
and for pointing out a lot of typos and improvements once it was finally written.
Also thanks to [Yossi Frenkel](https://abstractnonsense.xyz/) for recently motivating some automata applications back in my mind.
