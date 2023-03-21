---
title: The Algebraic Path Problem
keywords: [math, haskell]
date: 2023-03-11
---

\def\States{\mathcal{S}}
\def\Actions{\mathcal{A}}
\def\R{\mathbb{R}}
\def\E{\mathbb{E}}
\def\Bellman{\mathcal{B}}

> {-# LANGUAGE PackageImports,      DataKinds           #-}
> {-# LANGUAGE DeriveTraversable,   KindSignatures      #-}
> {-# LANGUAGE TypeApplications,    AllowAmbiguousTypes #-}
> {-# LANGUAGE ScopedTypeVariables, RankNTypes          #-}
> import         Control.Applicative
> import         Data.List (foldl')
> import         Data.Proxy ( Proxy(Proxy) )
> import         GHC.TypeNats ( KnownNat, Nat, natVal )
> import "array" Data.Array

Classical Shortest Paths
========================

```{.tikz tikzlibrary="positioning,quotes,arrows,arrows.meta"}
\begin{scope}[every node/.style = {circle, fill=black, outer sep=1mm, minimum size=2mm}]
  \node [] (A) []                       {};
  \node [] (B) [above right = of A]     {};
  \node [] (C) [below right = 2cm of A] {};
  \node [] (D) [right       = 4cm of A] {};
  \node [] (E) [left        = 2cm of A] {};
\end{scope}

\begin{scope}[every edge/.style = {-Latex, draw}
            ,every edge quotes/.style = {anchor = center, pos=0.5, fill = white, inner sep = 2pt, font = \tiny}
            ]
\path[->] (A) edge["10",   bend left]  (B)
          (A) edge["5",    bend right] (C)
          (B) edge["21",   bend left]  (C)
          (C) edge["-3",   bend right] (A)
          (D) edge["19.5", bend right] (B)
          (E) edge["2",    bend right] (A)
          (E) edge["-2",   bend right] (C);
\end{scope}
```

Consider a graph with weights on its edges.
Like the one in the figure, for example.
One way to represent this graph is via an adjacency matrix $A$
whose component $A(s, t)$ is the weight of the edge between nodes $s$ and $t$.
Notice that since there may not be edges between all vertices
(after all, what's the fun in a complete graph?), $A$ is a partial function.
We can solve this by adjoining a value $\infty$ to the possible weights
and assigning it as the weight of the non-existent edges.
In case this solution seems like a dirty trick to you,
a possibility is to think of this process as the same as returning
a `Maybe` value to make a partial function total,
with $\infty$ playing the role of `Nothing`.[^maybe-monad]

[^maybe-monad]: Assuming you don't consider Kleisli categories
as something as dirty as arithmetic with infinites.


Given this graph, a common question to ask is
what are best ways to travel between its vertices.
That is, if you want to go from node $s$ to node $t$,
which sequence of edges should you choose? If you've read what
[I have been writing about dynamic programming](/posts/dynamic-programming),
you are probably already smelling the sweet scent of
_value functions_ and Bellman equations in the air.

Let $\States$ be the (finite) set of nodes in the graph,
and let's define $V \colon \States \times \States \to (-\infty, \infty]$ as the value function
which tells us the length of the shortest path between two vertices.
With a bit of dynamic programming, we can find out that $V$
satisfies a Bellman equation:

$$
\begin{aligned}
 V(s, s) &= 0 \\
 V(s, t) &= \min_{q \in \States}\, A(s, q) + V(q, t), \; \forall s \ne t.
\end{aligned}
$$

The first line is the base case for the recursion
and represents the 0-step paths.
It costs nothing to just stay where you are but it is in turn impossible
to reach any other vertex in zero steps.
This may seem really simple, but let's nevertheless define a matrix $I$
to represent this 0-step reachability relation.
Why we want to do that will soon be clear.

$$
 I(s, t) = \begin{cases}
   0      ,& s = t \\
   \infty ,& s \ne t.
 \end{cases}
$$

We can use $I$ to write the recurrence more compactly:

$$
  V(s, t) = \min \left\{ I(s, t),\, \min_{q \in \States}\, A(s, q) + V(q, t) \right\}.
$$

What the above equation means is that the minimal distance between two vertices
is the shortest among staying in the vertex (an empty path)
and recursively following the shortest path with at least one edge.

Every problem is linear if you squint hard enough {#sec:algebraic-paths}
=================================================

The equation above is rather ugly indeed.
However, by looking at it with care, one can the structure unveiling.
First of all, the term

$$
  \min_{q \in \States}\, A(s, q) + V(q, t)
$$

looks a lot like some kind of composition where we aggregate over the middle index $q$.
Even more: if you squint your eyes enough, it looks a lot like the formula for matrix multiplication.

Well, let's try to discover where this idea takes us.
Even if it is just for the sake of abstract non-sense, it could be interesting.
In general, when talking about real numbers, we use the usual arithmetic structure
with addition and multiplication.
Today we're going to be eccentric and define a new kind of addition and multiplication:

$$
\begin{aligned}
  a \oplus b  &= \min\{a,\, b\}, \\
  a \otimes b &= a + b,          \\
  \mathbf{0}  &= \infty,         \\
  \mathbf{1}  &= 0.
\end{aligned}
$$

So, what is happening here?
The thing is: the real numbers extended with $\infty$
satisfy all the axioms for a [Semiring structure](https://en.wikipedia.org/wiki/Semiring)
if we take sum to be $\min$ and product to be $+$.
This is called the **min-plus** or
[**Tropical** semiring](https://en.wikipedia.org/wiki/Tropical_semiring)
and, surprisingly, satisfies a lot of the axioms we expect from a sum and product
The main difference is that instead of having inverses, $\oplus$ is idempotent.

Before further exploring the properties of the Tropical semiring,
or of semirings in general,
let's take advantage of our new notation to rewrite the ugly equation

$$
  V(s, t) = I(s, t) \oplus \bigoplus_{q \in \States} A(s, q) \otimes V(q, t)
$$

into the much more elegant (and index-free!) matrix format

$$
  V = I \oplus (A \otimes V).
$$

By now, the choice of the letter $I$ for the 0-step becomes clear:
it is the identity matrix in the Tropical algebra!

What is cool is that in this form, finding the shortest path
becomes the same as solving a linear system.
Can we transfer the methods from classical linear algebra to this semiring setting?
Unfortunately the answer is in general negative...
This is why we will have to require a further piece of structure from our semirings:
a **closure operator** denoted $^*$ which returns the fixed point of the mapping
$x \mapsto \mathbf{1} \oplus a \otimes x$, that is, we require that

$$ x^* = \mathbf{1} \oplus a \otimes x^* = \mathbf{1} \oplus x^\star \otimes a. $$

Now, you may call me a cheater since this is the scalar version of the shortest path equation.
Nevertheless, we will see that while such an operator
tends to be pretty simple to define for the scalar field,
it is also the missing piece for solving the **Algebraic Path Problem**.
That is, an algorithm capable of solving the Bellman equation
in a way that is polymorphic on the semiring.


Alright, that was a pretty mathematical introduction.
It's far from time for this post to become more computational.
Let's take the above discussion to Haskell land
by defining a `Semiring` class.

> class Semiring r where
>  zero,  one   :: r
>  (|+|), (|*|) :: r -> r -> r
>  closure      :: r -> r
>
> infixl 6 |+|
> infixl 7 |*|
> infixl 8 |^|

For later use, let's also define an exponentiation operator
using the classic divide-and-conquer formula.

> (|^|) :: (Semiring r, Integral n) => r -> n -> r
> x |^| 0 = one
> x |^| n | even n    = x |^| div n 2 |*| x |^| div n 2
>         | otherwise = x |*| x |^| (n-1)

As usual, there are also a bunch of laws that an element of this class must obey.
Since those are all pretty standard, I will just direct you to the
[Wikipedia article on the topic](https://en.wikipedia.org/wiki/Semiring).

The Tropical Semiring and Shortest Paths
========================================

One can think of a semiring as a structure
with an operation for combining two values ($\otimes$)
and another for aggregating values ($\oplus$).
Since the min-plus semiring was our choice for intuition,
let's start the implementation with it.
For the sake of polymorphism, we allow a Tropical version of any type.

> data Tropical a = Finite a | Infinity
>   deriving (Eq, Ord)

For any ordered numerical type, such as the integers or reals,
there is a Tropical structure given by what we discussed earlier.

> instance (Ord a, Num a) => Semiring (Tropical a) where
>  zero = Infinity
>  one  = Finite 0

For addition, the derived `Ord` instance
already puts `Infinity` as the largest element.

>  (|+|) = min

The product equals the usual sum for numbers
with the extension that `Infinity` is absorbing:
adding an infinite quantity to any number, no matter how small,
always produces an infinite result.

>  x        |*| Infinity = Infinity
>  Infinity |*| y        = Infinity
>  Finite x |*| Finite y = Finite (x + y)

Now, this next one is interesting.
The closure of `x` is equivalent to the shortest distance
in a graph with a single node.
If there are no negative cycles, this equals to $0$,
the length of the empty path.
Therefore,

>  closure _ = Finite 0 -- the distance taken by not moving

Of course, we are not interested only in single node graphs.
Let's take a look at weighted adjacency matrices.

Matrices over a semiring are a semiring
---------------------------------------

Since working wht Haskell arrays may be described as anything but pleasent,
let's define some simple wrappers to ease our life a little bit.
We will go with some dependentish square matrices,
but nothing that makes our code too complicated.

> -- | n x n square matrix with components in r
> newtype Matrix (n :: Nat) a = Matrix (Array (Nat, Nat) a)
>   deriving (Eq, Functor, Foldable, Traversable)

To ease our life, let's also define some methods to get a cleaner interface.
First of all, a smart constructor
that transforms a function on pairs of indices into a `Matrix`.

> -- | Extract a type-level natural to the term level
> nat :: forall n. KnownNat n => Nat
> nat = natVal (Proxy @n)
>
> matrix :: forall n a. KnownNat n => ((Nat, Nat) -> a) -> Matrix n a
> matrix f = Matrix $ array ends [(x, f x) | x <- range ends]
>   where ends = ((1, 1), (nat @n, nat @n))

And another smart constructor that turns a list of edges
into the respective adjacency matrix.

> adjacency :: forall n r. (KnownNat n, Semiring r)
>           => [((Nat, Nat), r)] -> Matrix n r
> adjacency es = let Matrix o = one :: Matrix n r
>                in Matrix (o // es)

It is also nice to have an `Applicative` instance, in order to aide us during future lifts.
Notice that since our matrices are a type with fixed size,
we can lift operations by simply matching the components.

> instance KnownNat n => Applicative (Matrix n) where
>  pure x = matrix (const x)
>  liftA2 f (Matrix x) (Matrix y) =
>     matrix $ \(s, t) -> f (x ! (s, t)) (y ! (s, t))


With these tools, we can keep away from the Array low-level API,
with the exception of indexing.
Now we can properly define the `Semiring` instance for a matrix.

Addition and zero are simply defined pointwisely.

> instance (KnownNat n, Semiring r) => Semiring (Matrix n r) where
>  -- These are the pointwise lifts of their scalar versions
>  zero  = pure zero
>  (|+|) = liftA2 (|+|)

For multiplication, we define `one` as the identity matrix with `one`s
on the diagonal and zero elsewhere (the same as in the introduction),
and use the traditional definition to build the product.

>  -- Identity matrix
>  one  = matrix $ \(i, j) -> if i == j then one else zero
>  -- Matrix multiplication using Semiring operators
>  Matrix x |*| Matrix y = matrix contract
>   where
>    contract (s, t) = add [x ! (s, q) |*| y ! (q, t) | q <- [1..nat @n]]
>    add             = foldl' (|+|) zero

Finally, it is time for our main algorithm: how to calculate the closure of a matrix.
Let's take another look at the equation that $A^\star$ must obey in order to gather some intuition.

$$
  A^\star = I \oplus (A \otimes A^\star)
$$

Let's try an experiment: what happens if we repeatedly
substitute the $A^\star$ term on the right-hand side
with the entire right-hand side?
After all, they're equal...
By doing that, we get a representation of the closure as a power series.

$$
  \begin{aligned}
  A^\star &= I \oplus A \oplus A^2 \oplus A^3 \oplus A^4 \oplus \ldots \\
          &= \bigoplus_{k = 1}^\infty A^k
  \end{aligned}
$$

What does this view contributes to us?
Let's take a look in the Tropical Semiring.
Each power $A^k$ of the adjacency matrix has at its components $A^k(s, t)$
the distance of the shortest path from $s$ to $t$ with exacly $k$ edges.
Thus, while the recursive Bellman equation decomposes the cost of an edge plus the cost of a smaller path,
this power series formulation, on the other hand,
is saying that the optimal cost among all paths may be decomposed by path length.
The minimum among all path is the minimum among all paths of any fixed length.[^linked-lists]

[^linked-lists]: This is very similar to how a linked list can be viewed either
as a pair containing a value and a reference to another list or as a vector that can have any possible length.

Since there are just $n$ vertices and we are assuming no negative cycles in order for the problem to be well-posed,
looking at any power $A^{n+1}$ is redundant.
Because, without negative cycles, it is never worth it to pass through any vertex more than once.

A way to calculate the closure is with a variation the
[Floyd-Warshall](https://en.wikipedia.org/wiki/Floyd-Warshall_algorithm)
or the [Gauss-Jordan](https://en.wikipedia.org/wiki/Gaussian_elimination) algorithm
that is adapted to work on any closed semiring.[^idempotent-algo]

>  closure x = one |+| foldl' step x [1..nat @n]
>   where step (Matrix m) k = matrix relax
>          where relax (i, j) = let c = closure (m ! (k, k))
>                  in m ! (i, j) |+| m ! (i, k) |*| c |*| m ! (k, j)

[^idempotent-algo]: There is also a pretty elegant alternative that,
  unfortunately, only works for idempotent semirings.
  It is a little slower ($O(n^3 \log n)$ versus $O(n^3)$) but it is worth showing here.
  If $R$ is idempotent, then all cross-terms in a binomial cancel out and we get
  $$ (I \oplus A)^n = \bigoplus_{k=0}^n = A^\star. $$
  From this equation follows a pretty sleek one-liner:

        closure a = (one |+| a) |^| nat @n
  Sadly, it requires idempotence...


To see that it is in general necessary to cross $n$ vertices,
consider a graph consisting of a single path passing through all nodes.
It takes exactly $n$ iterations to discover that all vertices are reacheable from the first.

```dot
digraph "Linear Graph" {
  rankdir=LR;
  size="8,5"

  node [shape     = circle
        style     = "solid,filled"
        color     = black
        fixedsize = shape
        fillcolor = invis];

  subgraph cluster_ldots {
    rank = same;
    style = invis;
    ldots [fontsize=20 color = "#00000000" fillcolor= "#00000000" label = ". . ."];
  }

  1 -> 2 -> 3 -> ldots -> n;
}
```

From distances to paths
-----------------------

Great, we now have a procedure capable of converting any
weight matrix into a matrix of shortest distances!
We can even write a little wrapper to work directly
with association lists of edges.

> shortestDistances :: (KnownNat n, Num r, Ord r)
>                   => [((Nat, Nat), r)] -> Matrix n (Tropical r)
> shortestDistances = closure . adjacency . tropicalize
>  where tropicalize = (fmap . fmap) Finite





From this matrix, it is possible to reconstruct
the arguments for the optimal paths.
But wouldn't it be great if the algorithm could already return that for us?
As you may guess, there is a semiring capable of just that.

> -- | A type with path information appended
> data Path a = Path a [(Nat, Nat)] | Unreacheable

Transitive Closures of Relations
================================

Let's now divert our attention to another topic
that is nevertheless closely related: finite relations.
Think about the common binary relations that we work with everyday:
equality, order ($\le$, <, $\ge$, >), equivalence relations...
All of them share two importante properties: **transitivity** and **reflexivity**.

So, if you're going to work with some  relation,
a typical process is to complete it in order to turn
this relation transitive and reflexive.
Of course, your relation is just for the VIP
and you don't want to add every arbitrary pair of elements during this operation.
You want to find the _smallest_ reflexive-transitive relation containing yours.

<object data="finite-relation.svg" type="image/svg+xml">
  <img src="finite-relation.svg"
       alt=""
       title="Three Views on Finite Relations"
  />
</object>

This is called the **reflexive-transitive closure** of a relation.
Now, how do we calculate this thing?
As you may imagine, there is a semiring made just for that
and the algorithm is exactly the same as for shortest paths.
All we have to do is to use the Boolean semiring.

> instance Semiring Bool where
>  one  = True
>  zero = False
>  (|+|) = (||)
>  (|*|) = (&&)
>  closure _ = one  -- Reflexivity for a single vertex

Using the old trick of representing a finite relation as a Boolean matrix,
the operation we need is exaclty the closure for the matrix semiring. 

> reflexiveTransitive :: KnownNat n => Matrix n Bool -> Matrix n Bool
> reflexiveTransitive = closure

Interestingly, the above can also be comprehended as a common operation on graphs.
By translating the boolean matrix into the (unweighted) adjacency matrix of a directed graph,
this closure returns the graph whose edges are all paths on the original graph.
If there one can go from $s$ to $t$ in $G$, then $G^\star(s, t) = \mathtt{true}$.
In the figure below, we illustrate this relationship.

<object data="finite-relation-views.svg" type="image/svg+xml">
  <img src="finite-relation-views.svg"
       alt=""
       title="Three Views on Finite Relations"
  />
</object>

Free as in Regular Expressions
==============================

When we study some kind of algebraic structure,
we tend to find out some kind of datastructure that is intimately related to it.
For example, monoids have lists, magmas have binary trees,
and vector spaces have fixed-length boxes of numbers.
These datatypes are a way to represent the **free** version of the algebraic structure.
The actual definition of something begin free in mathematics is a bit too technical
for this blog's tone, but the important to us is that, in general,
a free structure can act as a skeleton (or syntax tree) representing a computation
and any other instance of the algebraic structure can be recovered with a suitable interpreter.[^perrone]

[^perrone]: As always in mathematics there are some technicalities about when this holds.
That's a cool topic related to monads and CT, but too out of scope for this discussion.
I suggest the books: @perrone_notes_2021 [chapter 5] and @riehl_ct_context [chapter 5].

Well, do you have any guess for what is the free closed semiring?
Perhaps surprisingly, it consists of **Regular Expressions**,
at least when we assume the sum to be idempotent.[^kleene-algebra]
Yep, regular expressions, those strange grawlixes that perl programmers love so much
and that renders your queries completely unreadable a couple weeks after you've written them.
Since I don't want this page to be worthy of a comic book cursing context,
we will follow a more disciplined approach and represent the regular expressions
as its own a [datatype](https://en.wikipedia.org/wiki/Regular_expression#Formal_definition).

[^kleene-algebra]: To be precise, regular expressions are the free
                   [Kleene Algebra](https://en.wikipedia.org/wiki/Kleene_algebra).

> data Regex a = Nope                      -- Empty Set: matches nothing
>              | Empty                     -- Empty String: matches anything
>              | Literal a                 -- A single character from the alphabet
>              | Union (Regex a) (Regex a) -- matches either an argument or the other
>              | Join  (Regex a) (Regex a) -- Matches one argument followed by the other
>              | Many  (Regex a)           -- Kleene star: Matches zero or more instances of something

Notice that we're limiting ourselves to truly regular expressions,
those that represent some kind of [**Regular Language**](https://en.wikipedia.org/wiki/Regular_language).

The semiring instance is straighforward,
since the constructors closely resemble the class methods.
We will only implement a couple simplifications
in order to not get expressions with redundants parts.

> instance Semiring (Regex a) where
>  zero    = Nope
>  one     = Empty
>  -- Union with some eliminations for empty elements
>  Nope |+| e    = e
>  e    |+| Nope = e
>  -- Remove empties when possible
>  Empty    |+| (Many e) = Many e  -- Star contains the empty string
>  (Many e) |+| Empty    = Many e
>  Empty    |+| Empty    = Empty
>  x        |+| y        = Union x y
> -- Concatenation and some simplifications
>  Nope  |*| x     = Nope          -- Annihilation
>  x     |*| Nope  = Nope
>  Empty |*| x     = x             -- Identity
>  x     |*| Empty = x
>  x     |*| y     = Join x y
>  -- Kleene star
>  closure Nope     = Empty        -- the closure is at least empty
>  closure Empty    = Empty        -- many instances of something empty
>  closure (Many e) = Many e       -- idempotence of Kleene star
>  closure e        = Many e

The previous definition was long but rather mechanical
and, most important of all, gave us a shining new semiring to play with!
Alright, what does the closure of a matrix of regular expressions?
First of all, a graph labeled with regular expressions is exactly a finite state machine
(more precisely, a ε-NFA),
and since in this case $\oplus$ is union and $\otimes$ is concatenation,
we get from the power series interpretation that the closure $A^*$
is the component-wise union of all fixed length paths one can follow in this automaton.
Hence, each component $A^*(s, t)$ is a regular expression
representing the language accepted by this automaton with initial state $s$
and accepting state $t$.

What is the Floyd-Warshall implementation, when specialized to this context?
Here it becomes Kleene's algorithm for converting a finite state machine into a regular expression.

As we discussed earlier, regular expressions are the free closed (idempotent) semiring.
This means that any operation on semirings, such as finding shortest-paths,
may be first calculated on regexes and then converted to the proper type.
We achieve this with an interpreter function.

> interpret :: Semiring r => (a -> r) -> (Regex a -> r)
> interpret f Nope        = zero
> interpret f Empty       = one
> interpret f (Union x y) = interpret f x |+| interpret f y
> interpret f (Join x y)  = interpret f x |*| interpret f y
> interpret f (Many e)    = closure (interpret f e)

This is similar to how Haskell's [`foldMap`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#v:foldMap)
works for monoids or the `eval` function that we
[defined for calculus expressions on another post](/posts/calculus-symbolic#sec:floating-calculus).
All of them are some kind of "realizations of typeclass".

The usefulness of this interpreter comes from the fact that it commutes with the matrix operations.
Thus, whenever we want to perform many different queries in a same graph with idempotent aggregations
(such as shortest paths, largest paths, most reliable paths, widest paths etc.),
we may first calculate the regular expressions representing these paths
and then collapse them separately for each semiring.

Classical Matrix Inversion
==========================

To wrap up this post,
let's take a look at a semiring that is not idempotent:
the classical field structure on the real/complex numbers.
Or, to be technicality accurate, one of these fields complete with an extra point
to amout for the non-invertibility of zero.

> data Classical a = Field a | Extra
>   deriving Eq

> instance (Eq a, Fractional a) => Semiring (Classical a) where
>  zero = Field 0
>  one  = Field 1
>  Extra   |+| _       = Extra
>  _       |+| Extra   = Extra
>  Field x |+| Field y = Field (x + y)

>  Field 0 |*| _       = Field 0
>  _       |*| Field 0 = Field 0
>  Extra   |*| _       = Extra
>  _       |*| Extra   = Extra
>  Field x |*| Field y = Field (x * y)

For a field, the closure operation has, in general, a closed form solution:

$$
  x^\star = 1 + x\cdot x^* \implies x^* = (1 - x)^{-1}.
$$

Great, I must admit that I had already started missing
being able to subtract and divide our numbers...
Unfortunately, not everything is flowers
and the number $1$ is non-invertible.
That's why we had to add an extra point to the structure.

>  closure Extra     = Extra
>  closure (Field 1) = Extra
>  closure (Field x) = Field $ recip (1 - x)

Since matrices have themselves a notion of inverse,
their closure also satisfies the equation $B^* = (I - B)^{-1}$
whenever the inverse on the right makes sense.
With a change of variables $I - B \to A$,
we arrive at a formula for matrix inversion:

$$ A^{-1} = (I - A)^*.$$

Furthermore, this works whenever the right-hand side has no `Extra` terms,
meaning that we've arrived at a safe inversion formula for a matrix.

> inv :: (Fractional a, Eq a, KnownNat n) => Matrix n a -> Maybe (Matrix n a)
> inv = traverse toMaybe . closure . conj
>  where
>   conj a = one |+| fmap (Field . negate) a   -- A -> I - A
>   toMaybe Extra     = Nothing
>   toMaybe (Field a) = Just a

To which classical algorithm is this equivalent?
By looking attentively at the definition of closure,
you will see that this is exactly the Gauss-Jordan elimination,
where in our case multiplying by the closure takes the same role
as dividing by a row in the classical presentation.


\begin{code}
  instance Show a => Show (Regex a) where
   show Nope  = ""
   show Empty = "ε"
   show (Literal a) = show a
   show (Union x y) = "(" ++ show x ++ "|" ++ show y ++ ")"
   show (Join  x y) = "(" ++ show x ++ show y ++ ")"
   show (Many x) = show x ++ "*"


  instance Show a => Show (Classical a) where
   show Extra     = "∞"
   show (Field a) = show a
  instance Show a => Show (Tropical a) where
   show Infinity     = "∞"
   show (Finite a) = show a


  pp (Matrix m) = printGrid m

  printGrid :: Show a => Array (Nat, Nat) a -> IO ()
  printGrid grid = mapM_ (putStrLn . textRepresentation) (toSimpleArray grid)

  toSimpleArray :: Array (Nat, Nat) a -> [[a]]
  toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]]
    where ((lowx, lowy), (highx, highy)) =  bounds grid

  textRepresentation :: Show a => [a] -> String
  textRepresentation = unwords . fmap show

  deriving instance Show a => Show (Matrix n a)


  es :: [[Bool]]
  es = (fmap.fmap) (==1) [[1,1,0,0], [1,0,1,0], [0,0,0,0], [1,0,0,1]]

  a :: Matrix 4 Bool
  a = matrix (\(s,t) -> (es !! fromIntegral (t-1) ) !! fromIntegral (s-1))

  b :: Matrix 4 Bool
  b = closure a
\end{code}


References
==========

https://r6.ca/blog/20110808T035622Z.html

@{graphs_dioids_semirings_2008}
@{path_networks_2010}
@{dolan_semiring_pearl_2013}

https://www.schoolofhaskell.com/user/pbv/a-regular-expression-matcher
