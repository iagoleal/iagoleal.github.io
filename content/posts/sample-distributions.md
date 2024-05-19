---
title: Uniformly Sampling Finite Probabilites
keywords: [math]
date: 2024-05-14
---

<style>
.Missing {
  text-align: center;
  width:  100%;
  height: 300px;
  background-color: gray;
  border: black 1px;
}
</style>

\def\inlsum{\textstyle\sum}

\def\norm#1{\left\lVert#1\right\rVert}
\def\Unif{\mathrm{Unif}}
\def\Exp{\mathrm{Exp}}
\def\Area{\mathrm{Area}}
\def\Sto#1{\mathbf{Sto}}

Oftentimes, I need to sample points from some finite set.
Most times, there is an obvious way to draw elements,
but even so often, it happens that I need
the _probability distribution itself_ to be random
(e.g. property testing for solvers depending on weighted averages.)
To avoid possible statistical misfortunes,
it is desirable to choose such a distribution uniformly among all candidates.

As it stands, there are some methods to do that.
Today we're going to explore my favorite one,
which only requires us to be able to independently sample uniformly from the $[0, 1]$ interval.
Besides, it is pretty fast to compute and has an elegant geometric flavour.
What more could we ask?

What's more, a lot of the lemmas we'll use are $L^1$ versions of well-known results for Gaussians.
Although their equivalence seems to be common knowledge among mathematicians,
I don't know about any reference that does things this way.
So, as a bonus, now I'll have where to link people towards whenever I want to talk about Markov kernel-invariance.
Oh well, it seems I'm putting the cart before the horse in here.
Time to start out.

In case you've landed on this page in search for a sampling algorithm
and don't care for the mathematical intricacies --- no matter how elegant they might be ---
here is the final theorem, which should get you covered.

:::Theorem
Let $U \sim \Unif[0, 1]^N$ be uniform on the $N$-cube and define random variables $E_i = -\log(U_i) \sim \Exp(1)$.
We can get a random vector that is uniformly distributed on all probabilities over $N$ elements as

$$ Z = \frac{1}{\sum_i E_i} E.$$
:::

Probabilities Over the Probabilities
====================================

To sample from a set, we must have a measurable structure.
In our case, this means we need a _probability over the probabilities_.
Since whenever our set has more than a single element,
its probability distributions form an infinite set,
we must ask ourselves whether our problem is even well-posed.

- What is a probability distribution over the probability distributions over a finite set?

- In what sense can one such probability be considered uniform?

To answer these,
let's first reduce our problem to a more concrete one.
For this, consider a finite set $S$.
All probabilities over it can be written
as linear combinations of the point masses (Dirac deltas)
on the elements of $\Omega$:

$$ p = \sum_{\omega \in \Omega} p_\omega \cdot \delta_\omega(x).$$

Since we need the $p_s$ to represent the probability of outcome $\omega$,
not every linear combination of deltas is acceptable.
To produce a probability distribution, We need the coefficients to form a _convex combination_, i.e.,

$$ p_\omega \ge 0 \text{ and } \sum_{\omega \in \Omega} p_\omega = 1.$$

:::Missing
Graph with lollipops represent the probabilities
:::

The set of all such coefficients is known as the **standard simplex** (simplex for short, from now on).
It is a compact convex subset of $\R^{N}$ and, as such,
has a clear notion of uniform probability inherited from the Lebesgue measure.
Furthermore, since the set $\Omega$ is fixed, sampling a random probability distribution on it
is equivalent to sampling the probability coefficients.
Therefore, we reduce our problem to a more geometrical one:
generating points uniformly in the simplex.

:::Missing
Interactive graph going from 3 lollipops to a simplex
:::


How to Sample From the Standard Simplex
=======================================

Now forget all that abstract nonsense and let's focus on subsets of $\R^N$.
If you prefer to remain abstract,
you can enumerate $\Omega = \{\, \omega_1, \ldots, \omega_N \,\}$
and use the isomorphism $\delta_{\omega_k} \mapsto e_k$
taking the point masses to the corresponding canonical basis vectors.
Everything will work the same.

Alright, we are interested in the set of all **probability vectors** in $\R^N$:

$$ \Delta^N = \left\{\, x \in \R^{N} \mid x \ge 0,\, \inlsum_k x_k = 1  \,\right\}.$$

:::Missing
Show the simplex
:::

As a compact subset of $\R^N$, it has a uniform probability measure given by the area of a subset

$$ p_{\Delta^N}(A) = \frac{\Area(A \cap \Delta^N)}{\Area(\Delta^N)}.$$

Our main goal in this section is to construct a way to sample from this probability
using only the standard tools you'd find in any programming language,
such as $\Unif[0, 1]$ distributions.
There are a couple ways to do it involving
rejection sampling or some kind of combinatorics with partitions of the interval.
The method of choice for this post uses symmetries
and mimics the well-known construction for the uniform distribution on the sphere.

Before getting technical, I think a sketch of the idea is due.
We look at the non-negative cone $\R^N_{\ge 0}$ as a stacking of simplexes $r \Delta^N$
whose components sum to $r$
--- In the same way as $\R^N$ is a stacking of spheres with radius $r$.

:::Missing
Illustrate this sketch with figures!

The stacking
:::

This amounts to parametrizing the positive quadrant in barycentric coordinates:

$$ x = r \sigma,\;\text{ where }\; r = \inlsum_k x_k,\, \sigma \in \Delta^N.$$

To produce a distribution on $\Delta^N$, take a non-negative random vector $X$
and scale it such that its components sum to one (barycentric projection).
By choosing a $X$ whose distribution is _invariant_ with respect to symmetries of the simplex,
the projection will be uniformly distributed.

:::Missing
Illustrate this sketch with figures!

The projection
:::

Now that you've (hopefully) got some intuition on what's our plan,
it's time to go on and prove the necessary theorems.

Symmetry and Invariance
-----------------------

Considering we talked about symmetries,
the most straightforward thing to do would be taking a look
at the group of linear automorphisms that preserve the simplex.
Unfortunately, it is too meager: no more than permutation matrices.
In other words, the group of (linear) symmetries is just the --- no pun intended -- symmetric group on its vertices.

$$ \mathrm{LinAut}(\Delta^N) \cong S_N. $$

We would like to get a continuous group for our symmetries,
nevertheless, but not all is lost.
The thing is: a full-on group was just too much to ask for.
Instead, the _monoid_ of linear transformations that preserve the simplex
will attend our needs much better than that symmetry group.

:::Definition
A linear transformation $M$ preserves a subset $A \subset \R^N_{\ge 0}$
if it takes elements of $A$ to elements of $A$,

$$ x \in A \implies M x \in A. $$
:::

In the case of the simplex,
the definition above simplifies to a finite amount of conditions,
because linear maps preserve extreme points of convex sets.
Since the simplex is the convex hull of the canonical basis vectors,
we only need to impose that their image does not leave the set
to guarantee that a map preserves the simplex.

$$\boxed{M e_k \in \Delta^N,\, k = 1,\ldots, N.}$$

:::Missing
Spherical caps
:::

The above is the abstract version of our condition.
However, we can put it into a more concrete form
by recollecting that $M e_k$ equals the coefficients on the $k$-th column of $M$.

$$
M = \left[
  \begin{array}{ccc}
    \rvert &        & \rvert\\
    M e_1 & \cdots & M e_N \\
    \rvert &        & \rvert
  \end{array}
\right].
$$

Consequently, the matrices that preserve the simplex are those
whose components are themselves probability vectors.

If you've ever tinkered with Markov Processes,
you may know those by the name of _stochastic matrix_ or _Markov Kernel_
and used them for the exact same reason: preserving the total probability in a transition step.
Also, they're the reason why my friends who decided to study probability
to run away from algebra ended up needing to learn about semigroups and monoids.
Even though I'm a fan of saying "Markov Kernels",
in this post we'll go with the name "stochastic" [^left-stochastic].

[^left-stochastic]: In fact, people in probability generally work with _left stochastic_ matrices,
because they like multiplying vectors from the left, i.e. $p M = p$,
and thus require the _rows_ to be probability vectors.
The matrices in this post are _right stochastic_ because we multiply vectors like normal people.
No confusion should arise, because in this post we'll only use the right kind.

:::Definition
A matrix is **stochastic** if its columns are probability vectors (elements of $\Delta^N$).
We denote by $\Sto{N}$ the monoid of all such matrices.
:::

Throughout the realms of mathematics, symmetries are known for producing invariants
because by preserving a set, they end up preserving some simple function related to it.
Rotations conserve inner products and lengths, translations preserve the differences, etc.
So, what is the invariant associated with stochastic matrices?
As you might expect from the discussion before,
they conserve the sum of components.[^invariant-nonneg]

$$ \sum_k (M x)_k = \sum_{k,l} M_{kl} x_l = \sum_l x_l \underbrace{\left( \sum_k M_kl \right)}_{= 1} = \sum_l x_l.$$

[^invariant-nonneg]: They also conserve non-negativity.
But we are only working in the cone $\R^N_{\ge 0}$

On top of this algebraic derivation,
we can also look at it from a more geometrical point of view
by using the barycentric coordinates from the previous section.
Write $x = r \sigma$, with $r = \sum_k x_k$ and $\sigma \in \Delta^N$.
The coordinates of $M x$ are

$$ Mx = M(r\sigma) = r \underbrace{(M \sigma)}_{\in \Delta^N}.$$

Thus, a stochastic matrix alters the coordinates in the standard simplex
but does not change in which simplex the vector is.
It's similar to how rotations alter directions but conserve in which concentric sphere a vector is.

:::Missing
simplex/Spherical regions
:::

Something cool about invariants is that we're able
to transfer them from points to functions.
In general, if a function is $G$ invariant, i.e.,

$$ \forall M \in G,\, f(M x) = f(x),$$

One expects it to only depend on the quantities conserved by $G$,
In our case of interest, \Sto{N}$,
we can rigorously prove that only the sum matters for such functions.

:::Theorem
A function $f : \R^n_{\ge 0} \to \R$ is $\Sto{N}$-invariant
if and only if it only depends on the sum of the input's components.
That is, there is a $\phi : \R_{\ge 0} \to \R$ such that

$$ f(x) = \phi\left(\inlsum\nolimits_k x_k\right). $$
:::

:::Proof
* $f$ depends only on $\inlsum_k x_k \implies$ $f$ stochastic-invariant:

  The function $f$ cannot "see" the changes $M \in \Sto{N}$
  produces in the space:

  $$
  f(M x) = \phi(\inlsum_k (M x)_k) = \phi(\inlsum_k x_k) = f(x).
  $$

* $f$ stochastic-invariant $\implies f$ depends only on $\inlsum_k x_k$:

Since $f$ is invariant by the action of _any_ stochastic matrix,
we can construct one that's appropriate for our needs.
Let's build a matrix that accumulates the sum of any vector into its first component while turning all others into zero.
You can think of this procedure as a $1$-norm version of rotating the coordinate system to the axis defined by $x$.

Concretely, we need the matrix $A$ whose first row is all ones and the others are identically zero.

$$
A = \left[
  \begin{array}{ccc}
    1      & \cdots & 1      \\
    0      & \cdots & 0      \\
    \vdots & \vdots & \vdots \\
    0      & \cdots & 0
  \end{array}
\right]
$$

This is a stochastic matrix that accumulates the sum of any vector into its first component.

$$ A x =
\left[
  \begin{array}{ccc}
    1      & \cdots & 1      \\
    0      & \cdots & 0      \\
    \vdots & \vdots & \vdots \\
    0      & \cdots & 0
  \end{array}
\right]
\left[
  \begin{array}{c}
    x_1      \\
    x_2      \\
    \vdots   \\
    x_N
  \end{array}
\right]
=
\left[
  \begin{array}{c}
    \sum_k x_k  \\
    0           \\
    \vdots      \\
    0
  \end{array}
\right]
$$

By invariance, we then get that

$$ f(x) = f(A x) = f( (\inlsum_k x_k) \cdot e_1) = \phi(\inlsum_k x_k)$$

Where we define $\phi(t) = f(t \cdot e_1)$.
:::


A Concrete Sampling Algorithm
-----------------------------

:::Theorem
Le $E$ be a non-negative random vector
whose distribution is continuous and invariant by stochastic matrices.
The random vector

$$ Z = \frac{E}{\sum_i E_i} \sim \Unif(\Delta^N).$$

In other words, $Z$ is uniformly distributed on the standard simplex.
:::

:::Theorem
If $Z$ is a random vector with continuous non-negative independent components
and its distribution only depends on the $L^1$ norm, i.e.,

$$ p_Z(x) = \phi(\norm{x}_1),$$

Then its components are i.i.d. exponentials,

$$ Z_i \sim \Exp(\lambda).$$
:::

:::Proof
Also see,

http://luc.devroye.org/rnbookindex.html page 594.

https://math.stackexchange.com/questions/105418/very-elementary-proof-of-maxwells-theorem/105470#105470
:::

# Quantum

$$ \frac{2}{p}\Gamma(1/p) e^{-\norm{x}_p^p} $$


# The General Case


Barthe, Franck, Olivier Guédon, Shahar Mendelson, and Assaf Naor. “A Probabilistic Approach to the Geometry of the ℓpn-Ball.” The Annals of Probability 33, no. 2 (March 1, 2005). https://doi.org/10.1214/009117904000000874.

https://metaphor.ethz.ch/x/2017/hs/401-3370-67L/sc/Joost1.pdf

http://blog.geomblog.org/2013/01/a-sampling-gem-sampling-from-ellp-balls.html
