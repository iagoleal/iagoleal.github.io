---
title: Uniformly Sampling Finite Probabilites
keywords: [math]
date: 2024-05-14
---

\def\inlsum{\textstyle\sum}

\def\norm#1{\left\lVert#1\right\rVert}
\def\Unif{\mathrm{Unif}}
\def\Exp{\mathrm{Exp}}

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
Let's start out.

In case you've landed on this page in search for an algorithm
and don't care for the mathematical intricacies --- no matter how elegant they might be ---
here is the final theorem, which will get you covered.

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
not every linear combination of deltas produces a probability distribution.
We need the coefficients to form a _convex combination_.
In other words, the set of all acceptable coefficients is

$$ \Delta^N = \{\, p \in \R^{N} \mid p_\omega \ge 0,\, \sum p_\omega = 1  \,\}.$$

We call $\Delta^N$ the **standard simplex** (simplex for short, from now on).
It is a compact convex subset of $R^{N}$ and, as such,
has a clear notion of uniform probability inherited from the Lebesgue measure.

Since the set $\Omega$ is fixed, sampling a random probability distribution on it
is equivalent to sampling the probability coefficients.
Therefore, we reduce our problem to a more geometrical one:
generating points uniformly in the simplex.


How to Sample From the Standard Simplex
=======================================

:::Definition
A matrix $M$ is **stochastic** if all its columns are probability vectors (elements of $\Delta^N$).
:::

To simplify our notation,
we will often denote the $k$-th column of $M$ by $M_k$.

$$
M = \left[
  \begin{array}{ccc}
    \rvert &        & \rvert\\
    M_{1}  & \cdots & M_{N} \\
    \rvert &        & \rvert
  \end{array}
\right].
$$

:::Theorem
A matrix is stochastic if and only if it preserves the standard simplex.
:::

:::Proof
- $M$ stochastic $\implies$ $M$ preserves $\Delta^N$:

  The output of applying $M$ to a probability vector $x \in \Delta^N$
  amounts to a convex combination of its columns.
  Since, by definition of stochastic matrix, the columns $M_k \in \Delta^N$,
  the result follows by the convexity of the simplex.

  $$ M x = \sum_\beta x_\beta \cdot M_\beta \in \Delta^N.$$

- $M$ preserves $\Delta^N \implies M$ stochastic:

  We can recover the columns of $M$ by applying it to a vector in the canonical basis,
  $M_k = M e_k$.
  Since all $e_k \in \Delta^N$, and $M$ preserves the simplex, the columns $M_k = M e_k$
  are probability vectors.
:::

:::Theorem
A function $f : \R^n_{\ge 0} \to \R$ is invariant by the action of stochastic matrices
if and only if it only depends on the sum of the input's components.
That is, there is a $\phi : \R_{\ge 0} \to \R$ such that

$$ f(x) = \phi\left(\inlsum\nolimits_k x_k\right). $$
:::

:::Proof
- $f$ depends only on $\inlsum_k x_k \implies$ $f$ stochastic-invariant:

Start by noticing that stochastic matrices preserve the sum of components:

$$ \sum_k (M x)_k = \sum_{k,l} M_{kl} x_l = \sum_l x_l \underbrace{\left( \sum_k M_kl \right)}_{= 1} = \sum_l x_l.$$

This implies that $f$ is invariant by stochastic matrices:

$$
f(M x) = \phi(\inlsum_k (M x)_k) = \phi(\inlsum_k x_k) = f(x).
$$

- $f$ stochastic-invariant $\implies f$ depends only on $\inlsum_k x_k$:

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

Where we define  $\phi(t) = f(t \cdot e_1)$.


:::

:::Theorem
Le $E$ be a non-negative random vector
whose distribution is continuous and invariant by stochastic matrices.
The random random vector

$$ Z = \frac{E}{\sum_i E_i} \sim \Unif(\Delta^N).$$

That is, Z is uniformly distributed on the standard simplex.
:::

:::Theorem
If $Z$ is a random vector with continuous non-negative independent components
and its distribution only depends on the $L^1$ norm, i.e.,

$$ p_Z(x) = \phi(\norm{x}_1),$$

Then its components are i.i.d. exponentials,

$$ Z_i \sim \exp(\lambda).$$
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
