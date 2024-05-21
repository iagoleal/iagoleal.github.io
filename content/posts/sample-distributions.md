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
\def\abs#1{\left|#1\right|}
\def\Unif{\mathrm{Unif}}
\def\Exp{\mathrm{Exp}}
\def\Area{\mathrm{Area}}
\def\Sto#1{\mathbf{Sto}(#1)}
\def\Id{\mathbb{I}}

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

This amounts to parametrizing the positive orthant in barycentric coordinates:

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
they conserve the sum of components.

$$ \sum_k (M x)_k = \sum_{k,l} M_{kl} x_l = \sum_l x_l \underbrace{\left( \sum_k M_kl \right)}_{= 1} = \sum_l x_l.$$

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
In our case of interest, $\Sto{N}$,
we can rigorously prove that only the sum matters for such functions.

:::Theorem
A function $f : \R^N_{\ge 0} \to \R$ is $\Sto{N}$-invariant
if and only if it only depends on the sum of the input's components.
That is, there is a $\phi : \R_{\ge 0} \to \R$ such that

$$ f(x) = \phi\left(\inlsum\nolimits_k x_k\right). $$
:::

:::Proof
* $f$ depends only on $\inlsum_k x_k \implies f$ is $\Sto{N}$-invariant:

  The function $f$ cannot "see" the changes $M \in \Sto{N}$ produces:

  $$ f(M x) = \phi(\inlsum_k (M x)_k) = \phi(\inlsum_k x_k) = f(x). $$

* $f$ is $\Sto{N}$-invariant $\implies f$ depends only on $\inlsum_k x_k$:

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

Now that we're acquainted with stochastic matrices and their invariants,
it's time to go back to probabilities and distributions.
When a random vector has a $\Sto{N}$-invariant distribution,
its density will be locally constant on any scaling of the simplex.
Hence, we can project it back to the standard simplex to get a uniform distribution.

:::Theorem
Let $E$ be a non-negative random vector
whose distribution is continuous and $\Sto{N}$-invariant.
The random vector

$$ Z = \frac{E}{\sum_i E_i} \sim \Unif(\Delta^N).$$

In other words, $Z$ is uniformly distributed on the standard simplex.
:::

The previous theorem is a recipe for turning $\Sto{N}$-invariant distributions
into ones that are uniform on the simplex.
The only thing missing is finding a suitable distribution to project.
What could it be?
As it turns out,
a vector of i.i.d. [exponentially distributed random variables](https://en.wikipedia.org/wiki/Exponential_distribution)[^exp-rv] just cuts it.
I don't want to just postulate it, however.
We've come this far from first principles,
so let's make the exponentials show themselves in our deduction.

[^exp-rv]: Continuous random variables with density $f(x) = \lambda e^{-\lambda x} \Id_{[0, \infty)}(x)$.

We can choose any invariant distribution,
so let's go with the easiest kind: those with independent components.
What I find the most impressive is that the constraints of $\Sto{N}$-invariance and independence together
are strong enough to characterize exponential distributions.
You can think of it as an adaptation of [Maxwell's theorem](https://en.wikipedia.org/wiki/Maxwell's_theorem)[^se-maxwell]
for the simplex.

[^se-maxwell]: See [this link](https://math.stackexchange.com/questions/105418/very-elementary-proof-of-maxwells-theorem/105470#105470)
for the deduction of Maxwell's theorem which inspired my proof below.

:::Theorem
The only $\Sto{N}$-invariant absolutely continuous distributions on $\R^N_{\ge 0}$ (the non-negative cone)
with independent components are identically distributed exponentials $\sim \Exp(\lambda)$.
:::

:::Proof
We only consider absolutely continuous distributions supported on the positive orthant.
Hence, our distribution equals $p_Z = f \cdot \Id_{\ge 0}$ for an integrable probability density $f$.
Let's investigate this function.
By $\Sto{N}$-invariance, this density can only depend on the sum of components
and by independence, the joint density is a product of single-variable densities $f_i$,

$$ f(x) = \phi\left(\inlsum\nolimits_k x_k\right) = \prod_k f_i(x_k).$$

Let's turn it into a system of differential equations and solve for a closed form.
As we are working with distributions, we don't need to worry about smoothness right now
because all derivatives can be taken in a weak sense.

$$ \partial_i f(x) = \phi'\left(\inlsum\nolimits_k x_k\right) = f_i'(x_i) \prod_{k \ne i} f_k(x_k).$$

TODO: PROVE f POSITIVE

By dividing both sides by $f$, we get to

$$ \frac{\phi'\left(\inlsum\nolimits_k x_k\right)}{\phi\left(\inlsum\nolimits_k x_k\right)} = \frac{f_i'(x_i)}{f_i(x_i)}.$$

The above works independently of $i$, meaning that the fractions $f_i'(x_i) / f_i(x_i)$ are all equal.
However, each depends on a different variable.
The only way this can happen is if they all _equal the same constant_.
Let's smartly call this constant $-\lambda$.

$$ \frac{f_i'}{f_i} = -\lambda \implies f_i' = -\lambda f_i.$$

Great! As Since the only weak solutions to a linear ordinary differential equation
are the classical ones,
the above proves that the only possible densities are exponentials.

$$\boxed{f_i(x_i) = C_i e^{-\lambda x_i}}.$$

You can use that the $f_i$ are probability densities to deduce that $\lambda > 0$
and $C_i = \lambda$ are the only admissible constants.
The reasons are their tail has to go to zero and they must integrate to one.

$$ \begin{array}{ll}
\lim_{t \to \infty} C_i e^{-\lambda t} = 0 &\iff \lambda > 0, \\
1 = \int_0^\infty C e^{-\lambda t} dt = \frac{C}{\lambda} &\iff C_i = \lambda.
\end{array}$$

Therefore, for non-negative arguments, the joint density has the form

$$ p_Z(x) = \lambda^N e^{-\lambda \sum_k x_k} \cdot \Id_{\ge 0} $$

Which characterizes a multivariate exponential with i.i.d. components.
:::

Among its many properties, the exponential distribution is famously
simple to generate from a uniform distribution on $[0, 1]$ (which are available everywhere),
as a consequence of the [inverse transform sampling](https://en.wikipedia.org/wiki/Inverse_transform_sampling#):

$$ U \sim \Unif[0, 1] \implies -\frac{1}{\lambda}\log(U) \sim \Exp(\lambda).$$

Combining the above with this section's theorems,
we arrive at a way to sample uniformly from the simplex
using only $\Unif[0, 1]$ distributions --- which are available everywhere.
The function below illustrates how simple an implementation can be.
To be fair, I could've even made it a one-liner, but it's spelt out for legibility.


```julia
function sample_simplex(n)
  U = rand(n)              # Uniformly distributed on n-cube
  E = map(u -> -log(u), U) # E[i] ~ Exp(1)

  return E / sum(E)        # Uniformly distributed on simplex
end
```

It's a single step to go from the previous method
to a uniform sampling on the probabilities over probabilities.
In Julia code, you could do the below.

```julia
function sample_prob(xs)
  P = sample_simplex(length(xs))
  return Dist(xs[i] => P[i] for i in eachindex(xs))
end
```

Bonus: Using the Simplex to Sample Other Sets
=============================================

The previous section already wrapped up our main goals.
Nevertheless, it would be a shame develop such a tool
and simply finish the post and go home.
Let's thus explore how to use $\Delta^N$
to uniformly sample from a couple other interesting sets.
By the way, this is a bonus section, so feel free to ignore it
and just call it a day.

Stochastic Matrices
-------------------

Stochastic matrices helped us a lot during this post,
specially as a tool to understand the invariants related to the simplex.
Let's switch our view during this section to put them on the spotlight.
I think they deserve it.

How to sample uniformly from $\Sto{N}$?
Well, their only requirement is that each column must be a probability vector
--- an element of the simplex.
Thus, the stochastic matrices are equivalent to $N$ independent copies of the simplex,

$$ \Sto{N} \cong \prod_{i = 1}^N \Delta^N.$$

This is a compact set, by [Tychonoff's theorem](https://en.wikipedia.org/wiki/Tychonoff's_theorem#)
and we can uniformly sample from it by drawing each component independently.

```julia
function sample_sto(n)
  # Sample n vectors uniformly from simplex
  Ps = (sample_simplex(n) for i = 1 in 1:n)
  # Concatenate vectors into columns of a matrix
  return reduce(hcat, Ps)
end
```

$1$-norm unit sphere
--------------------

All that we've done is deeply linked to the $1$-norm,

$$ \norm{x}_1 = \sum_k \abs{x_k}.$$

The $1$-norm unit sphere is the set of all points
whose $1$-norm equals $1$,

$$ \partial B^N_1 = \{\, x \mid \norm{x}_1 = 1 \,\}.$$

:::Missing
1-norm in dimension 2.
:::

This is a collage of identical simplexes, one for each orthant.
As their intersections have zero measure,
we can uniformly sample from the sphere by drawing a point in the simplex
and throwing $n$ coins to decide the component's sign.

```julia
function sample_1_sphere(n)
  Z = sample_simplex(n)    # Point in the simplex
  S = rand([-1, 1], n)     # n choices of positive/negative

  return Z .* S            # Pointwise product
end
```

Triangulated Regions and Manifolds
----------------------------------


Bonus: Sampling from $p$-norm spheres
=====================================

$$ \frac{2}{p}\Gamma(1/p) e^{-\norm{x}_p^p} $$

https://mathoverflow.net/questions/9185/how-to-generate-random-points-in-ell-p-balls/9188#9188


Barthe, Franck, Olivier Guédon, Shahar Mendelson, and Assaf Naor. “A Probabilistic Approach to the Geometry of the ℓpn-Ball.” The Annals of Probability 33, no. 2 (March 1, 2005). https://doi.org/10.1214/009117904000000874.

https://metaphor.ethz.ch/x/2017/hs/401-3370-67L/sc/Joost1.pdf

http://blog.geomblog.org/2013/01/a-sampling-gem-sampling-from-ellp-balls.html
