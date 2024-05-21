---
title: A Uniform Probability over Probabilities
keywords: [math]
date: 2024-05-14
---

<style>
.Missing {
  text-align:       center;
  width:            100%;
  height:           300px;
  background-color: gray;
  border:           black 1px;
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
\def\Triang{\mathcal{T}}
\def\Prob{\mathbf{Prob}}

```{=tex}

\tikzset{
  bubble/.style = {fill=black, circle, inner sep = 0.3mm, minimum size=0.04mm,}
}

\colorlet{sgrey}{gray!80}
\colorlet{sorange}{orange!70}
\colorlet{sgreen}{green!40}

\pgfdeclarelayer{background}
\pgfdeclarelayer{axes}
\pgfdeclarelayer{behind}
\pgfdeclarelayer{infront}
\pgfdeclarelayer{plane}
\pgfdeclarelayer{decor}
\pgfsetlayers{background,axes,behind,main,infront,plane,decor}
```


Oftentimes, I need to sample points from a finite set.
Usually, there is an obvious way to draw elements,
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

The set of all such coefficients is known as the **standard simplex** (simplex for short, from now on).
It is a compact convex subset of $\R^{N}$ and, as such,
has a clear notion of uniform probability inherited from the Lebesgue measure.
Furthermore, since the set $\Omega$ is fixed, sampling a random probability distribution on it
is equivalent to sampling the probability coefficients.
Therefore, we reduce our problem to a more geometrical one:
generating points uniformly in the simplex.


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

```tikz
\begin{scope} [scale = 2, xscale = 1.5, rotate around y=45]
  \coordinate (O)  at (0, 0, 0);
  \coordinate (e1) at (1, 0, 0);
  \coordinate (e2) at (0, 1, 0);
  \coordinate (e3) at (0, 0, 1);

  % Coordinate axes
  \pgfonlayer{axes}
    \foreach \i in {1,2,3} {
      \draw        (O) -- ($ 1.5*(e\i) $);
      \draw[thick, -Latex] (O) -- (e\i);
    };
  \endpgfonlayer

  % Basis points
  \pgfonlayer{decor}
    \foreach \i/\where in {1/above,2/left,3/below} {
      \node[bubble] at (e\i) {};
      \node at (e\i) [\where] {$e_\i$};
    };

    % Pin / Label
    \draw[-{Kite}] (0.8, 0.5) node[above] {$\Delta^N$}
      to[out = -90, in = 45] ($ (e1) ! 0.333 ! (e2) ! 0.333 ! (e3) $) {};
  \endpgfonlayer

  % The simplex
  \filldraw[color = sgreen, fill opacity = 0.9] (e1) -- (e2) -- (e3) -- cycle;
\end{scope}
```

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

```tikz
% The stacking
\begin{scope} [scale = 2, xscale = 1.5, rotate around y=45]
  \coordinate (O)  at (0, 0, 0);
  \coordinate (e1) at (1, 0, 0);
  \coordinate (e2) at (0, 1, 0);
  \coordinate (e3) at (0, 0, 1);

  % Coordinate axes
  \pgfonlayer{axes}
    \foreach \i in {1,2,3} {
      \draw        (O) -- ($ 3*(e\i) $);
      \draw[thick, -Latex] (O) -- (e\i);
    };

    % Canonical basis
  \endpgfonlayer

  % Basis points
  \pgfonlayer{decor}
    \foreach \i/\where in {1/above,2/left,3/below} {
      \node[bubble] at (e\i) {};
      \node at (e\i) [\where] {$e_\i$};
    };
  \endpgfonlayer

  % The simplex
  \filldraw[color = sgreen, fill opacity = 0.9] (e1) -- (e2) -- (e3) -- cycle;

  % Simplex scalings
  \foreach \d in {2, 3} {
    \filldraw[color = sorange, fill opacity = 0.4]
      ($ \d*(e1) $) -- ($ \d*(e2) $) -- ($ \d*(e3) $) -- cycle;

    % Ticks
    \pgfonlayer{decor}
      \node[bubble] at ($ \d*(e1) $) {};
      \node[bubble] at ($ \d*(e2) $) ["$\d$" left] {};
      \node[bubble] at ($ \d*(e3) $) {};
    \endpgfonlayer

  };

\end{scope}
```

This amounts to parametrizing the positive orthant in barycentric coordinates:

$$ x = r \sigma,\;\text{ where }\; r = \inlsum_k x_k,\, \sigma \in \Delta^N.$$

To produce a distribution on $\Delta^N$, take a non-negative random vector $X$
and scale it such that its components sum to one (barycentric projection).

```tikz {tikzlibrary="decorations.pathreplacing"}
\begin{scope} [scale = 2, xscale = 1.5, rotate around y=45]
  \coordinate (O)  at (0, 0, 0);
  \coordinate (e1) at (1, 0, 0);
  \coordinate (e2) at (0, 1, 0);
  \coordinate (e3) at (0, 0, 1);

  % Coordinate axes
  \pgfonlayer{axes}
    \foreach \i in {1,2,3} {
      \draw        (O) -- ($ 1.5*(e\i) $);
      \draw[thick] (O) -- (e\i);
    };

    % Canonical basis
  \endpgfonlayer

  % Basis points
  \pgfonlayer{decor}
    \foreach \i/\where in {1/above,2/left,3/below} {
      \node[bubble] at (e\i) {};
      \node at (e\i) [\where] {$e_\i$};
    };
  \endpgfonlayer

  % The simplex
  \filldraw[color = sgreen, fill opacity = 0.9] (e1) -- (e2) -- (e3) -- cycle;

  % Projection
  \coordinate (x)  at (1.5, 1.0, 0.5);
  \def\norm{3};

  \pgfonlayer{infront}
  \draw[-Latex]
    (x) node[bubble, "$X$"] {}
    --
    ($ {1/\norm}*(x) $) node[bubble, "\sigma" below] {};
  \endpgfonlayer

  \pgfonlayer{behind}
    \draw (O) -- (x);

    \draw[decorate,decoration={brace,raise=3pt,mirror}]
      (x) -- node[font = \tiny, above = 2mm, rotate=25] {$\sum_k X_k$} (O);
  \endpgfonlayer

\end{scope}
```
By choosing a $X$ whose distribution is _invariant_ with respect to symmetries of the simplex,
the projection's distribution can be made uniform.
Now that you've (hopefully) got some intuition on what's our plan,
it's time to go on and prove the necessary theorems.

Symmetry and Invariance {#symmetries}
-----------------------

Considering we talked about symmetries,
the most straightforward thing to do would be taking a look
at the group of linear automorphisms that preserve the simplex.
Unfortunately, it is too meager: only permutation matrices and nothing more.
In other words, the group of (linear) symmetries is just the --- no pun intended -- symmetric group on its vertices.

$$ \mathrm{LinAut}(\Delta^N) \cong S_N. $$

We would like to get a continuous group for our symmetries.
Nevertheless, not all is lost!
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

```tikz {tikzlibrary="decorations.pathreplacing"}
\begin{scope} [scale = 3/2, scale = 2, xscale = 1.5, rotate around y=45, local bounding box = simplexBB]
  \coordinate (O)  at (0, 0, 0);
  \coordinate (e1) at (1, 0, 0);
  \coordinate (e2) at (0, 1, 0);
  \coordinate (e3) at (0, 0, 1);

  % Coordinate axes
  \pgfonlayer{axes}
    \foreach \i in {1,2,3} {
      \draw        (O) -- ($ 1.5*(e\i) $);
      \draw[thick] (O) -- (e\i);
    };

    % Canonical basis
  \endpgfonlayer

  % Basis points
  \pgfonlayer{decor}
    \foreach \i/\where in {1/above,2/left,3/below} {
      \node[bubble] at (e\i) {};
      \node at (e\i) [\where] {$e_\i$};
    };
  \endpgfonlayer

  % The simplex
  \filldraw[color = sgreen, fill opacity = 0.4] (e1) -- (e2) -- (e3) -- cycle;

  % Image by stochastic matrix

  \coordinate (m1) at (0.1, 0.8, 0.1);
  \coordinate (m2) at (0.3, 0.1, 0.6);
  \coordinate (m3) at (0.7, 0.2, 0.1);
  \filldraw[color = sorange] (m1) -- (m2) -- (m3) -- cycle;

  \pgfonlayer{decor}
    \foreach \i/\where in {1/above right,2/right,3/above right} {
      \node[bubble] at (m\i) [pin={[pin edge = {bend left}]\where:$M e_\i$}] {};
      % \node at (m\i) [\where] {$Me_\i$};
    };
  \endpgfonlayer
\end{scope}
```

The above is the abstract version of our condition.
However, it takes a more concrete shape by recollecting that
$M e_k$ equals the coefficients on the $k$-th column of $M$.

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

```tikz {tikzlibrary="decorations.pathreplacing"}
\begin{scope} [scale = 2, xscale = 1.5, rotate around y=45]
  \coordinate (O)  at (0, 0, 0);
  \coordinate (e1) at (1, 0, 0);
  \coordinate (e2) at (0, 1, 0);
  \coordinate (e3) at (0, 0, 1);

  % Coordinate axes
  \pgfonlayer{axes}
    \foreach \i in {1,2,3} {
      \draw        (O) -- ($ 2*(e\i) $);
      \draw[thick] (O) -- (e\i);
    };

    % Canonical basis
  \endpgfonlayer

  % Basis points
  \pgfonlayer{decor}
    \foreach \i/\where in {1/above,2/left,3/below} {
      \node[bubble] at (e\i) {};
      \node at (e\i) [\where] {$e_\i$};
    };
  \endpgfonlayer

  % The simplex
  \pgfonlayer{main}
    \filldraw[color = sgreen, fill opacity = 0.9] (e1) -- (e2) -- (e3) -- cycle;
  \endpgfonlayer

  % The other simplex
  \pgfonlayer{infront}
    \foreach \d in {2} {
      \filldraw[color = sorange, fill opacity = 0.4]
        ($ \d*(e1) $) -- ($ \d*(e2) $) -- ($ \d*(e3) $) -- cycle;

      % Ticks
      \pgfonlayer{decor}
        \node[bubble] at ($ \d*(e1) $) {};
        \node[bubble] at ($ \d*(e2) $) {};
        \node[bubble] at ($ \d*(e3) $) {};
      \endpgfonlayer

    };
  \endpgfonlayer

  % Projection
  \def\norm{2.0}
  \coordinate (x)  at (0.5, 1.0, 0.5);
  \coordinate (mx) at (0.2, 0.4, 1.4);

  \draw[-Latex]
    (x) node[bubble] {} -- ($ {1/\norm}*(x) $) node[bubble, "\sigma" above] {};
  \draw[-Latex]
    (mx) node[bubble] {} -- ($ {1/\norm}*(mx) $) node[bubble, "$M\sigma$" above] {};


  \pgfonlayer{decor}
    \node at (x) [above] {$x$};
    \node at (mx) [right] {$Mx$};

    % Pin / Label
    \draw[-{Kite}] (1.5, 1) node[above] {$r\Delta^N$}
      to[out = -90, in = 0] ($ {2}*(e1)!0.4!(e2)!0.1!(e3) $) {};
  \endpgfonlayer


  \pgfonlayer{behind}
    \draw (O) -- (x);
    \draw (O) -- (mx);
  \endpgfonlayer

\end{scope}
```

Thus, a stochastic matrix alters the coordinates in the standard simplex
but does not change in which simplex the vector is.
It's similar to how rotations alter directions but conserve in which concentric sphere a vector is.

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
##### $f$ depends only on $\inlsum_k x_k \implies f$ is $\Sto{N}$-invariant:

  The function $f$ cannot "see" the changes $M \in \Sto{N}$ produces:

  $$ f(M x) = \phi(\inlsum_k (M x)_k) = \phi(\inlsum_k x_k) = f(x). $$

##### $f$ is $\Sto{N}$-invariant $\implies f$ depends only on $\inlsum_k x_k$:

We use the invariance to turn $f$ into a one-dimensional function,
by constructing a stochastic matrix $A$ that takes each simplex to a single coordinate.
You can think of this procedure as a $1$-norm version of rotating the coordinate system such $x$ aligns to an axis.
Concretely, we achieve this by defining $A e_k = e_1$,
which amount to a matrix whose first row is all ones and the others are identically zero.

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

This way, we see that being $\Sto{N}$-invariant constrains a function
to a rather simple form, because it is as if it could only depend on a single axis.
As we will shortly see,
this locally constant restriction also simplifies a lot
the possible invariant random variables.


A Concrete Sampling Algorithm
-----------------------------

Now that we're acquainted with stochastic matrices and their invariants,
it's time to go back to probabilities and distributions.
When a random vector has a $\Sto{N}$-invariant distribution,
its density will be locally constant on any scaling of the simplex.
Hence, we can project it back to the standard simplex to get a uniform distribution.

:::Theorem
Let $E$ be an almost surely positive random vector
whose distribution is continuous and $\Sto{N}$-invariant.
The barycentric projection of $E$ is uniformly distributed on the standard simplex,

$$ \frac{E}{\sum_k E_k} \sim \Unif(\Delta^N).$$
:::

:::Proof
The components being positive guarantees that $\sum_k E_k$ is also almost surely positive
and the projection $Z = E / \sum_k E_k$ is well-defined.
We know that $Z$ is supported on the simplex because it is normalized.
Let's use the invariance to show that it is uniformly distributed.

Consider a region $A \subset \Delta^N$.
The projection is in $A$ if and only if the original variable $E$
lies in a ray coming from the origin and crossing $A$ somewhere,

$$
\begin{aligned}
  \Prob\left(\frac{E}{\inlsum_k E_k} \in A\right)
  &= \Prob\left(E \in \left\{\, r \sigma \mid r \in (0, +\infty), \sigma \in A \,\right\} \right).
\end{aligned}
$$

```tikz {tikzlibrary="shapes.geometric"}
\begin{scope} [scale = 2, xscale = 1.5, rotate around y=45,
    heptagon/.style = {
      regular polygon,
      regular polygon sides=7,
      minimum width=1cm,
      outer sep=0pt,
      rotate = -45,
    }
  ]
  \coordinate (O)  at (0, 0, 0);
  \coordinate (e1) at (1, 0, 0);
  \coordinate (e2) at (0, 1, 0);
  \coordinate (e3) at (0, 0, 1);

  % Coordinate axes
  \pgfonlayer{axes}
    \foreach \i in {1,2,3} {
      \draw        (O) -- ($ 1.5*(e\i) $);
      \draw[thick] (O) -- (e\i);
    };

    % Canonical basis
  \endpgfonlayer

  % Basis points
  \pgfonlayer{infront}
    \foreach \i/\where in {1/above,2/left,3/below} {
      \node[bubble] at (e\i) {};
    };
  \endpgfonlayer

  % The simplex
    \filldraw[color = sgreen, fill opacity = 0.4] (e1) -- (e2) -- (e3) -- cycle;

  \coordinate (m) at (barycentric cs:e1=1,e2=0.8,e3=1);

  \node[heptagon, draw] (A) at (m) {};

  \pgfonlayer{plane}
    \node[heptagon,
          draw,
          minimum width = 2cm,
          left color=cyan!10,
          right color=cyan!40,
          opacity = 0.6] (Ar) at ($ 2*(m) $) {};
  \endpgfonlayer


  \pgfonlayer{behind}
    \foreach \i in {1, 2,  6, 7}
      \draw[-, opacity = 0.6] (O) -- (A.corner \i);

    \foreach \i in {1,...,5} {
      \pgfmathtruncatemacro{\j}{\i + 1}
      \shade[left color=cyan!15,right color=cyan!40, opacity=0.6]
            (O) -- (A.corner \i) -- (A.corner \j) -- cycle;
    };

    \foreach \i in {3,...,5}
        \draw[-, opacity = 0.6] (O) -- (A.corner \i);
  \endpgfonlayer


  \pgfonlayer{infront}
    \foreach \i in {1, 2, 6, 7}
      \draw[-] (A.corner \i) -- (Ar.corner \i);

    \foreach \i in {1,...,5} {
      \pgfmathtruncatemacro{\j}{\i + 1}
      \fill[left color=cyan!15,right color=cyan!40, opacity=0.6]
            (A.corner \i) -- (Ar.corner \i) -- (Ar.corner \j) -- (A.corner \j) -- cycle ;
    };

    \foreach \i in {3,...,5}
        \draw[-] (A.corner \i) -- (Ar.corner \i);
  \endpgfonlayer

  \pgfonlayer{decor}
    \node at (A.center)  [pin={[pin distance = 1.4cm, pin edge = {Stealth-, bend right}]-30:$Z \in A$}] {};
    \node at (Ar.center) [pin={[pin distance = 1cm, pin edge = {Stealth-, bend right}]60:$E \in C_A$}] {};
  \endpgfonlayer
\end{scope}
```

The distribution of $E$ is continuous,
so calculating the probability on the right amounts to your commonplace integral.
Since the density $f_E$ is $\Sto{N}$-invariant,
it can only depend on the sum of components (i.e., $f_E(x) = \phi(\inlsum x_k)$).
Thus, a change of variables for barycentric coordinates $x = (\inlsum_k x_k)\sigma$ seems like a good bet.

$$
\begin{aligned}
  \Prob(Z \in A)
  &= \int_{C_A} \phi(\inlsum_k x_k) dx
  = \int_0^\infty \int_A \phi(r) r^{n-1} d\sigma dr \\
  &= \Area(A) \underbrace{\int_0^\infty \phi(r) r^{n-1} dr}_{\text{constant}}
\end{aligned}
$$

Consequently, the distribution of $E / \inlsum_k E_k$ is a multiple of the area element on the simplex,
defining a uniform measure.
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

This is compact and we can uniformly sample from it by drawing each component independently.

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

Triangulated Regions and Polyhedra
----------------------------------

Triangulations are inescapable in geometry:
from algebraic topology to computer graphics, you will eventually bump into them.
They also help us sample from polyhedra --- even when non-convex.

To put it simply, a triangulation of a compact $K \subset \R^N$
is a finite selection of simplices $\Triang$ such that their union covers $K$
and all pairwise intersections have dimension less than $K$ (therefore, zero measure).[^triangulation]

[^triangulation]: There are more details, such a requiring that the intersection be themselves simplices.
But we're already getting too much outside the scope of this post.

The method in this topic is an evolution of the one for the $1$-sphere.
First we assign a triangulation to the polyhedron $K$.
Then, by weighting a choice of simplex from the triangulation by their volume
and choosing uniformly from that simplex, we get a uniform distribution on the whole triangulation.

A non-standard simplex $S$ is a convex combination of affinely independent vectors $y_i$,

$$S = \{\, x \mid \exists p \in \Delta^N, x = \sum_i p_i y_i \,\}.$$

You can generate it from a standard simplex
by mapping the canonical basis to the vectors spanning $S$.
From [our previous discussions](#symmetries), you already know this is a matrix $Y$
whose columns are the vectors $y_i$.
Then the variable $YP$ is uniformly distributed on $S$ whenever $P \sim \Unif(\Delta^N)$.
With this variable and the weighting process just discussed,
we get a uniform distribution on the whole triangulation.

```julia
function sample_polyhedron(K)
  T  = triangulate(K)         # Assuming you have a method to do it
  ws = Dist(τ => volume(τ) / volume(K) for t in T)
  Y  = sample(ws)             # Random triangle from K
  P  = sample_simplex(dim(K)) # Uniform on simplex

  return matrix(Y)*P          # Y*P = Σ P_iY_i
end
```

Conclusion
==========

Very well, today's journey has come to an end.
I hope you had fun exploring how a at first strange problem in statistics
could be reduced to a geometrical problem about symmetries.
I surely did.

To wrap up, I should point the similarity between today's derivation and Herschel-Maxwell's theorem
for rotation-invariant distributions.
Besides 
In particular because the sum equals the $L^1$ norm in the first quadrant.
Does this mean something deeper? I don't really know, but I bet so.
What I know is that one can sample from any $p$-norm sphere using a distribution
proportional to $e^{-\norm{x}_p^p}$.
The problem is that finding good transformations preserving general $p$-norms is not so direct.
To be fair, I can't think of anything but permutations.
That's a topic for a future post, perhaps.

Farewell and good sampling for y'all!
