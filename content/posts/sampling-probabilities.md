---
title: A Uniform Probability over Probabilities
keywords: [math, probability, simplex]
date: 2024-05-21
description:
  You may be used to sampling from probabilities, but what about sampling a random finite probability?
  As it turns out, this is a procedure rich in geometry and symmetries.
suppress-bibliography: true
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

\def\inlsum{{\textstyle\sum}}
\def\R{\mathbb{R}}

\def\norm#1{\left\lVert#1\right\rVert}
\def\abs#1{\left|#1\right|}
\def\Unif{\mathrm{Unif}}
\def\Exp{\mathrm{Exp}}
\def\Area{\mathrm{Area}}
\def\Sto#1{\mathbf{Sto}(#1)}
\def\Id{\mathbb{I}}
\def\Triang{\mathcal{T}}
\def\Prob{\mathbf{Prob}}
\def\d{\mathrm{d}}
\def\sumop{\Sigma}

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
but every so often, I end up needing
the _probability distribution itself_ to be random
(e.g. property testing for solvers depending on weighted averages.)
To avoid possible statistical misfortunes,
it is also desirable to choose such a distribution uniformly among all possibilities.

As it stands, there are some methods to do that.
Today we're going to explore my favorite one,
whose only requirement is uniformly sampling from the $[0, 1]$ interval.
Besides, it is pretty fast to compute and has an elegant geometric flavour.
What else could we ask?

What's more, a lot of the lemmas we'll use are $L^1$ versions of well-known results for Gaussians.
Although their equivalence seems to be common knowledge among mathematicians,
I don't know about any reference that does things this way.
So, as a bonus, now I have a place to link people to whenever I want to talk about Markov kernel-invariance.
Oh well, it seems I'm putting the cart before the horse in here.
Time to start out.

In case you've landed on this page in search for a sampling algorithm
and don't care for the mathematical intricacies --- no matter how elegant they might be ---
here is the final theorem, which should get you covered.

:::Theorem
Let $U_i \sim \Unif[0, 1]$ be i.i.d. uniform random variables and define $E_i = -\log(U_i) \sim \Exp(1)$.
The projection

$$ Z = \frac{1}{\inlsum_i E_i} E$$

Is uniformly distributed over all $N$-element probability weights.
:::

Probabilities Over the Probabilities
====================================

To sample from a set, we must have a measurable structure.
In our case, this means a _probability over the probabilities_.
Since whenever our set has more than a single element,
its probability distributions form an infinite set,
we must ask ourselves whether our problem is even well-posed.

- What is a probability distribution over the probability distributions over a finite set?

- In what sense can such probability be considered uniform?

To answer these,
let's first reduce our problem to a more concrete one.
For this, consider a finite set $S$.
All probabilities over it can be written
as linear combinations of the point masses (Dirac deltas)
on the elements of $\Omega$:

$$ p = \sum_{\omega \in \Omega} p_\omega \cdot \delta_\omega(x).$$

Since the $p_\omega$ represents the probability of outcome $\omega$,
not every linear combination of deltas is acceptable.
To produce a probability distribution, the coefficients must form a _convex combination_, i.e.,

$$ p_\omega \ge 0 \text{ and } \sum_{\omega \in \Omega} p_\omega = 1.$$

The set of all such coefficients is known as the **standard simplex** (from now on, just _simplex_ for short).
It is a compact convex subset of $\R^{N}$ and, as such,
has a clear notion of uniform probability inherited from the Lebesgue measure.
Furthermore, since the set $\Omega$ is fixed, sampling a random probability distribution
is equivalent to sampling the coefficients.
This way, our problem becomes geometrical:
how to generate points uniformly in the simplex.

Before, proceeding, let's introduce a little notation.
Since we will stumble into a lot of component sums,
let's define an operator to represent it.
This way, we free ourselves from juggling unnecessary indices.

::: {.Definition data-title="Component sum operator"}
The linear operator $\sumop : \R^N \to \R$
takes a vector to the sum of its components in the canonical basis
$$ \sumop x = \sum_{k = 1}^N x_k.$$
:::


How to Sample From the Standard Simplex
=======================================

Now forget all that abstract nonsense and let's focus on subsets of $\R^N$.
If you prefer to remain abstract,
you can enumerate $\Omega = \{\, \omega_1, \ldots, \omega_N \,\}$
and use the isomorphism $\delta_{\omega_k} \mapsto e_k$
taking the point masses to the corresponding canonical basis vectors.
Everything works the same.

Alright, we are interested in the set of all **probability vectors** in $\R^N$,
called the **standard simplex**

$$ \Delta^N = \left\{\, x \in \R^{N} \mid x \ge 0,\, \sumop x = 1  \,\right\}.$$

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

As a compact subset of $\R^N$, its normalized (hyper-)area element
defines a uniform probability measure

$$ p_{\Delta^N}(A) = \frac{\Area(A \cap \Delta^N)}{\Area(\Delta^N)}.$$

Our main goal in this section is to construct a way to sample from this probability
using only the standard tools you'd find in any programming language,
such as $\Unif[0, 1]$ distributions.
There are a couple ways to do it involving
rejection sampling or some kind of combinatorics with partitions of the interval.
The method of choice for this post, however,
uses symmetries and mimics the well-known construction for the uniform distribution on the sphere.

Before getting technical, I think a sketch of the idea is due.
We can think about the non-negative cone $\R^N_{\ge 0}$
as a stacking of simplexes $r \Delta^N$ whose components sum to&nbsp;$r$
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

$$ x = r \sigma,\;\text{ where }\; r = \sumop x,\, \sigma \in \Delta^N.$$

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
      (x) -- node[font = \tiny, above = 2mm, rotate=25] {${\textstyle\sumop X}$} (O);
  \endpgfonlayer

\end{scope}
```
By choosing an $X$ whose distribution is _invariant_ with respect to symmetries of the simplex,
the projection's distribution can be made uniform.
Now that you've (hopefully) got some intuition on what's our plan,
it's time to go on and prove the necessary theorems.

Symmetry and Invariance {#symmetries}
-----------------------


Considering we talked about symmetries,
the apparent next step would be taking a look
at the group of linear automorphisms that preserve the simplex.
Unfortunately, it is too meager: only permutation matrices and nothing more.
In other words, its group of (linear) symmetries is just the --- no pun intended -- symmetric group on its vertices.

$$ \mathrm{LinAut}(\Delta^N) \cong S_N. $$

We would prefer a _continuous group_ for our symmetries.
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
It takes a more concrete shape by noticing that
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
you may know those by the name of _stochastic matrix_ or _Markov Kernel_,
and perhaps even used them for the exact same reason: preserving the total probability in a transition step.
Also, they're the reason why my friends who decided to study probability
to run away from algebra ended up needing to learn about semigroups and monoids.
Even though I'm a fan of saying "Markov Kernels",
in this post we'll go with the name "stochastic matrices" [^left-stochastic].

[^left-stochastic]: In fact, people in probability generally work with _left stochastic_ matrices,
because they like multiplying vectors from the left, i.e. $p M = p$,
and thus require the _rows_ to be probability vectors.
The matrices in this post are _right stochastic_ because we multiply vectors like normal people.
(To be fair, people prefer the _left stochastic_ because they think about measures as functionals instead of vectors)
No confusion should arise, because in this post we'll only use the right kind.

:::Definition
A matrix is **stochastic** if its columns are probability vectors (elements of $\Delta^N$).
We denote by $\Sto{N}$ the monoid of all such matrices.
:::

Throughout the realms of mathematics, symmetries are known for producing invariants
because, by preserving a set, they end up preserving some simple function related to it.
Rotations conserve inner products and lengths, translations preserve the differences, etc.
So, what are the invariants associated with stochastic matrices?
For one, as you might expect from the previous discussion,
they conserve the sum of components.

$$ \sumop (M x) = \sum_{k,l} M_{kl} x_l = \sum_l x_l \underbrace{\left( \sum_k M_{kl} \right)}_{= 1} = \sumop x.$$

On top of this algebraic derivation,
there is also a more geometrical point of view.
Let's use the barycentric coordinates from the previous section to write
$x = r \sigma$, with $r = \sumop x$ and $\sigma \in \Delta^N$.
By linearity, the coordinates of $M x$ are

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
  \coordinate (x)  at ($ \norm*(barycentric cs:e1=1,e2=3,e3=1) $);
  \coordinate (mx) at ($ \norm*(barycentric cs:e1=1.5,e2=1.5,e3=2) $);
  \coordinate (s)  at ($ {1/\norm}*(x) $);
  \coordinate (ms) at ($ {1/\norm}*(mx) $);

  \draw[-Latex]
    (x)  --  (s)  node[bubble, "$\sigma$" above] {};
  \draw[-Latex]
    (s)  --  (ms) node[bubble, "-90:$M\sigma$"] {};
  \draw[-Latex]
    (ms) --  (mx);

  \pgfonlayer{decor}
    \node[bubble] at (x) ["$x$" above] {};
    \node[bubble] at (mx) ["$Mx$" right] {};

    \draw (x) edge [-Latex, bend left] (mx);

    % Pin / Label
    \draw[-{Kite}] (1.5, 1) node[above] {$r\Delta^N$}
      to[out = -90, in = 0] ($ {2}*(e1)!0.4!(e2)!0.1!(e3) $) {};
  \endpgfonlayer
\end{scope}
```

Thus, a stochastic matrix alters the coordinates in the standard simplex
but does not change in which simplex the vector is.
It's similar to how rotations alter directions but conserve in which concentric sphere a vector is.

:::Definition
A function $f : \R^N_{\ge 0} \to \R$ is $\Sto{N}$-invariant
if it is preserved by the action of all stochastic matrices, i.e.,

$$ \forall M \in \Sto{N},\, f(M x) = f(x).$$
:::

As we saw, the sum $\sumop$ is $\Sto{N}$-invariant.
Even more than that, it is a _universal invariant_,
in the sense that all other invariants are generated by it.
This will be extremely useful later on,
because it effectively says that $\Sto{N}$-invariant functions act as if they were unidimensional.

:::Theorem
A function $f : \R^N_{\ge 0} \to \R$ is $\Sto{N}$-invariant
if and only if it exclusively depends on the sum of the input's components.
That is, there exists a unique $\phi : \R_{\ge 0} \to \R$ such that
$f(x) = \phi(\sumop x)$.
:::

:::Proof
We prove the implications and uniqueness separately.

**$f = \phi \circ \sumop \implies f$ is $\Sto{N}$-invariant**:

  The function $f$ cannot "see" the changes $M \in \Sto{N}$ produces:

  $$ f(M x) = \phi(\sumop (M x)) = \phi(\sumop x) = f(x). $$

**$f$ is $\Sto{N}$-invariant $\implies f = \phi \circ \sumop$**:

We use the invariance to turn $f$ into a one-dimensional function
by constructing a stochastic matrix $A$ taking each simplex to a single coordinate.
You can think of this procedure as a $1$-norm version of rotating the coordinate system such $x$ aligns to an axis.

Concretely, we achieve this by taking an arbitrary $p \in \Delta^N$
and defining $A_p e_k = p$,
which amounts to the square matrix whose columns[^e1-visualize] are all $p$.

[^e1-visualize]: In case you find this hard to visualize, just set $p = e_1$.

$$
A_p = \left[
  \begin{array}{ccc}
    \rvert &        & \rvert\\
    p & \cdots & p \\
    \rvert &        & \rvert
  \end{array}
\right].
$$
This stochastic matrix accumulates the sum into the $p$-axis, $A_p x = (\sumop x) \cdot p$.
By invariance, $f$ is indifferent to collapsing the domain with $A_p$,

$$ f(x) = f(A_p x) = f( (\sumop x) \cdot p) = \phi_p(\sumop x)$$

Where we define $\boxed{\phi_p(t) = f(t \cdot e_1)}$.

**Uniqueness**:

Suppose $\phi \circ \sumop = f = \psi \circ \sumop$
and fix $p \in \Delta^N$ arbitrary.
We prove that $\phi$ and $\psi$ are extensionally equal.

$$ \phi(t) = \phi(\sumop (t p)) = f(t p) = \psi( \sumop (t p)) = \psi(t).$$
:::

This way, we see that being $\Sto{N}$-invariant constrains a function
to a rather simple form, because it is as if it could only depend on a single axis.
As we will shortly see,
this locally constant restriction also simplifies a lot
the possible invariant random variables.

Additionally, if you thought that the previous theorem reads a lot like a universal property,
you can bet you're right.
Here's the theorem recast as a commutative diagram of invariant functions.

```tikzcd
\R^N_{\ge 0} \ar[r, "f"] \ar[d, "\sumop"'] & \R \\
\R_{\ge 0}   \ar[ur, dashed, "\phi"']      &
```


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

$$ \frac{E}{\sumop E} \sim \Unif(\Delta^N).$$
:::

:::Proof
The components being positive guarantees that $\sumop E$ is also almost surely positive
and the projection $Z = E / \sumop E$ is well-defined.
We know that $Z$ is supported on the simplex because it is normalized.
Let's use the invariance to show that it is uniformly distributed.

Consider a region $A \subset \Delta^N$.
The projection is in $A$ if and only if the original variable $E$
lies in the cone $C_A$ of rays coming from the origin and crossing $A$ somewhere,

$$
\begin{aligned}
  \Prob\left(\frac{E}{\sumop E} \in A\right)
  &= \Prob\left(E \in \left\{\, r \sigma \mid r \in (0, +\infty), \sigma \in A \,\right\} \right).
\end{aligned}
$$

```tikz {tikzlibrary="shapes.geometric"}
\begin{scope} [scale = 2, xscale = 1.5, rotate around y=45,
    blend group = multiply,
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
    \filldraw[color = sgreen, draw = sgreen!80!black, opacity = 1] (e1) -- (e2) -- (e3) -- cycle;

  \coordinate (m) at (barycentric cs:e1=1,e2=1.3,e3=1);

  \node[heptagon, draw = black, fill = sgreen!50!sorange] (A) at (m) {};

  %% Build the cone
  {[every path/.style = {ultra thin, draw opacity = 1, fill opacity = 0.6},
    wall/.style = {left color=cyan!10, right color=cyan!40},
   ]
    % Top part
    \pgfonlayer{plane}
      \node[heptagon,
            draw,
            minimum width = 2cm,
            wall,
           ] (Ar) at ($ 2*(m) $) {};
    \endpgfonlayer

    % middle part
    \pgfonlayer{infront}
      \foreach \i in {1, 2, 6, 7}
        \draw (A.corner \i) -- (Ar.corner \i);

      \foreach \i in {1,...,5} {
        \pgfmathtruncatemacro{\j}{\i + 1}
        \shade[wall] (A.corner \i) -- (Ar.corner \i) -- (Ar.corner \j) -- (A.corner \j) -- cycle ;
      };

      \foreach \i in {3,...,5}
          \draw (A.corner \i) -- (Ar.corner \i);
    \endpgfonlayer

    % Back part
    \pgfonlayer{behind}
      \foreach \i in {1, 2,  6, 7}
        \draw (O) -- (A.corner \i);

      \foreach \i in {1,...,5} {
        \pgfmathtruncatemacro{\j}{\i + 1}
        \shade[wall] (O) -- (A.corner \i) -- (A.corner \j) -- cycle;
      };

      \foreach \i in {3,...,5}
          \draw (O) -- (A.corner \i);
    \endpgfonlayer
  }

  \pgfonlayer{decor}
    \node at (A.center)  [pin={[pin distance = 1.4cm, pin edge = {Stealth-, bend right}]-30:$\frac{\textstyle E}{\sumop E} \in A$}] {};
    \node at (Ar.center) [pin={[pin distance = 1cm, pin edge = {Stealth-, bend right}]60:$E \in C_A$}] {};
  \endpgfonlayer
\end{scope}
```

The distribution of $E$ is continuous,
so calculating the probability on the right amounts to your commonplace integral.
Also, from invariance,
its density $f_E$ only depends on the sum of components
(i.e., $f_E(x) = \phi(\sumop x)$).
Thus, a change of variables for barycentric coordinates $x = (\sumop x)\sigma$ seems like a good bet.

$$
\begin{aligned}
  \Prob\left(\frac{E}{\sumop E} \in A\right)
  &= \int_{C_A} \phi(\sumop x) \d{x} \\
  &= \int_0^\infty \int_A \phi(r) r^{n-1} \d\sigma \d{r} \\
  &= \Area(A) \underbrace{\int_0^\infty \phi(r) r^{n-1} \d{r}}_{\text{constant}}.
\end{aligned}
$$

Consequently, the distribution of $E / \sumop E$ is a multiple of the area element on the simplex,
defining a uniform measure.
:::

The previous theorem is a recipe for turning $\Sto{N}$-invariant distributions
into ones that are uniform on the simplex.
The only thing missing is to find a suitable distribution to project.
What could it be?
As it turns out,
a vector of i.i.d. [exponentially distributed random variables](https://en.wikipedia.org/wiki/Exponential_distribution)[^exp-rv] just cuts it.
I don't want to just postulate it, however.
We've come this far from first principles,
so let's make the exponentials appear by themselves.

[^exp-rv]: Continuous random variables with density $f(x) = \lambda e^{-\lambda x} \Id_{[0, \infty)}(x)$.

Any invariant distribution will do,
so let's go with the easiest kind: those with independent components.
What I find the most impressive is that the constraints of $\Sto{N}$-invariance and independence together
are strong enough to characterize exponential distributions.
You can think of it as an adaptation of [Maxwell's theorem](https://en.wikipedia.org/wiki/Maxwell's_theorem)[^se-maxwell]
for the simplex.

[^se-maxwell]: See [this link](https://math.stackexchange.com/questions/105418/very-elementary-proof-of-maxwells-theorem/105470#105470)
for the deduction of Maxwell's theorem from which inspired this proof.

:::Theorem
The only $\Sto{N}$-invariant absolutely continuous distributions on $\R^N_{\ge 0}$ (the non-negative cone)
with independent components are identically distributed exponentials $\sim \Exp(\lambda)$.
:::

:::Proof
We only consider absolutely continuous distributions supported on the positive orthant.
Hence, the distribution equals $p_Z = f \cdot \Id_{\ge 0}$ for an integrable probability density $f$.
Let's investigate this function.
By $\Sto{N}$-invariance, this density only depends on the sum of components
and, by independence, the joint density is a product of single-variable densities $f_i$,

$$ f(x) = \phi\left(\sumop x \right) = \prod_k f_i(x_k).$$

Let's turn it into a system of differential equations and solve for a closed form.
As we are working with distributions, there's no need to worry about smoothness right now
because all derivatives can be taken in a weak sense.

$$ \partial_i f(x) = \phi'\left(\sumop x\right) = f_i'(x_i) \prod_{k \ne i} f_k(x_k).$$

By dividing both sides by $f$, it becomes

$$ \frac{\phi'\left(\sumop x\right)}{\phi\left(\sumop x\right)} = \frac{f_i'(x_i)}{f_i(x_i)}.$$

The above works independently of $i$, meaning that the fractions $f_i'(x_i) / f_i(x_i)$ are all equal.
However, each depends on a different variable.
The only way this can happen is if they all _equal the same constant_.
Let's smartly call this constant $-\lambda$.

$$ \frac{f_i'}{f_i} = -\lambda \implies f_i' = -\lambda f_i.$$

Great! As the only weak solutions to a linear ordinary differential equation
are the classical ones,
the above proves that the densities are exponentials.

$$\boxed{f_i(x_i) = C_i e^{-\lambda x_i}}.$$

You can use that the $f_i$ are probability densities to deduce that $\lambda > 0$
and $C_i = \lambda$ are the only admissible constants.
The reasons are their tail has to go to zero and they must integrate to one.

$$ \begin{array}{rcl}
\lim_{t \to \infty} C_i e^{-\lambda t}                &= 0 &\iff \lambda > 0, \\
\int_0^\infty C_i e^{-\lambda t} \d{t} = \frac{C_i}{\lambda} &= 1 &\iff C_i = \lambda.
\end{array}$$

Therefore, for non-negative arguments, the joint density has the form

$$ p_Z(x) = \lambda^N e^{-\lambda \sumop x} \cdot \Id_{\ge 0} $$

Which characterizes a multivariate exponential with i.i.d. components.
:::

Among its many properties, the exponential distribution is famously
simple to generate from a uniform distribution on $[0, 1]$,
as a consequence of the [inverse transform sampling](https://en.wikipedia.org/wiki/Inverse_transform_sampling#):

$$ U \sim \Unif[0, 1] \implies -\frac{1}{\lambda}\log(U) \sim \Exp(\lambda).$$

Combining the above with this section's theorems,
we arrive at a way to sample uniformly from the simplex
using only $\Unif[0, 1]$ distributions --- which are available in any programming language worth its salt.
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
Assuming you have a type `Dist` for distributions, its just a mapping.

```julia
function sample_prob(xs)
  P = sample_simplex(length(xs))
  return Dist(xs[i] => P[i] for i in eachindex(xs))
end
```

Bonus: Using the Simplex to Sample Other Sets
=============================================

The previous section already wrapped up our main goals.
Nevertheless, it would be a shame to develop such a fine tool
to simply finish the post and go home.
Let's thus explore a couple examples of using $\Delta^N$
to uniformly sample from other interesting sets.
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

Drawing each component independently samples from it uniformly.

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

This whole post i deeply linked to the $1$-norm

$$ \norm{x}_1 = \sum_k \abs{x_k}.$$

The $1$-norm unit sphere is the set of all points
whose $1$-norm equals $1$,

$$ \partial B^N_1 = \{\, x \in \R^N \mid \norm{x}_1 = 1 \,\}.$$

This is a collage of identical simplexes, one for each orthant.
As their intersections have zero measure,
you sample uniformly from the sphere by drawing a point in the simplex
and throwing $N$ coins to decide the component's sign.

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
First, assign a triangulation to the polyhedron $K$.
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
I hope you had fun exploring how a seemingly strange problem in statistics
could be reduced to a geometrical problem about symmetries.
I know that I did.

To wrap up, I should point out the similarity between today's derivation
the and Herschel-Maxwell's theorem for rotation-invariant distributions.
In particular, because the sum equals the $L^1$ norm in the first quadrant.
Does this mean something deeper? I don't really know, but I bet so.
What I know is that one can sample from any $p$-norm sphere [@barthe_probabilistic_2005] using a distribution
proportional to $e^{-\norm{x}_p^p}$.
The problem is that finding good transformations preserving general $p$-norms is not so direct.
To be fair, I can't think of anything but permutations.
That's a topic for a future post, perhaps.

Farewell and good sampling for y'all!

Acknowledgements
================

Many thanks to Ivani Ivanova for her comments and effort proofreading this post.
