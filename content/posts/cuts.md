---
title: Approximation by a thousand cuts
subtitle: A guide to piecewise-linear approximations
keywords: [math]
date: 2023-09-08
header-includes:
- '<script src="https://cdn.jsdelivr.net/npm/d3@7"></script>'
---

<style>
/* CSS for styling */
svg.diagram {
  border: 1px solid #ccc;
}

.convex-set {
  fill: hsl(147 42% 64%);
  stroke: black;
  opacity: 1;
  transition: fill 100ms;
}

.not-good {
  fill: #de82a2;
}

.hyperplane {
  stroke: orange;
  stroke-width: 1pt;
}

.half-space {
  fill: rgba(255, 165, 0, 0.5);
}

.function-graph {
  stroke: black;
  fill: none;
}

.epigraph {
  fill: steelblue;
  fill-opacity: 0.3;
  transition: fill 400ms;
}

.epigraph:hover {
  fill: blue;;
}
</style>

\def\R{\mathbb{R}}
\def\hole\cdot
\def\inner<#1,#2>{\left\langle#1,\,#2\right\rangle}
\def\op#1{\operatorname{\mathrm{#1}}}
\def\graph{\op{graph}}
\def\epi{\op{epi}}
\def\d#1{\op{d}\!{#1}}

Oh, a friend just called you and started to talk about
this amazing function $f : \R^n \to \R$ that he just found.
It is well-behaved, fits perfectly with your current work
and has all the good characteristics you can imagine.
Excited, you ask see such an impressive function.
But there is a problem, describing an arbitrary function
requires an infinite amount of information and any of our modern ways to send messages,
such as email, disks, or carrier pigeon,
can only transmit a finite number of bits.

What to do in such a situation?
If it was a polynomial, it would be easy: all you would need are the coefficients.
The same applies if it was a finite domain instead of $\R^n$.
Even for elementary functions, one can transmit a syntax tree
that completely represents the function.
Those all need some finite amount of information to represent.[^real-numbers-size]
In the general case, however, the solution is to somehow approximate
the function using something storable
and then invoke any of the magical theorems from analysis
that guarantee that the error in your approximation is less
than the smallest $\epsilon > 0$ that you may care about.

[^real-numbers-size]: This all assumes that you can somehow store
a single real number with finite information.
But this is not today's subject, so I'm just going to gloss over it.

Mathematicians all over the world have already discovered a myriad of methods
for performing such approximations.
For example, if all you can do is evaluate $f$,
an option is to interpolate and represent it as a polynomial.
Or, if you are feeling fancier, you could treat these evaluations as a dataset
and train a neural network that approximates the function.
Every method has its pros and cons,
depending on what properties of the function you want to preserve.
In today's post we will focus on one such method called _approximation by cuts_ or _cutting planes_.

<svg width="800" height="200">
  <rect width="800" height="200" style="fill: rgb(200,200,200);stroke-width: 3;stroke: rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
  TODO: Interactive figure with figure and cuts
  </text>
</svg>

<div>
  <svg id="function-cuts" class="diagram" viewBox="0 0 750 400" width="750" height="400">
  </svg>
</div>


Approximation by cuts
=====================

Cutting planes have widespread usage on the Operations Research community
because they play really well with optimization problems.
Nevertheless, I unfortunately don't see them being used quite as much on other branch
of engineering or mathematics.
Thus, before talking about why they are so cool,
let's first present tonight's star.

:::Definition
A **cut** for $f : \R^n \to \R$ is an affine function everywhere below $x$.
That is,

$$  f(x) \ge \inner<a, x> + b. $$
:::

In particular, we say that a cut is _tight_ at $x_0$
if it equals the function at this point.
In this case, it is common to denote the cut as centered around $x_0$:

$$  f(x) \ge f(x_0) + \inner<a, x - x_0>. $$


```tikz
{ [domain=-1.8:2, xscale=1.8]
  \draw[very thin, opacity=0.2, color=gray, path fading=west] (-2.9,-1.9) grid (2.9,1.9);

  % Function
  \draw[color=black, samples=100]  plot (\x,{0.5*(\x - 1.5)*(\x - 1)*(\x + 0.5)*(\x + 1.5)})
  node[right] {function};
  % Common cut
  \draw[color=orange] plot (\x,{-1          + 0.403875*(\x - 1.35)})
  node[right] {ordinary cut};
  % Tight cut
  \draw[color=blue]   plot (\x,{-0.13803125 + 0.403875*(\x - 1.35)})
  node[right] {tight cut};
  % Point of tangency
  \node[fill=blue, draw, circle, inner sep = 0.5pt] at (1.35,-0.13803125) {};
}
```

In general, a single cut is a terrible approximation.
But it is not its fault, after all, it is just a first order polynomial!
The magic begins when you combine a lot of cuts to represent $f$ as a _polyhedral function_.
Taking an approximation of $f$ by cuts $C = \{\inner<a,\hole> + b\}$
amounts to choosing the largest cut at each given point:

$$ f(x) \approx \max\limits_{(a,b) \in C} \inner<a_,x> + b. $$

This formula also explains why we refer to affine functions as "cuts".
Think about the graph of $f$.
The graph of a cut is a hyperplane dividing the space in two halves:
points above and below it.
To form the polyhedral function, we slice the space, one cut at a time,
in order to carve a polyhedron containing the graph of $f$.

Click anywhere in the figure below to carve the shape of a function using cuts.

<div>
  <svg id="function-epigraph-carving" class="diagram" viewBox="0 0 750 400" width="750" height="400">
  </svg>
</div>

Why I like cuts and you should too
----------------------------------

What are the nice properties that an approximation by cuts has?
Why is it used so much by the optimization and operations research folks?
Let's go through some of the reasons.

- **Convexity**: 
    Every polyhedral function is convex,
    and convex functions are full of nice properties needed for optimization.
    Furthermore, one can prove that
    as you add more cuts any such approximation converges
    to the tightest convex function everywhere below $f$.

    I also find it worth commenting that this property works as a double-edged sword:
    since polyhedral functions are always convex,
    you cannot guarantee a satisfactory approximation for a non-convex function.
    Keep this in mind when using cuts.

- **Underapproximation**:
    Since all cuts are below $f$, their maximum also has to be.
    Thus, cutting planes are a good tool for estimating lower bounds,
    which is a necessity in optimization.

- **Easy to improve**:
    Approximations by cuts can be refined without much hassle.
    Whenever you obtain a new cut $\phi$, all you have to do is update your bag of cuts
    from $C$ to $C \cup \{\phi\}$, and it's done.
    This allows for algorithms based on iterative refinement of the approximation.

- **Linear Programming Formulation**:
    Minimizing a polyhedral functions can be represented as a
    [Linear Program](https://en.wikipedia.org/wiki/Linear_programming),
    which is among the easiest kinds of problems to optimize.
    So if $f$ is some complicated convex function, but you already have
    a good approximation by cuts for it, you can minimize it instead
    and get an approximation for the optimum.

    $$
    \begin{aligned}
    \min_x f(x) &\ge
      \begin{array}{rl}
        \min\limits_{x} &\max\limits_{(a,b) \in C} \inner<a_,x> + b  \\
      \end{array} \\
      &=
      \begin{array}{rl}
        \min\limits_{x, t} & t \\
        \textrm{s.t.}  & \inner<a_,x> + b \le t,\, \forall (a,b) \in C  \\
      \end{array}
    \end{aligned}
    $$

These properties make cuts an excellent tool for estimating lower bounds on optimization problems,
which is a necessity for any solving method that "sandwiches" the optimal value
between two bounds.
Let's see an example of how to do that.

<svg width="800" height="200">
  <rect width="800" height="200" style="fill:rgb(200,200,200);stroke-width:3;stroke:rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
    TODO: Interactive solution via cutting planes
    The user selects a point and the algorithm updates the bounds.
  </text>
</svg>

### Example: Kelley's cutting planes algorithm {#example-sandwich-algorithm}

Let's now put all these properties to good use.
Imagine that you must minimize a convex function,
but, unfortunately,
you only have access to a linear programming solver such a Gurobi, Xpress or GLPK.
How do you do that?

Fortunately, one can minimize an arbitrary convex function
by iteratively solving linear programs.
For it to work, we will have to suppose (by now)
that we have access to some oracle capable of both evaluating a function and calculating a tight cut for it.
This is not cheating though,
because in section [cuts-from-derivatives] we will learn how to write such an oracle.

We begin with a constant cut approximation[^unconstrained-lp] $\tilde{f}_0$ of $f$
everywhere equal to a initial lower bound.
The idea is to, at each iteration, give the oracle a point $x_n$.
This will return an evaluation $f(x_n)$, which serves as an upper bound
for $\min f$ because all evaluations are larger than the minimum.
You also get a tight cut $c_n$ that can be used to improve your approximation to $\tilde{f}_n$.
This way, we squeeze the optimal value between a sequence of bounds:

$$ \min_n f(x_n) \ge \min_x f(x) \ge \min_x \tilde{f}_n(x). $$

The algorithm can also be more directly described in Julia code:

```julia
function minimize(f::Function, initial_bound ; eps, x = zeros())
  opt = Polyhedral(initial_bound)
  ub  = +Inf
  lb  = initial_bound

  while abs(ub - lb) > eps
    fx, cut = oracle(f, x)  # Ask oracle for evaluation and cut at x

    # Improve upper bound
    ub = min(ub, f(x))

    # Improve lower bound
    add_cut!(opt, cut)
    x, lb = solve_lp(opt)   # Also updates the best solution
  end

  return x, fx              # Optimal point and optimal value
end
```

<svg width="800" height="200">
  <rect width="800" height="200" style="fill:rgb(200,200,200);stroke-width:3;stroke:rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
    TODO: Animation of solution via this algorithm
  </text>
</svg>

[^unconstrained-lp]: One way to think of it is as $f(x) = -\infty$.

In practice there are better ways to do unconstrained optimization,
but this idea of sandwiching with evaluations and cuts
works as a building block to more complex optimization algorithms
such as [Benders' decomposition](https://en.wikipedia.org/wiki/Benders_decomposition)
or Stochastic Dual Dynamic Programming.


A bit of convex geometry
========================

So, where does this idea of cuts comes from?
Did some mathematician in the fifties cut his finger while preparing lunch
and suddenly became illuminated?
Well, in fact, the idea of cutting planes is pretty central for convex analysis.
Let's thus make a small detour into the world of convex sets
to see how cuts arise.

We will only go over the necessary results in this post.
For a more comprehensive overview of convex geometry (or for actually learning it)
I recommend the great book by @{boyd_convex_2004} [chapter 2].
Alright, time for some definitions.

:::Definition
A set $X$ is **convex** if, given two points $x, y \in X$, it also contains
the entire line segment between them.

Algebraically, for any $\lambda \in [0, 1]$,
$$ \lambda x + (1-\lambda) y \in X.$$
:::

My intuition for convex sets is that they are sets
without holes nor wobbles.
This encompasses a lot of the good ol' shapes you are familiar with,
such as planes, balls, and polyhedra.

```tikz
\coordinate (M) at (4.25,1);
\coordinate (x) at (1,0);
\coordinate (y) at (2, 0.7);

\pgfmathsetmacro{\slope}{atan2(2,-4.5)}

\draw[fill=orange!50] (M) --
  ++ (-2, 0.5)
  to[out=\slope+10,in=90] ++ (-2, -2.5)
  to[out=-90,in=\slope+180] cycle;

\draw[thick, fill=black]
  (x) circle[radius=0.03] node[below]{x} --
  (y) circle[radius=0.03] node[above]{y};

\path[-Latex, semithick, color=black] (M) edge[bend left, shorten <= 7pt] node[pos=1.1] {Convex Set} +(2, -1);
 ```

```tikz
\coordinate (A) at (0,2);
\coordinate (B) at (4.5,0);
\coordinate (C) at (0,-2);
\coordinate (M) at (0, 0);


\coordinate (x) at (-3,-3);
\coordinate (y) at (-2, -0.5);

\pgfmathsetmacro{\slope}{atan2(2,-4.5)}

\draw[fill=yellow!30] (M)
  to[out=\slope,in=0] ++ (-2,0.5)
  to[out=180,in=90] ++ (-1,-0.5)
  to[out=-90,in=90] ++ (1,-2)
  to[out=-90,in=90] ++ (-2,-1)
  to[out=-90,in=180] ++(4,-1)
  to[out=0,in=\slope+180] cycle;

\filldraw[fill=white] (C) circle [rotate=30, x radius=0.5, y radius = 0.4];

% Draw points
\draw[thick, fill=red, color=red]
  (x) circle[radius=0.03] node[below, color=black]{$x$} --
  (y) circle[radius=0.03] node[above, color=black]{$y$};

\path[-Latex, semithick, color=black] (M) edge[bend right, shorten <= 7pt] node[pos=1.1] {Non-convex Set} +(2, 1);
```

A consequence of their regular shape is that they can be separated by hyperplanes.
That is, given two disjoint convex sets, it is always possible
to divide the space such that each of them lies in one of the halves.[^hahn-banach]

::: {.Theorem data-title="Separating Hyperplane"}
For any pair of disjoint non-empty convex sets $X$, $Y$,
there is an affine function $\inner<a,\hole> + b$ that is non-negative on one of them
and non-positive on the other one.

That is, for all $x \in X$ and $y \in Y$,
$$\inner<a,x> + b \le 0 \quad \text{and}\quad \inner<a,y> + b \ge 0.$$
:::

The proof to this result is more technical than elucidating,
so we are going to skip it on this post.
You can find a great presentation with all details on @{boyd_convex_2004}'s book.
Fortunately, this is one of these results were we can get a good intuition from a picture.
Due to their absence of wobbles, one can pass a plane between two convex sets without bumping into anything.

[^hahn-banach]: For the Functional Analysts lurking around: in infinite dimension,
this theorem is equivalent to the Hahn-Banach Theorem.

<div>
  <svg id="set-separating-hyperplane" class="diagram" viewBox="0 0 750 400" width="750" height="400">
  </svg>
</div>

Let's focus now on the case where one of the sets is a single point.
An interesting consequence of this theorem is that given any point $p$ outside of the set,
we can find an affine function that is zero in this point and contains
a convex set in one of its half-spaces.
It is really similar to a cut, but for a set.
For example, in the imagine below, you can choose a separating hyperplane between
your mouse and the convex set in the middle.

<div>
  <svg id="set-point-hyperplane" class="diagram" viewBox="0 0 750 400" width="750" height="400">
  </svg>
</div>


Now, what happens when we choose our point to be in the set's boundary?
In this case, we acquire a _supporting hyperplane_.
That is, a tangent hyperplane that has the entire set in one of its sides.

:::Definition
A **supporting hyperplane** at a point $x$ in the boundary of $X$
is a tangent hyperplane that touches $X$ at $x$ and is
non-negative at every other point of $X$.
:::

This start to look a bit like cuts, right?
Very well, from the Separating Hyperplane Theorem,
we can conclude a similar theorem saying that convex sets
have supporting hyperplanes at all points in their boundary.

::: {.Theorem data-title="Supporting Hyperplane"}
A convex set $X$ has a supporting hyperplane
for any point $x_0$ on its boundary.
That is, there is an affine function $\inner<a,\hole> + b$
such that

$$\inner<a,x_0> + b = 0 \quad \text{and}\quad \forall x \in X,\, \inner<a,x> + b \ge 0.$$
:::

:::Proof
If $X$ has nonempty interior, there is a hyperplane separating
$\{x_0\}$ from the interior of $X$,
and if it is empty, then the entirety of $X$ lies inside in an affine subspace of $\R^n$,
and we can take this subspace as our hyperplane.
:::

Notice that this theorem also has the equivalent formulation as
$$\forall x \in X,\, \inner<a,x> \ge \inner<a,x_0>.$$

<div>
  <svg id="set-supporting-hyperplane" class="diagram" width="750" height="400">
  </svg>
</div>

From convex sets to convex functions
====================================

After our detour on convex sets,
it's time to investigate how to convert these results about sets and hyperplanes
into results about functions and cuts.
After all, that is what we are interested in here.

Foremost, one remark: for simplicity of presentation we will work with
_extended real functions_.
These are functions that, besides the usual real numbers, can evaluate to $\infty$.
The advantage of this approach is that we can just
assume that all functions are defined everywhere on $\R^n$.
We call their _domain_ wherever they are finite
and define them as infinity elsewhere.

The most common definition in the wild for a _convex function_
is through _Jensen's inequality_,

$$ f(\lambda x + (1 - \lambda) y  \le \lambda f(x) + (1 - \lambda) f(y).$$

Nonetheless, convex functions are notorious for having a multitude of equivalent definitions.
Hence, for our purposes it will be better to choose a more geometric one.

:::Definition
A function is **convex** if its epigraph is a convex set.
:::

But what is this _epigraph_ thing after all?
Well, everybody knows a function's graph,

$$ \graph(f) = \left\{ (x, y) \in \R^{n+1} \mid f(x) = y \right\}. $$

The epigraph adjoins the suffix epi-, from greek ['ἐπί']{lang=grc}
meaning ["above" or "on top of"](https://outils.biblissima.fr/fr/eulexis-web/?lemma=%E1%BC%90%CF%80%CE%AF&dict=LSJ),
to denote the set of all points _above the graph_.

$$ \epi(f) = \left\{ (x, y) \in \R^{n+1} \mid f(x) \le y \right\}. $$

<div>
  <svg id="function-epigraph" class="diagram" width="750" height="400">
  </svg>
</div>

Besides sharing an etymology with _epic_,
the epigraph also lets us easily translate results from convex sets to convex functions.
For example, the supporting hyperplane theorem translates into a statement about tight cuts for convex function!

:::Theorem
A convex function $f$ has a tight cut at any point in its domain.
:::

Visually this theorem looks like the figure below.
You can hover it to view the cut for each point.

<div>
  <svg id="function-supporting-cut" class="diagram" width="750" height="400">
  </svg>
</div>

:::Proof
The graph of $f$ lies in the boundary of its epigraph, which is convex.
Then, from the supporting hyperplane theorem, for any $x_0$,
there is an affine function supporting $\epi(f)$ at $(x_0, f(x_0))$.
Since the tangent to a graph is non-vertical,
the last component of this graph cannot be zero,
and we can assume without loss of generality that it equals $1$.
Then,

$$ \begin{aligned}
  \inner<(a,1), (x, f(x))> &\ge \inner<(a,1), (x_0, f(x_0))> \\
  \inner<a,x> + f(x) &\ge \inner<a,x_0> + f(x_0) \\
  f(x) &\ge f(x_0) - \inner<a,x - x_0>
\end{aligned} $$

By defining $\mu = -a$, we arrive at

$$f(x) \ge f(x_0) + \inner<\mu,x - x_0>.$$
:::

With this theorem we finally achieved all we need to be able
to calculate, not just postulate, cuts for a large set of functions.

Cuts from derivatives
---------------------

Cuts are a special case of tangent hyperplane
which does not cross the functions graph anywhere.
This means that they have everything to do with derivatives.
Indeed, we just arrived into our first method for calculating cuts.

Suppose $f$ is convex and differentiable at $x_0$.
From the convexity it has a tight cut with inclination $\mu$

$$f(x) \ge f(x_0) + \inner<\mu,x - x_0>.$$

This cut is a tangent hyperplane at $x_0$,
and, from differentiability,
this hyperplane is unique with inclination given by the derivative.
Therefore

$$\boxed{\forall x,\, f(x) \ge f(x_0) + \inner<\d{f}(x_0),x - x_0>.}$$

Now that's a formula!
Recall our [sandwich algorithm](example-sandwich-algorithm).
There I asked you to take in good faith my words about our oracle,
and now
It's finally time to pay my debt and turn that black box into a simple procedure.

All we need is a `derivative` Julia function that calculates
the derivative of `f` at a point `x`.
There are lots of packages which do that using either automatic or numerical differentiation.
I'll just let you choose your favorite.
Our oracle then becomes a simple function that assembles the cut.

```julia
function oracle(f, x)
  fx  = f(x)
  dfx = derivative(f, x)

  return fx, Cut(dfx, fx - dot(dfx, x))
end
```

Cuts from Lagrangian duality
----------------------------

We have learned how to calculate cuts for a convex function at any point of differentiability.
Nevertheless, the supporting hyperplane theorem guarantees a cut
at _every point_ on the function's domain.
How can we also calculate cuts where $f$ is not differentiable?

If you are coming from an area of mathematics where everything is smooth,
this last question may sound strange.
Nevertheless, in operations research and optimization,
it is rather common to stumble into non-smooth functions.
Take the polyhedral functions, for example.
They almost always have edges and corners,
nonetheless, it should be easy to calculate cuts for them.
They are made out of linear pieces, after all!

Our focus on this section will be on calculating cuts for _optimal value functions_,
that is, functions defined as the optimal value of a parameterized optimization,
such as those one [would encounter, for example, in dynamic programming](/posts/dynamic-programming).

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X.
  \end{array}
$$

This last definition may, at first, seem to be too restrictive.
Perhaps even more than requiring differentiability.
There is a small, trick, however, that lets us write any function
as an optimal value function:

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u} & f(u) \\
    \textrm{s.t.}   & u = x.
  \end{array}
$$

If this feels like cheating, it's because it certainly is!
In practice, exchanging evaluation by optimization may be rather bad for performance.
From a theoretical point-of-view, however,
this representation always works and we are all safe.

The advantage of concentrating in optimal value functions
is that it opens to us a whole world of tools from Optimization.
In particular, we will see that [Lagrangian duality](https://en.wikipedia.org/wiki/Duality_(optimization))
has everything to do with cuts.[^duality-refs]

[^duality-refs]: For an overview of duality, you can (again) check @boyd_convex_2004.
And for duality in the context of optimal value functions, there is also my M.Sc. thesis. [@leal_de_freitas_convexification_2019]

Lagrangian duality consists of taking a hard constraint such as $(x, u) \in X$
and substituting it by a linear penalty on how much we divert from the original feasible set.

We thus go from this optimal value function,
written with an additional equality for clarity,

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u} & c(u) \\
    \textrm{s.t.}   & (y, u) \in X, \\
                    & y = x, \\
  \end{array}
$$

To the penalized value function, called its _Lagrangian relaxation_,

$$
  \begin{array}{rl}
    L(x; \lambda) = \min\limits_{u, y} & c(u) - \inner<\lambda, y - x> \\
    \textrm{s.t.}   & (y, u) \in X.
  \end{array}
$$

Notice that while in the original problem we had $y$ fixed to be whatever
argument the function receives,
in the new problem $y$ can take any feasible value,
but the cost is augmented by a new penalty term on the difference between $y$ and $x$.
The factor lambda is called a _dual variable_ or _Lagrange multiplier_
and represents how much $y$ is averse to diverting from $x$.

In the original feasible set where $x=y$,
the term $\inner<\lambda, y - x>$ vanishes and both problems have the same objective function.
However, the relaxed problem has a larger feasible region to explore
in its search for the minimum, allowing it the opportunity to achieve lower costs.
In other words, it is always below the primal:

$$ f(x) \ge L(x; \lambda). $$

<svg width="800" height="200">
  <rect width="800" height="200" style="fill:rgb(200,200,200);stroke-width:3;stroke:rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
    TODO: Function and Lagrangian for many lambdas (or perhaps interactive)
  </text>
</svg>

In forming the relaxation, one has freedom on which $\lambda$ to use.
Essentially all state-of-the-art algorithms in (continuous) optimization[^market-opt]
solve at the same time both the original problem
and the problem of choosing the best multiplier possible.
What do I mean by the best multiplier?
Well, it's the one who closes the gap the most!
The optimal value of choosing the multiplier is defined as


$$
\begin{aligned}
  \check{f}(x) &= \max\limits_{\lambda} L(x ; \lambda) \\
               &= \max\limits_{\lambda} \min\limits_{(y, u) \in X} c(u) - \inner<\lambda, y - x>.
\end{aligned}
$$

That thing looks scary but, fortunately for us,
all solvers are pretty good at it.
The $\check{f}$ is called the dual value function
because it solves the dual problem.
And since all relaxations are below $f(x)$, it also is.
A result known as _weak duality_.

$$ \boxed{f(x) \ge \check{f}(x)} $$

[^market-opt]: And consequently all free or commercial solvers in the market.

<svg width="800" height="200">
  <rect width="800" height="200" style="fill:rgb(200,200,200);stroke-width:3;stroke:rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
    TODO: Function and Lagrangian for many lambdas and dual function
  </text>
</svg>

Since $\check{f}$ is defined as the maximum of a bunch of affine functions (affine in $\lambda$),
it is convex!
Meaning that it has a cut at every point.
Moreover, one can prove that it is the tightest convex function everywhere below $f$.
So the cuts for $\check{f}$ are also the best ones possible for $f$.
The only thing remaining is discovering how to actually calculate these cuts.
As it stands out, they come for free from the optimization problem,
because their inclination is precisely the optimal multiplier.

:::Theorem
There is a cut for $f$ at the point $x_0$ defined by the dual value $\check{f}(x_0)$
and optimal dual multiplier $\lambda_0$. That is,

$$ \forall x, f(x) \ge \check{f}(x_0) + \inner<\lambda_0, x - x_0>.$$
:::

:::Proof
By definition,
$$ \check{f}(x_0) = \max\limits_{\lambda} \min\limits_{(y, u) \in X} c(u) - \inner<\lambda, y - x_0>. $$

The maximum in the equation above is achieved precisely by $\lambda_0$:

$$ \check{f}(x_0) = \min\limits_{(y, u) \in X} c(u) - \inner<\lambda_0, y - x_0>. $$

The equation above has a minimum, hence it has to be below
any feasible choice of $(y, u)$ that we make.
In particular, we can fix $y$ equal to our desired $x$.

$$
  \begin{array}{rl}
    \check{f}(x_0) \le \min\limits_{u} & c(u) - \inner<\lambda_0, x - x_0> \\
    \textrm{s.t.}   & (x, u) \in X.
  \end{array}
$$

Since the affine term in the objective function does not depend upon $u$,
we can take it out of the minimization and pass it to the left-hand side.

$$
  \begin{array}{rl}
    \check{f}(x_0) + \inner<\lambda_0, x - x_0>
      \le \min\limits_{u} & c(u) \\
          \textrm{s.t.}   & (x, u) \in X.
  \end{array}
$$

Now the right-hand side equals exactly the definition of $f(x)$.
This concludes the theorem.
:::

Perfect! We just discovered that the solutions to the dual problem
are cuts for the primal problem.
And, best of all, solving an optimization problem in general
already provides us with the dual solutions, meaning that we get cuts for free.
How cool is that?


Of course, now is the time for you to point your finger towards me
and say that there is no tightness guarantee in the previous theorem.
Although the dual is the best relaxation possible,
it could still be too below the primal value function.
But remember that we discussed above that $\check{f}$ is the _tighest_ convex function
approximating $f$ from below.
Thus, they should be equal whenever $f$ is convex.
This is an important result called _strong duality_,
which states that $\check{f}$'s epigraph is the closed convex hull of $f$'s epigraph.[^lsc-cvx]
Therefore, now we also know how to calculate tight cuts
for the points of non-differentiability of a convex function!

$$ f\;\text{convex} \implies \forall x, f(x) \ge f(x_0) + \inner<\lambda_0, x - x_0>.$$

[^lsc-cvx]: Technically, the theorem requires $f$ to be  proper convex _lower semicontinuous_ function.
But in practice, convexity tends to be the only part you need to worry about.

As a bonus, if you like automatic differentiation as much as I do,
you also just learnt how to differentiate procedures defined as convex optimization problems.
Suppose that $f$ is differentiable at $x_0$.
Since the derivative is unique and the cut provides us with a tangent,
it must follow that the derivative equals the optimal multiplier $\nabla f(x_0) = \lambda_0$.
Thus, by solving a parameterized (convex) optimization problem,
you gain both an evaluation and a derivative.
Now all you have to do is plug it into the chain rule et voilà!

<script src="./convex-support.js"></script>
<script>
  figureFunctionCuts("#function-cuts", x => x*x+1, x => 2*x, -2, 2);

  figureFunctionEpigraphCarving("#function-epigraph-carving", x => x*x+1, x => 2*x, -2, 2);

  figureSetSeparatingHyperplane("#set-separating-hyperplane");

  figureSetPointHyperplane("#set-point-hyperplane");

  figureSetSupportingHyperplane("#set-supporting-hyperplane");

  figureFunctionEpigraph("#function-epigraph", x => 0.5*(x - 1.5)*(x - 1)*(x + 0.5)*(x + 1.5), -1.8, 2);

  figureFunctionSupportingCut("#function-supporting-cut", x => x*x+1, x => 2*x, -2, 2);
</script>
