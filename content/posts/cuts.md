---
title: Approximation by a thousand cuts
subtitle: A guide to piecewise-linear approximations
keywords: [math]
date: 2023-09-08
---

\def\R{\mathbb{R}}
\def\hole\cdot
\def\inner<#1,#2>{\langle#1,\,#2\rangle}

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
In today's post we will focus on one such method called **approximation by cuts** or **cutting planes**.

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


<svg width="800" height="200">
  <rect width="800" height="200" style="fill:rgb(200,200,200);stroke-width:3;stroke:rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
    TODO: FIGURE with f and a cut.
  </text>
</svg>

In general, a single cut is a terrible approximation.
But it is not its fault, after all, it is just a first order polynomial!
The magic begins when you combine a lot of cuts to represent $f$ as a **piecewise-linear function**.
Taking an approximation of $f$ by cuts $C = \{\inner<a,\hole> + b\}$
amounts to choosing the largest cut at each given point:

$$ f(x) \approx \max\limits_{(a,b) \in C} \inner<a_,x> + b. $$

This formula also explains why we refer to affine functions as "cuts".
Think about the graph of $f$.
The graph of a cut is a hyperplane diving the space in two halves:
points above and below it.
You can think of the process of maxing out the cuts
as slicing the space to form a region containing the graph of $f$.
Below we see an illustration of this view.

<svg width="800" height="200">
  <rect width="800" height="200" style="fill:rgb(200,200,200);stroke-width:3;stroke:rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
    TODO: Interactive figure with figure and cuts
  </text>
</svg>

Why I like cuts and you should too
----------------------------------

What are the nice properties that an approximation by cuts has?
Why is it used so much by the optimization and operations research folks?

The first reason is that _it preserves convexity_,
and convex functions are full of nice properties needed when doing optimization.
Even more than that! As we will [later see](#milp),
as you add more cuts this approximation converges
to the tightest convex function everywhere below $f$.

It is also an underapproximation of $f$,
because since all cuts are below it, their maximum also has to be.
Thus, cutting planes are a good tool for estimating lower bound,
which is a necessity in optimization.

Another reason is that minimizing cuts can be transformed into a
[Linear Program](https://en.wikipedia.org/wiki/Linear_programming),
which is the among the easiest kinds of problems to optimize.
So if $f$ is some complicated convex function, but you already have
a good approximation by cuts to it, you can minimize it instead
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

This last two properties make cuts an excellent tool for estimating lower bounds on optimization problems,
which is a necessity for any solving method that "sandwiches" the optimal value
between two bounds.

<svg width="800" height="200">
  <rect width="800" height="200" style="fill:rgb(200,200,200);stroke-width:3;stroke:rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
    TODO: Figure illustrating solving via cutting planes
  </text>
</svg>

Finally, a nice property is that an approximation by cuts can be refined
without much hassle.
If you get a new cut $\phi$, all you have to do is update your bag of cuts
from $C$ to $C \cup \{\phi\}$, and it's done.
This allows for algorithms based on iterative refinement of the approximation.

### Example: The cutting sandwich algorithm for optimization

Let's now put all these properties to good use.
We will devise a simple algorithm for unconstrained minimization
of an arbitrary convex function.
For it to work, we will have to suppose by now
that we have access some oracle capable of both evaluating a function and calculating a tight cut for it.
This is not cheating though,
because in section [AD] we will learn general methods to write such an oracle.

We begin with an empty unconstrained cut approximation[^unconstrained-lp] $\tilde{f}_0$ of $f$.
The idea is to, at each iteration, give the oracle a point $x_n$.
This will return an evaluation $f(x_n)$, which serves as an upper bound
for $\min f$ because all evaluations are above the minimum.
You also get a tight cut $c_n$ that can be used to improve your approximation to $\tilde{f}_n$.
This way, we squeeze the optimal value between a sequence of bounds:

$$ \min_n f(x_n) \ge \min_x f(x) \ge \min_x \tilde{f}_n(x). $$

The algorithm can also be more directly describe in Julia code:

```julia
function minimize(f::Function ; eps, x = zeros())
  opt = CutApproximation()
  f_lb, f_ub = -Inf, +Inf

  while abs(f_ub - f_lb) > eps
    fx, cut = oracle(f, x)     # Ask oracle for evaluation and cut at x

    # Improve upper bound
    f_ub = min(f_ub, f(x))

    # Improve lower bound
    add_cut!(opt, cut)
    x, f_lb = solve_lp(opt)
  end

  return x, fx      # Optimal point and optimal value
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
A set $X$ is convex if, given two points $x, y \in X$, it also contains
the entire line segment between them.
Algebraically, for any $\lambda \in [0, 1],

$$ \lambda x + (1-\lambda) y \in X.
:::

<svg width="800" height="200">
  <rect width="800" height="200" style="fill:rgb(200,200,200);stroke-width:3;stroke:rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
    TODO: Illustrate convex vs non-convex set
  </text>
</svg>

My intuition for convex sets is that they are sets
without holes nor wobbles.
This encompasses a lot of the good ol' shapes you are familiar with,
such as planes, balls, and polyhedra.

A consequence of this regular shape is that they can be separated
by hyperplanes.
That is, given two disjoint convex sets, one can always divide the space
such that each of them lies in one of the halves.[^hahn-banach]


:::Theorem
(Separating Hyperplane) For any pair of disjoint non-empty convex sets $X$, $Y$,
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

<svg width="800" height="200">
  <rect width="800" height="200" style="fill:rgb(200,200,200);stroke-width:3;stroke:rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
    TODO: Separating hyperplane (perhaps interactive)
  </text>
</svg>

The consequence of most interest to us is that this theorem can be used
to acquire **supporting hyperplanes** for any convex set.

:::Definition
A **supporting hyperplane** at a point $x$ in the boundary of $X$
is a tangent hyperplane that is touches $X$ at $x$ and his
non-negative at every other point of $X$.
:::

<svg width="800" height="200">
  <rect width="800" height="200" style="fill:rgb(200,200,200);stroke-width:3;stroke:rgb(0,0,0)" />
  <text x="50" y="100" length="800" rx="20" ry="10">
    TODO: Interactive supporting hyperplane (point => hyperplane)
  </text>
</svg>

This start to look a bit like cuts, right?
Very well, from the Separating Hyperplane Theorem,
we can conclude a similar theorem saying that convex sets
have supporting hyperplanes at all points in their boundary.

:::Theorem
(Supporting Hyperplane) A convex set $X$ has a supporting hyperplane
for any point $x_0$ on its boundary.
That is, there is an affine function $\inner<a,\hole> + b$
such that

$$\inner<a,x_0> + b = 0 \quad \text{and}\quad \forall x \in X,\, \inner<a,x> + b \ge 0.$$
:::

:::Proof
If $X$ has nonempty interior, there is an hyperplane separating
$\{x_0\}$ from the interior of $X$,
and if it is empty, then the entirety of $X$ lies inside in an affine subspace of $\R^n$,
and we can take this subspace as our hyperplane.
:::

From convex sets to convex functions
====================================

After our detour on convex sets,
it's time to investigate how to convert these results about sets and hyperplanes
into results about functions and cuts.

--------------------------------------------------------------------------------


convex == intersection of half spaces

## AD

## Duality + Linear Programming

# MILP {#milp}

## Benders

## Lagrangian / exact

## Strengthened Benders
