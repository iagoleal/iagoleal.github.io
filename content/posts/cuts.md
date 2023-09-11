---
title: Approximation by a thousand cuts
subtitle: A guide to piecewise-linear approximations
keywords: [math]
date: 2023-09-08
---

\def\R{\mathbb{R}}
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
Taking an approximation of $f$ by cuts $C = \{\inner<a,\cdot> + b\}$
amounts to choosing the largest cut at each given point:

$$ f(x) \approx \max\limits_{(a,b) \in C} \inner<a_,x> + b. $$

This formula also explains why we refer to affine functions as "cuts".
Think about the graph of $f$.
The graph of a cut is an hyperplane diving the space in two halves:
points above and below it.
You can think of the process of maxing out the cuts
as slicing the space to form a region containing the graph of $f$.
Below we see a illustration of this view.

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
So if $f$ is some complicated convex function but you already have
a good approximation by cuts to it, you can minimize it instead
and get and approximation of the optimum.

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

A dive into the geometry
========================







-----------------------------------------------------------------------------------

Convex separation theorem

separation to a point

convex == intersection of half spaces

# Convex functions

## AD

## Duality + Linear Programming

# MILP {#milp}

## Benders

## Lagrangian / exact

## Strengthened Benders
