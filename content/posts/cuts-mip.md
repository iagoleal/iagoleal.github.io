---
title: Cuts for Mixed Integer Programs
keywords: [math]
date: 2023-11-24
suppress-bibliography: true
header-includes:
- '<script src="https://cdn.jsdelivr.net/npm/d3@7"></script>'
---

\def\cont#1{{#1}_C}
\def\cvx\check
\def\inner<#1,#2>{\left\langle#1,\,#2\right\rangle}

The Shape of an Optimal Value Function
======================================
<!-- (Anatomy ?) -->

Cut Families
============

Benders Cuts
------------

Let's start with the simplest kind of cut for a mixed integer program.
Given a value function $f$,
we define its _continuous relaxation_ as the value of the same optimization problem,
but with the integer variables constrained to be continuous.

$$
  \begin{array}{rl}
    \cont{f}(x) = \min\limits_{u} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X \\
                    & u \in \R^n \times \textcolor{red}{\R^k}.
  \end{array}
$$

Based on our previous discussions,
the relaxation $\cont{f}$ is a convex function.
It is also everywhere below $f$,
because they have the same objective function but $\cont{f}$ has a larger feasible set.

$$f(x) \ge \cont{f}(x).$$

From this relation and the convexity of $f$,
we can calculate (not necessarily tight) cuts at any point.

:::Definition
A **Benders cut** for $f$ at $x_0$ is a cut for $\cont{f}$ at $x_0$,

$$ f(x) \ge \cont{f}(x_0) + \inner<\lambda, x - x_0>.$$
:::

Since $\cont{f}$ is convex,
Benders cuts exist at each point by Lagrangian duality.
They are also cheap to calculate, because,
after all, you only have to solve a convex program to find them.
Also, in the particular case where $F$ is a MILP,
its relaxation will be a Linear Program.
The name Benders comes from the [Benders decomposition](https://en.wikipedia.org/wiki/Benders_decomposition)
for stochastic linear programming,
for which this family of cuts was invented in the 60s.

In terms of implementation,
calculating a Benders cut amounts to relaxing the parameterized optimization problem
and finding a cut for it.
We illustrate the procedure below, in Julia code.

```julia
funtion benders_cut(f, a)
  fc = continuous_relaxation(f)

  # Find optimal value and dual multiplier
  value, multiplier = solve_convex(f, a)
  return Cut(a, value, multiplier)
end
```

Everything has been well and good, so you might be asking:
what are the disadvantages of Benders cuts?
The answer is that, in general, they are far away from tight.
The continuous relaxation $\cont{f}$ is _a convex underapproximation_ of $f$,
but there is not guarantee that it is a good underapproximation --- It can be too low.
It also depends on the representation used for the optimization problem.
Writing an optimal value function in different ways may yield different relaxations.

Strengthened Benders Cuts
-------------------------

Although they may be loose,
Benders cuts' greatest advantage is being cheap to calculate.
It wouldn't be great if we could adapt them to become tight?
Actually we can do that, by paying the price of solving one extra mixed integer program.

The intuition is that we can calculate a Benders cut and then move it up
until it hits the function's graph somewhere.
This way we find a new tight cut with the Benders multiplier.
How can we achieve this?

Recall that in the previous post about cut,
we introduced the [Lagrangian relaxation](/posts/cuts#lagrangian-duality)
of a value function,

$$
  \begin{array}{rl}
    L(x; \lambda) = \min\limits_{u, y} & c(u) + \inner<\lambda, x - y> \\
    \textrm{s.t.}   & (y, u) \in X \\
                    & y \in \R^n \times \Z^k,
  \end{array}
$$

which calculates the value for the tightest cut with a fixed inclination $\lambda$.
We can use this property to strengthen a loose cut into a tight one.
In particular, Benders cuts are great candidates for being strengthened.


:::Definition
A **strengthened Benders cut** for $f$ at $x_0$ is a cut
using the multiplier $\lambda$ of $\cont{f}$ at $x_0$
and the Lagrangian relaxation as constant:

$$ f(x) \ge L(x; \lambda)(x_0) + \inner<\lambda, x - x_0>.$$
:::

Evaluating the Lagrangian relaxation amounts to solving a MIP not much harder than $f$ itself.
Thus, if we know how to find the value of $f$, we also know how to find its Lagrangian.

In terms of computational effort,
calculating a strengthened Benders cut requires
solving a convex program for the inclination
followed by a mixed integer program for the constant.
Below is some Julia some illustrating the procedure.

```julia
funtion strenghtened_benders_cut(f, a)
  # Calculate Benders cut to find dual multiplier
  cut = benders_cut(f, a)
  # Use multiplier to relax problem
  L   = lagrangian_relaxation(f, cut.λ)

  # Find optimal value for Lagrangian relaxation
  value = solve_mip(L, a)
  return Cut(a, value, cut.λ)
end
```

Solving a MIP is much more costly than solving a convex program,
especially if they are linear.
Nevertheless, tight cuts tend to be worth the effort,
since they are directly approximating the best convex underapproximation
instead of some representation-dependent relaxation.
The only problem of strengthened Benders is that they are only guaranteed to be tight _somewhere_.
They are not necessarily tight at the point of choice.

Lagrangian Cuts
---------------

Conclusion
==========
