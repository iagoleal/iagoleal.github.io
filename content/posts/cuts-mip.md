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

Lagrangian Cuts
---------------

Conclusion
==========
