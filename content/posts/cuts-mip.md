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


<style>
/* CSS for styling */
.diagram-container {
  flex: auto 1 1;
  max-width: 100%;
}

svg.diagram {
  width: 100%;
  height: 100%;
  border: 1px solid #ccc;
  margin-left: auto;
  margin-right: auto;
}

.multi-figure-container {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  gap: 1.5rem;
  justify-content: space-between;
}

.multi-figure-container > svg {
  max-width: 100%;
  height: auto;
}

.convex-set {
  fill: hsl(147 42% 64%);
  stroke: black;
  opacity: 1;
  transition: fill 100ms;
}
</style>

The Shape of an Optimal Value Function
======================================
<!-- (Anatomy ?) -->

Cut Families
============

For a value function defined via a convex optimization problem,
we can always calculate cuts via Lagrangian duality.
In the presence of integer variables, however, life is not so simple
because the solving methods do not automatically calculate dual multipliers for us.

To calculate a cut for a MIP,
the usual way is to underapproximate it using an easier to calculate, convex program
and then employ the cuts for this approximation.
As you may imagine, there are several options on how to do that,
all with their pros and cons.
In the following,
let's take a look at three such methods with varying approximation quality,
and corresponding computational cost.

Throughout this section,
we will only consider value functions with a convex objective and convex feasible set,

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X \\
                    & u \in \R^n \times \Z^k.
  \end{array}
$$

All non-convexity arises from some of the decision variables being integer.

Benders Cuts
------------

<figure class="diagram-container">
  <svg id="benders" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

Let's start with the simplest kind of cut for a mixed integer program.
Given a value function $f$,
we define its _continuous relaxation_ as the value of the same optimization problem,
but with the integer variables relaxed to be continuous.

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
A **Benders cut** for $f$ at $x_0$ is a tight cut
for the continuous relaxation $\cont{f}$ at $x_0$,

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
A **strengthened Benders cut** for $f$ at $x_0$ is a tight cut
for the Lagrangian relaxation using the multiplier $\lambda$ from the Benders cut:

$$ f(x) \ge L(x_0; \lambda) + \inner<\lambda, x - x_0>.$$
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

Sometimes, we really want to find a cut that is the tightest possible
at a chosen point.
In this case, we have to directly calculate a cut for the convex relaxation of $f$.

As you may recall from the previous post,
calculating the best cut at a point amounts to
[solving the dual problem](/posts/cuts#lagrangian-duality)

$$
  \begin{array}{rl}
    \cvx{f}(x) = \max\limits_{\lambda} \min\limits_{u, y} & c(u) + \inner<\lambda, x - y> \\
    \textrm{s.t.}   & (x, u) \in X \\
                    & u \in \R^n \times \Z^k.
  \end{array}
$$

A cut for the dual value function is called a _Lagrangian cut_,
because it comes from the Lagrangian dual,
and is the tightest cut possible at a point.

:::Definition
A **Lagrangian cut** for $f$ at $x_0$ is a tight cut
for the dual $\cvx{f}$ at $x_0$,

$$f(x) \ge \cvx{f}(x_0) + \inner<\lambda, x - x_0>.$$
:::

The dual value function $\cvx{f}$ is always convex,
since it is the maximum of affine functions (affine in $\lambda$).
Nevertheless, calculating it is not as simple as solving a convex optimization problem
because, for each evaluation, it is necessary to solve several mixed integer programs.
Thus, despite their precision, Lagrangian cuts are computationally expensive approximations.
This is a limiting factor in their usefulness.

Conclusion
==========

+--------------+-------------------+------------------------+
| Cut          | Tightness         | Effort                 |
+:============:+:=================:+:======================:+
| Benders      | Loose             | Convex problem         |
+--------------+-------------------+------------------------+
| Strengthened | Tight             | Convex problem         |
| Benders      | somewhere         | +  MIP                 |
+--------------+-------------------+------------------------+
| Lagrangian   | Tight             | Many MIP               |
+--------------+-------------------+------------------------+
