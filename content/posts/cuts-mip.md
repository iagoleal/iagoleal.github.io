---
title: Cuts for Mixed Integer Programs
keywords: [math]
date: 2023-12-15
description:
  Approximations by cuts are common in convex optimization.
  How to leverage them in the presence of integer variables?
suppress-bibliography: true
---

\def\cont#1{{#1}_C}
\def\cvx\check
\def\inner<#1,#2>{\left\langle#1,\,#2\right\rangle}


<style>
/* CSS for styling */
.diagram-container {
  flex:      auto 1 1;
  max-width: 100%;
  margin-top: 16px;
  margin-bottom: 16px;
}

svg.diagram {
  width:            100%;
  height:           100%;
  border-radius:    5px;
  box-shadow:       rgba(0, 0, 0, 0.1) 0px 4px 12px;
  margin-left:      auto;
  margin-right:     auto;
}

.hyperplane {
  stroke:       var(--color-opposite);
  stroke-width: 1pt;
}

.mark {
  fill: var(--color-opposite);
}

.function-graph {
  stroke: black;
  fill:   none;
}

.relaxation-continuous {
  stroke: var(--color-attention);
  fill:   none;
}

.relaxation-dual {
  stroke:       var(--color-attention);
  stroke-width: 2pt;
  fill:         none;
}

.epigraph {
  fill: var(--color-crystal, hsl(147 42% 64%));
}

.epigraph-component {
  fill: var(--color-attention, hsl(180 43% 51%));
}
</style>

Some people's mind, upon hearing about optimization, start to wander through continuous variables, gradients and such.
But there is much more out there.
Sometimes, for example, we need to deal with variables that act like a _switch_,
showing some kind of on--off behaviour.
In that case, continuous variables are not enough:
we need the full power of integer variables.

<figure id="figure-integer-switch" class="diagram-container">
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <label>
    <input type="checkbox" class="toggle" name="component" />
    Toggle
  </label>
</figure>

Integer variables naturally arise in a variety of situations:
discrete choices,
problems in combinatorics,
machines that are either off or are on with a minimum cost,
feasibility sets formed as the union of simpler parts, etc.
Despite the richness of the continuous world,
these are all common cases requiring only expressible via integrality.

In [a previous post](/posts/cuts), we discussed approximations by cuts
and how they are a useful tool for representing value functions of parameterized optimization problems.
Cuts are tightly related to convex functions and, consequently, to continuous optimization.
Nevertheless, there are techniques to calculate cuts in the presence of integer variables.

Today, we will continue exploring the world of cutting planes
with a focus on _mixed integer programs_ (MIP)
--- optimization problems containing both continuous and integer decision variables:

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X \\
                    & u \in \R^n \times \Z^k.
  \end{array}
$$

We will investigate how the presence of integer variables affects the graph of value functions,
and learn how to use cuts to approximate this kind of function.

The Shape of an Optimal Value Function
======================================

Given a parameterized optimization problem,
how does it value function look like?
One should expect that the better behaved a problem,
the better behaved its value function,
and it is totally right!
Value functions inherit some good properties of their underlying programs.

Let's start with convex programming.
These are among the well-behaved classes of optimization problems
and, as expected, their optimal value functions have a well-defined shape.
Interestingly, if all data in an optimization problem is convex,
its optimal value function ends up also being convex.

:::{.Theorem data-title="Convex Programming"}
An optimal value function

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X \\
  \end{array}
$$

where the objective $c$ is convex and the feasibility set $X$ is jointly convex in $x$ and $u$,
_is a convex function_.
:::

<figure id="figure-ovf-convex"  class="diagram-container">
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

A case of particular importance is that of linear programs,
that is, optimization problems where the objective is linear
and the feasibility set is given by linear equalities and inequalities.
These are a special case of convex programming,
so their optimal value functions are guaranteed to be convex.
But in the presence of all this structure, we can go further:
they are in fact polyhedral.

For simplicity, we will only state the theorem for standard form LPs.
This is without loss of generality,
since one can put any linear program into this form.

:::{.Theorem data-title="Linear Programming"}
The optimal value function for a _standard form linear program_

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u} & \inner<c, u> \\
    \textrm{s.t.}   & Au = x, \\
                    & x \ge 0,
  \end{array}
$$

is a _polyhedral function_.
:::

<figure id="figure-ovf-lp" class="diagram-container">
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

A direct corollary is that the value function of any linear program
is representable with _finitely many cuts_.
This is a powerful tool when proving the convergence of cutting plane algorithms for linear programs.

Let's now switch our focus towards _mixed integer programs_ (MIP):
optimization problems with both real and integer decision variables.

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X \\
                    & u \in \R^n \times \Z^k.
  \end{array}
$$

Our first step in the analysis of a MIP is to isolate the continuous and integer variables
into a two-stage program.
To do that, we have to rewrite $f$ into an equivalent but more verbose form.
We begin by separating the decision variable $u$ into
a continuous part $u_C$ and an integer part $u_I$.
The trick is to change how we enforce the integrality of $u_I$.
Let's allow the entire control --- including $u_I$ --- to be continuous
but add a new integer variable $z$ constrained to equal $u_I$.

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u_C, u_I} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X \\
                    & u = (u_C, u_I) \in \R^{n+k} \\
                    & \textcolor{red}{u_I = z} \\
                    & \textcolor{red}{z \in \Z^k}.
  \end{array}
$$

This construction adds nothing but redundancy to our problem.
Nevertheless, this gained redundancy isolates the integer variables
into a single constraint separable from the others.
This way, we can rewrite the optimization as a problem with two stages:

$$
  \begin{array}{rl}
    f(x) = \min\limits_{z} & Q(x, z) \\
    \textrm{s.t.}  & z \in \Z^k \\
    Q(x, z) = \min\limits_{u_C} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X \\
                    & u = (u_C, z).
  \end{array}
$$

In the equation above, the second optimization problem in $Q$ has only continuous variables
because it only sees the variable $z$ _fixed_ at some integer value.

When $Q$ has some known structure on $x$ --- convex or linear programming, for example ---
the function $f$ will locally have this same structure,
because it is a discrete minimum of $Q$.
The intuitive view is that for each value of z, there is a well-behaved function $Q(\cdot, z)$.
For a given $x$, $f$ chooses the best of these functions and then optimizes on it.
Furthermore, since the minimum in $z$ is discrete, continuity implies
that we must have whole connected ranges of $x$ for which it will choose the same $z$.

<figure id="figure-ovf-cip" class="diagram-container">
  <caption>You can hover the graph below to highlight each of its convex components.</caption>
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

For example, in the case of a _mixed integer linear program_,
the optimal value function is a discrete minimum of polyhedral functions,
meaning that despite not being polyhedral itself, it is nevertheless piecewise-linear.
See the paper by @ralphs_hassanzadeh_2014 for an in-depth study of such value functions.

<figure id="figure-ovf-milp" class="diagram-container">
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

All this discussion indicates that convex problems with integer variables tend to look like convex, at least locally.
Thanks to that, it is common to use techniques from convex programming to approximate and solve them.
In particular, one generally expects to be able to approximate a MIP using cuts,
even if this approximation isn't the tightest possible.
Let's focus then on how to calculate cuts for this kind of value function.

Different Flavours of Cuts
==========================

For a value function defined via a convex optimization problem,
Lagrangian duality yields tight cuts for free.
In the presence of integer variables, however, life is not so simple
because the solving methods do not automatically calculate dual multipliers for us.

Calculating a cut for a MIP requires more work but is still feasible.
All the strategies that we will show today follow the same idea:
Find a convex underapproximation for your value function and calculate cuts for it.
These are also automatically cuts for the original problem.

As you may imagine, there are several options to choose from, each with its pros and cons.
In the following,
let's take a look at three such methods with varying approximation quality,
and corresponding computational cost.
We follow the cut families defined by @sddip_2019
while generalizing the ideas for a simpler context.

Do keep in mind that not all MIP value functions accept cuts,
because their epigraph could have the entire space as its convex hull.[^mip-no-cuts]
Nevertheless, in the real world you rarely stumble into one of these.
Thus, knowing how approximate MIPs can prove to be a useful tool to keep in your utility belt

[^mip-no-cuts]: As an example, consider $h(x) = -|x|$.
  This function accepts no cuts and is representable as the MIP:
  $$
    \begin{array}{rl}
      h(x) = \min\limits_{t,z} & t \\
      \textrm{s.t.}   & t \ge zx, \\
                      & t \in \R,\, z \in \{-1, +1\}.
    \end{array}
  $$

Throughout this section,
we will only consider optimization problems that are "convex except for integer variables",
that is, problems having a convex objective and convex feasible set,
but with non-convexity arising from part of the decision variables being integer.

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X \\
                    & u \in \R^n \times \Z^k.
  \end{array}
$$

Benders Cuts
------------

Let's start with the simplest kind of cut for a mixed integer program.
Given a value function $f$,
we define its _continuous relaxation_ as the value of the same optimization problem,
but with the integer variables relaxed to be continuous.[^lp-relaxation]

$$
  \begin{array}{rl}
    \cont{f}(x) = \min\limits_{u} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X \\
                    & u \in \R^n \times \textcolor{red}{\R^k}.
  \end{array}
$$

[^lp-relaxation]: In the context of Linear Programming, you will probably find it
with the name "LP relaxation" because it turns a MILP into a LP.

Based on our previous discussions,
the relaxation $\cont{f}$ is a convex function.
It is also everywhere below $f$,
because they have the same objective function but $\cont{f}$ has a larger feasible set.

$$f(x) \ge \cont{f}(x).$$

<figure id="figure-benders-relaxation" class="diagram-container">
  <caption>
    Below, you can see a piecewise convex function,
    representing the optimal value of an MIP, and its relaxation.
  </caption>
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

From this inequality and the convexity of $f$,
we can calculate (not necessarily tight) cuts at any point.

:::Definition
A **Benders cut** for $f$ at $x_0$ is a tight cut
for the continuous relaxation $\cont{f}$ at $x_0$,

$$ f(x) \ge \cont{f}(x_0) + \inner<\lambda, x - x_0>.$$
:::

<figure id="figure-benders-cut" class="diagram-container">
  <caption>
    By moving your mouse in the diagram below,
    you can visualize the Benders cut at the selected position.
    Notice how they are, in general, nowhere tight.
  </caption>
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <label>
    <input type="checkbox" class="toggle" name="show-relaxation-continuous" />
    Continuous relaxation
  </label>
</figure>

Since $\cont{f}$ is convex,
Benders cuts exist at each point by Lagrangian duality.
They are also cheap to calculate, because,
after all, you only have to solve a convex program to find them.
Also, in the particular case where $f$ is a MILP,
its relaxation will be a Linear Program --- which are even faster to solve.
The name Benders comes from the [Benders decomposition](https://en.wikipedia.org/wiki/Benders_decomposition)
for stochastic linear programming,
for which this family of cuts was invented in the 60s.

In terms of implementation,
calculating a Benders cut amounts to
relaxing the parameterized optimization problem and finding a cut for it.
We illustrate the procedure below, in Julia code.

```julia
function benders_cut(f, a)
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
but there is no guarantee that it is a good underapproximation --- It can be too low.
Another question is that the continuous relaxation is _representation dependent_.
Equivalent optimization problems have the same optimal value function
but may yield different continuous relaxations.

Lagrangian Cuts
---------------

Even though it does not come for free, as in the convex case,
Lagrangian duality still works for mixed integer programs.
The difference is that you have to explicitly solve the dual formulation.
As a prize for your effort, you will gain the tightest cut possible at a chosen point.

Recall from the previous post,
that the convex relaxation of a function is represented as the
[the Lagrangian dual problem](/posts/cuts#lagrangian-duality).
For our MIP, it has the following form:

$$
  \begin{array}{rl}
    \cvx{f}(x) = \max\limits_{\lambda} \min\limits_{u, y} & c(u) + \inner<\lambda, x - y> \\
    \textrm{s.t.}   & (y, u) \in X \\
                    & u \in \R^n \times \Z^k.
  \end{array}
$$

A cut for the dual value function is called a _Lagrangian cut_,
because it comes from the Lagrangian dual.
It is the tightest cut possible at a point.

:::Definition
A **Lagrangian cut** for $f$ at $x_0$ is a tight cut
for the dual $\cvx{f}$ at $x_0$,

$$f(x) \ge \cvx{f}(x_0) + \inner<\lambda, x - x_0>.$$
:::

<figure id="figure-lagrangian-cut" class="diagram-container">
  <caption>
    Move your mouse inside the diagram below to visualize the Lagrangian cut at the selected position.
    This is the tightest cut possible at that position, since it is tight for the epigraph's convex hull.
  </caption>
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <label>
    <input type="checkbox" class="toggle" name="show-relaxation-dual" />
    Lagrangian dual
  </label>
</figure>

Despite the dual function $\cvx{f}$ being convex,
it does not come from a convex optimization problem
and, consequently, is much harder to solver.
In general, to evaluate $\cvx{f}$, you will need to solve several mixed integer programs
in order to find the optimal multiplier $\lambda$.
Thus, despite their precision, Lagrangian cuts are computationally expensive approximations.
This is a limiting factor in their usefulness.

Strengthened Benders Cuts
-------------------------

Until now we've seem two kinds of cuts:
Benders are cheap but loose while Lagrangian are tight but expensive.
Is there a middle ground?
Perhaps there is some way to calculate cuts that are still good,
even if not the best possible,
but do not require so many resources to calculate.

The idea we will follow is that instead of directly calculating good cut,
we will take a loose one and improve it.
As we will shortly see,
it is possible to turn any valid cut into a tight one
at the price of solving one extra mixed integer program.
The intuition is that we can move a cut up until it hits the function's graph.

Recall from the previous post that the [Lagrangian relaxation](/posts/cuts#lagrangian-duality)
of a value function is an affine underapproximation
equal to the tightest cut for $f$ with fixed inclination $\lambda$,
defined as

$$
  \begin{array}{rl}
    L(f, \lambda)(x) = \min\limits_{u, y} & c(u) + \inner<\lambda, x - y> \\
    \textrm{s.t.}   & (y, u) \in X \\
                    & y \in \R^n \times \Z^k.
  \end{array}
$$

Thus, the Lagrangian relaxation transforms
a loose cut into a tight one with the same inclination.
We call this procedure _strengthening a cut_.
The implementation goes like this:

```julia
# Returns the tightest cut for `f`
# with the same inclination as the argument cut.
function strengthen_cut(f, cut)
  L  = lagrangian_relaxation(f, cut.λ)
  # Find optimal value for Lagrangian relaxation
  Lx = solve_mip(L, cut.x)

  return Cut(cut.x, Lx, cut.λ)
end
```

In particular, Benders cuts are great candidates for being strengthened.

:::Definition
A **strengthened Benders cut** for $f$ at $x_0$ is a tight cut
for the Lagrangian relaxation using the multiplier $\lambda$ from the Benders cut:

$$ f(x) \ge L(x_0; \lambda) + \inner<\lambda, x - x_0>.$$
:::

<figure id="figure-strenghtened-benders-cut" class="diagram-container">
  <caption>
    Hover the diagram below to visualize the Benders cut at the selected position.
    You can also click after choosing a position to strengthen the cut.
    Notice that these cuts are always tight but not necessarily at the chosen point.
  </caption>
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <label>
    <input type="checkbox" class="toggle" name="show-relaxation-continuous" />
    Continuous relaxation
  </label>
  <label>
    <input type="checkbox" class="toggle" name="show-relaxation-dual" />
    Lagrangian dual
  </label>
</figure>

Evaluating the Lagrangian relaxation amounts to solving a MIP not much harder than $f$ itself.
Thus, if we know how to find the value of $f$, we also know how to find its Lagrangian.

In terms of computational effort,
calculating a strengthened Benders cut requires
solving a convex program for the inclination
followed by a mixed integer program for the constant.
Below is some Julia some illustrating the procedure.

```julia
function strenghtened_benders_cut(f, a)
  # Calculate Benders cut to find dual multiplier
  # Cost: Solve 1 convex program
  cut_b = benders_cut(f, a)
  # Solve Lagragian relaxation to improve cut
  # Cost: Solve 1 mixed integer program
  cut_sb = strengthen(f, cut)

  return cut_sb
end
```

Solving a MIP is much more costly than solving a convex program,
especially in the linear case.
Nevertheless, tight cuts tend to be worth the effort,
since they are directly approximating the best convex underapproximation
instead of some representation-dependent relaxation.
The main problem with strengthened Benders cuts is that they are only guaranteed to be tight _somewhere_.
They are not necessarily tight at the point of choice.


Parting Thoughts
================

In the end of the day,
the right cut for your MIP will depend a lot on the application at hand.
There is always a trade-off between precision and computational effort,
as you can see in the following table.

+--------------+----------------------+------------------------+
| Cut          | Tightness            | Effort                 |
+:============:+:====================:+:======================:+
| Benders      | Loose                | Convex                 |
+--------------+----------------------+------------------------+
| Strengthened | Tight                | Convex + MIP           |
| Benders      | somewhere            |                        |
+--------------+----------------------+------------------------+
| Lagrangian   | As tight as possible | Many MIPs              |
+--------------+----------------------+------------------------+

One thing to keep in mind is that the time necessary to calculate a tighter cut
could be used to calculate a lot of Benders cuts --- a looser but better-shaped approximation.
If this Benders approximation stops converging, you can strengthen some cuts to "clean up some area".
A Lagrangian cut works best as a last resort, and only if you are interested in the area around a very specific point,
because in the time you are calculating it, you could be getting many strengthened Benders cuts to cover a larger area.

<script type="module">
  import * as figures from "./figures.js";

  const convex  = x => Math.max(Math.exp(-x-1.2), x, (0.7*x)**4) - 0.5;
  const lp      = x => Math.max(-2*(x+0.5) -1.3, -0.2, 0.5*x, 0.9*x -0.5) + 0.2;

  const cvxs    = [x => 4*(x+1)**4, x => 1*(x-1.5)**2 + 0.5, x => Math.max((x+0.7)**2, 1, (x-1)+2)];
  const lps     = [x => Math.abs(x+1), x => Math.abs(x-1)+1];

  const cip     = x => Math.min(...cvxs.map(f => f(x)));
  // Convex everywhere below cip
  const benders = x => x < 0 ? 0.05*x**4 - 0.12 : -0.12 + 0.15*x**2;

  figures.figureSwitch("#figure-integer-switch", [cvxs[0], x => Math.max(0, -2*x)], -2, 2);

  figures.figureOVF("#figure-ovf-convex", convex, -2, 2);

  figures.figureOVF("#figure-ovf-lp", lp, -2, 2);

  figures.figureMinOVF("#figure-ovf-cip", cvxs, -2, 2);

  figures.figureMinOVF("#figure-ovf-milp", lps, -2, 2);

  figures.figureContinuousRelaxation("#figure-benders-relaxation", cip, benders, -2, 2);

  figures.figureCutBenders("#figure-benders-cut", cip, benders, -2, 2);

  figures.figureCutStrenghtenedBenders("#figure-strenghtened-benders-cut", cip, benders, -2, 2);

  figures.figureCutLagrangian("#figure-lagrangian-cut", cip, -2, 2);
</script>
