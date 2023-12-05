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

.hyperplane {
  stroke: rgb(255, 165, 0);
  stroke-width: 1pt;
}

.mark {
  fill: rgb(255, 165, 0);
}

.function-graph {
  stroke: black;
  fill: none;
}

.relaxation-continuous {
  stroke: blue;
  fill: none;
}

.relaxation-lagrangian {
  stroke: orange;
  fill: none;
}

.relaxation-dual {
  stroke: yellow;
  fill: none;
}

.epigraph {
  fill: hsl(147 42% 64%);
  fill-opacity: 0.3;
  transition: fill-opacity 400ms;
}

.epigraph:hover {
  fill-opacity: 0.6;
}
</style>

For many people, when they hear about optimization, their mind wanders through about continuous variables, gradients and such.
But there is much more out there.
Sometimes, for example, we need to deal with variables that act like a _switch_,
showing some kind of on--off behaviour.
For modeling this we will need integer variables.

<figure class="diagram-container">
  <svg id="binary-variable-switch" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <b><caption>Switch between two states with integer variables.</caption></b>
</figure>

Integer variables naturally arise in a variety of situations:
discrete choices,
modeling generators that are either off or are on with a minimum output,
feasibility sets formed as the union of simpler parts.
Despite the richness of the continuous world,
these are all common cases which are, sometimes, necessary for an accurate modeling.

In [a previous post](/posts/cuts), we discussed approximations by cuts
and how they are a useful tool for representing value functions of parameterized optimization problems.
Cuts are tightly related to convex functions,
and, consequently, to continuous optimization.
Nevertheless, there are techniques to calculate cuts in the presence of integer variables.

Today, we will continue exploring the world of cutting planes
with a focus on _mixed integer programs_ (MIP)
--- optimization problems that can have both continuous and integer decision variables.
We will investigate how the presence of integer variables affects the graph of value functions,
and learn how to use cuts to both solve and approximate this kind of function.

The Shape of an Optimal Value Function
======================================

First of all, let's take a look at some convex programming.
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

<figure class="diagram-container">
  <svg id="figure-ovf-convex" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <caption>Graph of convex optimization problem.</caption>
</figure>

A case of particular importance is that of linear programs,
that is, optimization problems where the objective is linear
and the feasibility set is given by linear equalities and inequalities.
These are a special case of convex programming,
so their optimal value functions are guaranteed to be convex.
But in the presence all this structure, we can go further:
they are in fact polyhedral.

For simplicity, we will only state the theorem for standard form LPs.
This is without loss of generality,
since any linear program can be put into this form.

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

<figure class="diagram-container">
  <svg id="figure-ovf-lp" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <caption>Graph of linear program.</caption>
</figure>

A direct corollary is that the value function of any linear program
can be represented by _finitely many cuts_.
This is a powerful tool when proving the convergence
of algorithms that approximate linear programs by cuts.

Let's now switch our focus towards _mixed integer programs_ (MIP):
optimization problems with both real and integer decision variables.

$$
  \begin{array}{rl}
    f(x) = \min\limits_{u} & c(u) \\
    \textrm{s.t.}   & (x, u) \in X \\
                    & u \in \R^n \times \Z^k.
  \end{array}
$$

Our first step in the analysis of MIP is to isolate the continuous and integer variables
into a two-stage program.
To do that, we have to rewrite $f$ into an equivalent form.
We begin by separating the control variable $u$ into
a continuous part $u_C$ and an integer part $u_I$.
The trick is to change how we enforce the integrality of $u_I$.
Let's allow the entire control --- including $u_I$ --- to be continuous
but add a new integer variable $z$ that is constrained to equal $u_I$.

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

<figure class="diagram-container">
  <svg id="figure-ovf-cip" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <caption>Graph of Mixed Convex Program.</caption>
</figure>

For example, in the case of a _mixed integer linear program_,
the optimal value function is a discrete minimum of polyhedral functions,
meaning that despite not being polyhedral itself, it is nevertheless piecewise-linear.
See the paper by @ralphs_hassanzadeh_2014 for an in-depth study of such value functions.

<figure class="diagram-container">
  <svg id="figure-ovf-milp" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <caption>Graph of MILP.</caption>
</figure>

All this discussion indicates that convex problems with integer variables tend to look like convex, at least locally.
Thanks to that, it is common to use techniques from convex programming to approximate and solve them.
In particular, one generally expects to be able to approximate a MIP using cuts,
even if this approximation isn't the tightest possible.
Let's focus then on how to calculate cuts for this kind of value function.

Different Flavours of Cuts
==========================

For a value function defined via a convex optimization problem,
calculating a Lagrangian dual automatically yields a cut.
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
We follow the cut families defined by @sddip_2019
while generalizing the ideas for a simpler context.

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

<figure class="diagram-container">
  <caption>
    Below, you can see a piecewise convex function,
    representing optimal value of of MIP, and its relaxation.
  </caption>
  <svg id="figure-benders-relaxation" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

From this relation and the convexity of $f$,
we can calculate (not necessarily tight) cuts at any point.

:::Definition
A **Benders cut** for $f$ at $x_0$ is a tight cut
for the continuous relaxation $\cont{f}$ at $x_0$,

$$ f(x) \ge \cont{f}(x_0) + \inner<\lambda, x - x_0>.$$
:::

<figure class="diagram-container">
  <svg id="figure-benders-cut" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <input type="checkbox" id="show-continuous-relaxation" name="show-continuous-relaxation" />
  <label for="show-continuous-relaxation">Show continuous relaxation?</label>
</figure>

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

Although their looseness,
Benders cuts' greatest advantage is being cheap to calculate.
It wouldn't be great if we could somehow make them tighter?
Actually we can do that, by paying the price of solving one extra mixed integer program.
The intuition is that we can calculate a Benders cut and then move it up
until it hits the function's graph somewhere.
This way we find a new tight cut with the Benders multiplier.
But how can we achieve this?

Recall from the previous post about cuts,
that we introduced the [Lagrangian relaxation](/posts/cuts#lagrangian-duality)
of a value function,

$$
  \begin{array}{rl}
    L(x; \lambda) = \min\limits_{u, y} & c(u) + \inner<\lambda, x - y> \\
    \textrm{s.t.}   & (y, u) \in X \\
                    & y \in \R^n \times \Z^k.
  \end{array}
$$

This optimization problem is an affine function
which equals the tightest cut to $f$ with fixed inclination $\lambda$.
We can use this property to strengthen a loose cut into a tight one.
The implementation goes something like this:

```julia
# Returns the tightest cut for `f`
# with the same inclination as the argument cut.
function strengthen_cut(f, cut)
  L = lagrangian_relaxation(f, cut.λ)
  # Find optimal value for Lagrangian relaxation
  opt = solve_mip(L, cut.point)

  return Cut(cut.point, opt, cut.λ)
end
```

In particular, Benders cuts are great candidates for being strengthened.

:::Definition
A **strengthened Benders cut** for $f$ at $x_0$ is a tight cut
for the Lagrangian relaxation using the multiplier $\lambda$ from the Benders cut:

$$ f(x) \ge L(x_0; \lambda) + \inner<\lambda, x - x_0>.$$
:::

<figure class="diagram-container">
  <svg id="figure-strenghtened-benders-cut" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <input type="checkbox" id="show-continuous-str" name="show-continuous-str" />
  <label for="show-continuous-str">Show continuous relaxation?</label>
</figure>

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
  # Cost: Solve 1 convex program
  cut_b = benders_cut(f, a)
  # Solve Lagragian relaxation to improve cut
  # Cost: Solve 1 mixed integer program
  cut_sb = strengthen(f, cut)

  return cut_sb
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

<figure class="diagram-container">
  <svg id="" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <b><caption>Function and Lagrangian cut at each point.</caption></b>
</figure>

The dual value function $\cvx{f}$ is always convex,
since it is the maximum of affine functions (affine in $\lambda$).
Nevertheless, calculating it is not as simple as solving a convex optimization problem
because, for each evaluation, it is necessary to solve several mixed integer programs.
Thus, despite their precision, Lagrangian cuts are computationally expensive approximations.
This is a limiting factor in their usefulness.

Conclusion
==========

<figure class="diagram-container">
  <svg id="" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <b><caption>User can choose between each family of cuts.</caption></b>
</figure>


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

<script type="module">
  import * as figures from "./figures.js";

  const mip     = x => Math.min(Math.abs(x+1), Math.abs(x-1)+1);
  const benders = x => Math.max(-x -1.3, 0.5*x, x -0.5);
  const convex  = x => Math.max(Math.exp(-x-1.2), x, (0.7*x)**4) - 0.5;
  const cip     = x => Math.min( 4*(x+1)**4, 1*(x-1.5)**2 + 0.5, Math.max(1, (x-1)+2));

  figures.figureOVF("#figure-ovf-convex", convex, -2, 2);
  figures.figureOVF("#figure-ovf-lp", benders, -2, 2);
  figures.figureOVF("#figure-ovf-cip", cip, -2, 2);
  figures.figureOVF("#figure-ovf-milp", mip, -2, 2);

  figures.figureContinuousRelaxation("#figure-benders-relaxation", mip, benders, -2, 2);

  figures.figureCutBenders("#figure-benders-cut", mip, benders, -2, 2);

</script>
