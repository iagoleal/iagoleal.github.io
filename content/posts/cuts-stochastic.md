---
title: Cuts for Stochastic Programming
keywords: [math]
date: 2024-04-25
theme: [math, interactive]
requisites: [cuts]
thumbnail: "figure-tree-singlecut.png"
description:
  Cutting planes are a powerful tool for solving stochastic programs.
  We focus on the possible ways to represent the cuts during algorithms
  and their consequences for parallelization.
suppress-bibliography: true
---

<style>
.Missing {
  text-align: center;
  width:  100%;
  height: 300px;
  background-color: gray;
  border: black 1px;
}
</style>

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
  stroke: var(--color-accent, hsl(147 42% 64%));
  fill:   none;
}

.stochastic {
  opacity: 0.4;
  stroke-width: 1pt;
  transition: all 0ms, opacity 200ms;
}

.stochastic:hover {
  opacity: 0.8;
  stroke-width: 1.5pt;
}

.deterministic {
  opacity: 1.0;
  stroke-width: 1pt;
  /* filter: url("#glow"); */
}

.decomposed {
  stroke: var(--color-attention);
}

.linked {
  stroke:       var(--color-opposite);
  stroke-width: 2pt;
}

.epigraph {
  fill: var(--color-crystal, hsl(147 42% 64%));
}

</style>

\def\E#1{\mathbb{E}\left[ #1 \right]}
\def\inner<#1,#2>{\left\langle#1,\,#2\right\rangle}
\def\Scenarios{\mathcal{S}}
\def\Cuts{\mathcal{C}}
\def\Qfrak{\mathfrak{Q}}

```{=tex}
\usetikzlibrary{graphs,shapes.geometric}
\usetikzlibrary{fit}

\tikzset{
  opt/.style   = {draw=black, circle, minimum size=1cm},
  clink/.style = {draw=black, shape=diamond, thick, inner sep = 0.5mm, minimum size = 0.1mm},
}

\tikzset{
  svgclass/.style={
    execute at begin scope={\special{dvisvgm:raw <g class="#1">}},
    execute at end scope={\special{dvisvgm:raw </g>}},
  }
}

\pgfdeclarelayer{background}
\pgfsetlayers{background,main}
```

Life is full of uncertainties and so are the problems one must face.
When solving optimization problems with stochastic components,
it is necessary to adapt your solution methods to prevent an explosion on the model's size.

Let's continue today with this blog series on _cutting planes_
by taking them as our tool of choice for stochastic optimization.
Since these problems can get huge pretty easily,
we focus on the possible ways to represent the cuts during the solve
and their consequences for parallelization.

By the way, the content about [non-convex stochastic programs](#ncvx)
comes directly from my [Master's thesis](/masters/), called _Convexification by Averages_.


Optimizing under an Uncertain Future
====================================

In many real-world problems,
it's necessary to take a decision incurring an immediate cost plus future consequences.
Think about buying a car, for example: there is the payment at the buying time
plus future costs such as maintenance, fuel or even purchase installments.

$$
  \begin{array}{rl}
    \min\limits_{z} & c_{\text{now}}(z) + c_{\text{future}}(z) \\
    \textrm{s.t.}  & z \in Z. \\
  \end{array}
$$

As the future is known to be an uncertain beast, we cannot be sure of how much of its costs.
At best, we can estimate it using a random function $c_{\text{future}}$.
Besides that, considering the decision-maker you are,
let's break the decision variable into a current decision $x$
and a future decision $y$, which itself depends on $x$.
This way, we're better able to codify how different parts of the program interact.

$$
  \begin{array}{rl}
    \min\limits_{x, y} & c_{\text{now}}(x) + c_{\text{future}}(x, y) \\
    \textrm{s.t.}   & x \in X \\
                    & (x, y) \in Y.
  \end{array}
$$

Thanks to our temporal considerations,
the program above has a particular structure we're able to exploit.
Notice that the present only depends on $x$ while the future depends on both variables.

```tikz {tikzlibrary="fit"}
% Block matrix
\matrix [
    matrix of nodes,
    minimum width  = 0.9cm,
    minimum height = 0.6cm,
    column sep = 0.5mm,
    row    sep = 0.5mm,
    left  delimiter = {[},
    right delimiter = {]},
    column 1/.style = {
      minimum width  = 2cm,
    },
    row 1/.style = {
      minimum height = 2cm,
    },
    row 1 column 1/.style = {
      minimum size = 2cm,
    },
  ] (m) {
  {} &[0.5mm] {} & {} & {} \\[0.5mm]
  {} & {} & {} & {} \\
  {} & {} & {} & {} \\
  {} & {} & {} & {} \\
};

{ [rectangle, rounded corners = 1mm, inner sep = 0pt]
  \node[fill=orange!70, fit=(m-1-1)]          {};
  \node[fill=orange!70, fit=(m-2-1)(m-4-1)]   {};

  % Scenarios for 2nd stage
  \node[fill=green!40, fit=(m-2-2) (m-4-4)]   {};
}

% Labels
\node [above = 0.2cm of m-1-1] {$x$};
\node [above = 0.2cm of m-1-3] {$y$};
\node [left  = 0.4cm of m-1-1] {present};
\node [left  = 0.4cm of m-3-1] {future};
```

Whenever the variable dependencies have this particular "L-shape",
it is possible to break the program into two interacting parts:

* A _first stage_ that is deterministic but whose total cost depends on the ensuing stage's cost;
* A _second stage_ that is stochastic and whose parameters depend on the previous stage's decision.

```tikz
{ [every edge/.style = {{Round Cap}-Kite, draw},
   every loop/.style = {{Round Cap}-Kite, draw},
   stage/.style = {circle, minimum width = 1.3cm, draw = black},
  ]
  \node[stage] (fst)    {present};
  \node[stage] (snd) [right = 3cm of fst]  {future};

  \path[->] (fst) edge[bend left] node [above] {decision} (snd)
            (snd) edge[bend left] node [below] {cost}     (fst);
}
```

Notice that one needs to solve both stages together,
because of their mutual dependence.
By introducing a (random) optimal value function $Q$ for the second stage,
called its __cost-to-go__, our decision problem becomes

$$
  \begin{array}{rl}
    \min\limits_{x} & c_{\text{now}}(x) + Q(x) \\
    \textrm{s.t.}  & x \in X, \\
    Q(x) = \min\limits_{y} & c_{\text{future}}(x, y) \\
    \textrm{s.t.}   & (x, y) \in Y.
  \end{array}
$$

Also, the stochastic second stage is equivalent
to a family of deterministic value functions,
each with its decision variables $y^s$,
where the distinct scenarios do not interact.
Thus, besides its L-shape, the future dependency on $y$ also
breaks into small blocks corresponding to each scenario.
This will be important in the ensuing sections to make the solution methods computationally tractable.

```tikz {tikzlibrary="fit"}
% Block matrix
\matrix [
    matrix of nodes,
    minimum width  = 0.9cm,
    minimum height = 0.6cm,
    column sep = 0.5mm,
    row    sep = 0.5mm,
    left  delimiter = {[},
    right delimiter = {]},
    column 1/.style = {
      minimum width  = 2cm,
    },
    row 1/.style = {
      minimum height = 2cm,
    },
    row 1 column 1/.style = {
      minimum size = 2cm,
    },
  ] (m) {
  {} &[0.5mm] {} & {} & {} \\[0.5mm]
  {} & {} & {} & {} \\
  {} & {} & {} & {} \\
  {} & {} & {} & {} \\
};

{ [rectangle, rounded corners = 1mm, inner sep = 0pt]
  \node[fill=orange!70, fit=(m-1-1)]          {};
  \node[fill=orange!70, fit=(m-2-1)(m-4-1)]   {};

  % Scenarios for 2nd stage
  \node[fill=green!40, fit=(m-2-2)]   {};
  \node[fill=green!40, fit=(m-3-3)]   {};
  \node[fill=green!40, fit=(m-4-4)]   {};
}

% Labels
\node [above = 0.2cm of m-1-1] {$x$};
\node [above = 0.2cm of m-1-3] {$y$};
\node [left  = 0.4cm of m-1-1] {present};
\node [left  = 0.4cm of m-3-1] {future};
```


There is still one detail to sort out in this formulation:
since the cost-to-go $Q$ is stochastic, the total cost is also random.
This is terrible for analysis,
because, commonly, one wishes the final decision the model spits out to be bold and deterministic.
Therefore, to properly solve the optimization problem,
we need a way to aggregate the second stage's cost into a single number.
Since [the best constant representing a random variable is its expected value](/posts/shower-thoughts-averages),
we take the average of all possible future outcomes to represent its cost.[^future-risk-averse]

[^future-risk-averse]: This choice of aggregating with the expected value
  is called a _risk-neutral formulation_ for a stochastic program and is, by far,
  the simplest and most common formulation you'll find.
  Nevertheless, it is possible to use a _risk-averse formulation_ by substituting the mean by any [coherent risk measure](https://en.wikipedia.org/wiki/Coherent_risk_measure).
  Everything we do in this post works the same for both formulations.
  We're only sticking to the average because it is simpler to understand.

$$
  \begin{array}{rl}
    \min\limits_{x} & c_{\text{now}}(x) + \E{Q(x)} \\
    \textrm{s.t.}  & x \in X. \\
  \end{array}
$$

This setup configures a (two-stage) stochastic program
and appears in a lot of industrial problems.
Since it is the main subject we're going to explore today,
I think it deserves its own definition.

:::Definition
A __stochastic program__ is a two-stage optimization problem
with random future.

$$
  \begin{array}{rl}
    \min\limits_{x} & c_1(x) + \E{Q(x)} \\
    \textrm{s.t.}  & x \in X \\
    Q(x) = \min\limits_{y} & c_2(x, y) \\
    \textrm{s.t.}   & (x, y) \in Y.
  \end{array}
$$
:::


Stochastic Optimal Value Functions
----------------------------------

Stochastic optimal value functions allow us to talk about, and solve,
optimization problems with uncertain components.
They take the form

$$
  \begin{array}{rl}
    Q(x) = \min\limits_{y} & c(y) \\
    \textrm{s.t.}   & (x, y) \in Y
  \end{array}
$$

Where the objective $c$ and the feasibility set $Y$ are _random_.
Leaving any concerns about measurability aside [^finite-sto],
this defines a random variable for each input $x$
and is, thus, what one calls a _random function_.

<figure id="figure-random-function" class="diagram-container">
  <caption>
    For finite uncertainty, a good intuition is to think of
    a random function as a collection of functions defined for each scenario.
  </caption>
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

[^finite-sto]: We will only work with finite uncertainties,
so there shouldn't be any difficulties.

As usual in probability,
we treat random functions as a normal function but whose output
may depend on some stochastic external state.
Taking this view, we say that a random function has some property,
such as being non-negative, linear, convex, monotone etc.
whenever it has this property whatever the outcome.

For example, consider $Q(x) = D_{6}\cdot x^2$,
where $D_6$ corresponds to throwing a 6-sided die.
It is a convex, non-negative random function
because these properties are valid no matter the scenario.

To make our notation more functional, we're going to write $\E{Q}$ for the deterministic function
defined pointwisely as the average of $Q$,

$$\E{Q}(x) = \E{Q(x)}.$$

These averages will appear a lot throughout this post,
so it is good to get acquainted with them soon.
For optimization, there is an important result --- which we will use a lot --- regarding the average: it preserves convexity.

:::Theorem
The average $\E{Q}$ of a convex random function $Q$ is also convex.
:::

<figure id="figure-random-average" class="diagram-container">
  <caption>
    In the figure below you can see a convex random function (faded) together with its average (bold graph).
  </caption>
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

Convex Stochastic Programs
==========================

To start delving into the world of stochastic programs,
let's begin by the _convex_ case.
Hence, throughout this section we will always assume
that the second stage is a random convex optimization problem.
We need to put no restrictions on the first stage,
since we will only need cuts for the future part.
Now it's time to approximate some random functions by cuts.

Thanks to convexity, we can use Lagrangian duality
at each scenario to produce (random) tight cuts at any point of choice.
Suppose we want to calculate our cut at $x_0$.
It defines a random optimal value $Q(x_0)$
but also a dual multiplier $\Lambda$ dependent on the solution of $Q(x_0)$.
This produces the cut we want:

$$ Q(x) \ge Q(x_0) + \inner<\Lambda, x - x_0>.$$

<figure id="figure-random-cuts" class="diagram-container">
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

Since expected values preserve inequalities,
this equation is all we need to approximate $\E{Q}$ by cuts.

:::Theorem
If a random cut is tight at $x_0$ for $Q$,
then its average is tight for the expected function $\E{Q}$,

$$ \E{Q}(x) \ge \E{Q}(x_0) + \inner<\E{\Lambda}, x - x_0>.$$
:::

<figure id="figure-average-cuts" class="diagram-container">
  <caption>
    Hover the figure below to calculate a cut for each scenario and an average cut.
  </caption>
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

The theorem above is the key for solving two-stage stochastic programs using cuts.
When the uncertainty has a finite amount of scenarios,
we can calculate the average cut as a two-step process:

- For each scenario $s$, calculate a cut for the (deterministic) sample $Q^s$;
- Combine the optimal values and multipliers into a cut for $\E{Q}$.

For infinite uncertainty, it is possible to use _Sample Average Approximation_
or the law of large numbers to estimate the averages by independently sampling from $Q$.
But this is out of scope for this post.


Single Cut Approach
-------------------

Now that we know how to approximate the average of a random function,
we can use it to construct an algorithm for solving stochastic programs.
The idea is to adapt [Kelley's cutting plane algorithm](/posts/cuts#example-kelley-cutting-planes)
for stochastic programs.

All algorithms we're going to see in this post are variations on the same idea.
We turn the recursive relation between present and future into an iterative algorithm,
where the first stage sends its decision as a parameter to the second stage,
which in counterpart sends cuts to improve the first stage's view of the future.
For convex programs, this process eventually converges towards a good enough approximation of the optimal decision.

```tikz
{ [every edge/.style = {{Round Cap}-Kite, draw},
   every loop/.style = {{Round Cap}-Kite, draw},
   stage/.style = {circle, minimum width = 1.3cm, draw = black},
  ]
  \node[stage] (fst)    {present};
  \node[stage] (snd) [right = 3cm of fst]  {future};

  \path[->] (fst) edge[bend left] node [above] {decision} (snd)
            (snd) edge[bend left] node [below] {cut}     (fst);
}
```


Now, it's time to get technical.
Consider a pool of cuts $\Cuts$ for $\E{Q}$ that starts empty.
From now on, we will call it the algorithms **cut pool**.
This pool allows the construction a polyhedral underapproximation of $\E{Q}$ as

$$ \Qfrak(x) = \max\limits_{(b, \lambda) \in \Cuts} b + \inner<\lambda, x>.$$

This is called a _single cut approximation_ for $\E{Q}$
because it is made only of cuts that directly approximate the average
(But this nomenclature will make no difference until next section,
so let's go back to how to use our new friend to solve stochastic programs.)
With $\Qfrak$ at hand,
we can construct an underapproximation for the total cost $z^\star$ as

$$
  \begin{array}{rrl}
    z(\Cuts) &= \min\limits_{x}   & c_1(x) + \Qfrak(x) \\
      &\textrm{s.t.}  & x \in X \\
      &= \min\limits_{x,t} & c_1(x) + t \\
      &\textrm{s.t.}  & x \in X, \\
      &               & t \ge b + \inner<\lambda, x>,\; \forall (\lambda, b) \in \Cuts.
  \end{array}
$$

We are substituting the complicated expected cost-to-go $\E{Q}$
by a bunch of linear constraints, making the problem much easier.
Notice also that if the first stage was convex or an LP, it remains so,
which is a big win in terms of plugging it into a solver.

If $\Qfrak$ is a good approximation of $Q$,
we expect $z(\Cuts)$ to be close to the real optimal $z^\star$.
But what do we do if it isn't?
We get more cuts to improve it, of course!
By using what we discussed in the previous section,
we can calculate tight cuts for $\E{Q}$.
Whenever we solve the first stage, it gives us a solution $x \in X$
that is feasible in the real problem.
We can use it as the parameter to solve
the _deterministic_ optimization problem corresponding to each scenario,

$$
  \begin{array}{rl}
    Q^s(x) = \min\limits_{y} & c_2^s(y) \\
    \textrm{s.t.}   & (x, y) \in Y^s.
  \end{array}
$$

The expectation of the optimal value and dual variable define a tight cut for $\E{Q}$,
which we include into $\Cuts$.
In code, the procedures for calculating a new cut looks like this.

```julia
function average_cut(prog, x)
  v, y, λ = [], [], []   # value, solution, multiplier

  for s in prog.Scenarios
    v[s], y[s], λ[s] = solve(prog, s, x)
  end

  return Cut(mean(v), mean(λ), x)
end
```

To iteratively solve a stochastic program,
the algorithm alternates between two steps,
which correspond to how the information propagates between the stages:

- Solve the first stage and propagate the primal solution $x$ _forwards_ to the second stage;
- Solve the second stage for each scenario and propagate the average cut _backwards_ to the first stage.

Graphically, we visualize this process as the decision tree below.
The nodes are optimization problems and the edges represent their communication.
We represent $\Qfrak$ by a small diamond,
which is the only thing the first stage's node sees of the future.

```tikz {id="figure-tree-singlecut" png=true}
% First stage
{ [svgclass=stage1]
  \node[opt]   (t1) {};
}

% Cut pool

{ [svgclass=cut-pool, pin distance=1cm]
  \node[clink] (m) [right = 0.5cm of t1, pin={[pin edge={<-}]100:$\Qfrak$}] {};
}

% Second Stage
{ [svgclass=stage2]
  \matrix [matrix of nodes,
           nodes in empty cells,
           every node/.style = {opt},
           row sep = 4mm,
          ] (t2) [right = 2cm of m] {
      \\  \\  \\  \\
  };
}

% Connectivity
\draw (t1) -- (m)
      (m)  -- (t2-1-1)
      (m)  -- (t2-2-1)
      (m)  -- (t2-3-1)
      (m)  -- (t2-4-1);

% Overlays for animations
{ [svgclass=send-cut]
  \draw (m) -- (t2-1-1);
  \draw (m) -- (t2-2-1);
  \draw (m) -- (t2-3-1);
  \draw (m) -- (t2-4-1);
}

{ [svgclass=send-state]
\draw (t1) -- (m);
}
```

We repeat these forwards-backwards steps until the algorithm converges
to a solution for the original problem.
The only thing remaining is a stopping criterion for us to know when the solution is good enough.
For this, we use that we calculate both primal and dual solutions during the algorithm
to estimate upper and lower bounds over the real optimal.

- **Lower bound:** Since $\Qfrak \le \E{Q}$,
the calculated first stage value is below the real optimal value:
$$z(\Cuts) \le z^\star.$$

- **Upper Bound:** The calculated solutions $x, y^s$ for each stage are feasible,
thus their cost must be above the true minimum:
$$ c_1(x) + \E{c_2(y)} \ge  z^\star.$$

These bounds "sandwich" the true solution and,
when they're closer than a required tolerance,
we can consider our approximation good enough.
The code snippet below summarizes this whole procedure.

```julia
function solve_single_cut(stage1, stage2; tol = 1e-8)
  v2, y, λ = [], [], []   # Value, solution, multiplier
  ub, lb   = +Inf, -Inf

  while ub - lb > tol
    # First stage
    v1, x = solve(stage1)
    # Second stage
    for s in stage2.Scenarios
      v2[s], y[s], λ[s] = solve(stage2[s], x)
    end
    # Update approximation for E[Q]
    addcut!(stage1, Cut(mean(v2), mean(λ), x))
    # Check the bounds
    lb = opt1
    ub = stage1.cost(x) + mean(v2)
  end

  return x, y   # Calculated optima
end
```


### A Note on Parallelism

Notice on the algorithm above that the second-stage scenarios are independent of each other.
Since solving optimization problems is no simple task,
one use this to distribute the work over multiple processors.

\def\ncores{4}

```tikz {id="figure-tree-singlecut-parallel"}
% First stage
{ [svgclass=stage1]
  \node[opt]   (t1) {};
}

% Cut pool
{ [svgclass=cut-pool]
  \node[clink] (m) [right = 0.5cm of t1] {};
}

% Second Stage
{ [svgclass=stage2]
  \matrix [matrix of nodes,
           nodes in empty cells,
           every node/.style = {opt},
           row sep = 4mm,
          ] (t2) [right = 2cm of m] {
      \\  \\  \\  \\
  };
}

% Connectivity
\draw (t1) -- (m)
  \foreach \i in {1,...,\ncores} {
    (m) -- (t2-\i-1)
  };

% Overlays for animations
{ [svgclass=send-cut]
  \foreach \i in {1,...,\ncores}
    \draw (m) -- (t2-\i-1);
}

{ [svgclass=send-state]
\draw (t1) -- (m);
}

% Processors
\pgfonlayer{background}
    \node[draw, dotted, rectangle, rounded corners, opacity=0.8, fit=(t2-1-1) (t2-2-1)] (core1) {};
    \node[draw, dotted, rectangle, rounded corners, opacity=0.8, fit=(t2-3-1) (t2-4-1)] (core2) {};
\endpgfonlayer
```

The algorithm is not completely parallel though,
because the processors must synchronize their work to calculate the average cut.
While you can solve $|\Scenarios|$ optimization problems in parallel
in the second stage,
the process of gathering their results to produce a cut,
updating the first-stage problem and solving it to get a new solution is synchronous.
While only one processor runs this part, all others must wait.

In the next section we will learn a more flexible representation for the cost-to-go
that, at the cost of more memory usage, overcomes this limitation.


Multicut Approach
-----------------

In the single cut approach,
we calculate one cut per iteration to directly approximate the cost-to-go $\E{Q}$.
Thus, in a sense, approximating the average is the second stage's job:
the first stage never sees any information regarding the different scenarios,
it already comes aggregated to it.

Another possibility is to use a __multicut approximation__
where we keep a pool of cuts $\Cuts^s$ for each scenario
and construct separate approximations

$$ \Qfrak^s(x) = \max\limits_{(b, \lambda) \in \Cuts^s} b + \inner<\lambda, x>.$$

Now it is the first stage's job to take these approximations
and convert into an approximation to $\E{Q}$.
We do this by noticing that

$$ \Qfrak^s \le Q^s \implies \sum_{s \in \Scenarios} p_s \Qfrak^s \le \E{Q}$$

And plugging this whole average expression into our first stage approximation.

$$
  \begin{array}{rrl}
    z(\Cuts) &= \min\limits_{x}   & c_1(x) + \sum_{s \in \Scenarios} p_s \Qfrak^s(x) \\
      &\textrm{s.t.}  & x \in X \\
      &= \min\limits_{x,t_s} & c_1(x) + \sum_{s \in \Scenarios} p_s t_s \\
      &\textrm{s.t.}  & x \in X, \\
      &               & t_s \ge b + \inner<\lambda, x>,\; \forall s \in \Scenarios, (\lambda, b) \in \Cuts^s.
  \end{array}
$$

The multicut approach defines a larger optimization problem
than the one we had for the single cut,
because it unlinks the cost-to-go for each scenario.


```tikz {id="figure-tree-multicut"}
% First stage
{ [svgclass=stage1]
  \node[opt]   (t1) {};
}

% Second Stage
{ [svgclass=stage2]
  \matrix [matrix of nodes,
           nodes in empty cells,
           every node/.style = {opt},
           row sep = 4mm,
          ] (t2) [right = 2.5cm of t1] {
      \\  \\  \\  \\
  };
}

{ [svgclass=cut-pool]
  \path (t1) -- (t2-1-1) node[clink, midway] (m1) {}
        (t1) -- (t2-2-1) node[clink, midway] (m2) {}
        (t1) -- (t2-3-1) node[clink, midway] (m3) {}
        (t1) -- (t2-4-1) node[clink, midway] (m4) {};
}

% Connectivity
\graph {
  (t1) -- {
    (m1) -- (t2-1-1),
    (m2) -- (t2-2-1),
    (m3) -- (t2-3-1),
    (m4) -- (t2-4-1),
  }
};

% Overlays for animations
{ [svgclass=send-state]
  \foreach \c in {1,...,\ncores} {
    \draw (t1) -- (m\c);
  }
}

{ [svgclass=send-cut]
  \foreach \c in {1,...,\ncores} {
    \draw (m\c) -- (t2-\c-1);
  }
}
```

We can use this structure to build a similar algorithm to our previous one.
The main difference is that the multicut solver adds the cuts immediately after completing each scenario.

```julia
function solve_multicut(stage1, stage2; tol = 1e-8)
  v2, y, λ = [], [], []   # Value, solution, multiplier
  ub, lb   = +Inf, -Inf

  while ub - lb > tol
    # First stage
    v1, x = solve(stage1)
    # Second stage
    for s in stage2.Scenarios
      v2[s], y[s], λ[s] = solve(stage2[s], x)
      # Update approximation for E[Q[s]]
      addcut!(stage1, s, Cut(v2[s], λ[s], x))
    end
    # Check the bounds
    lb = opt1
    ub = stage1.cost(x) + mean(v2)
  end

  return x, y   # Calculated optima
end
```

### Flexibility and parallelism

An advantage of representing each scenario separately in the first-stage is the flexibility it gains us.
Suppose, for example, that there is a scenario with a value function that's more complicated than the others.
In the single cut approach we have to solve all scenarios to obtain an average cut,
and, therefore, cannot focus on the problematic one.
With separate cut pools, on the other hand, it is possible to adapt the algorithm
to sample more often the scenarios known to be harder.

This idea is particularly useful when parallelizing the algorithm
because it doesn't generate a synchronicity bottleneck for calculating average cuts.
We could, for example, assign some scenarios for each worker
and a processor responsible for solving the first stage from time to time.
As soon as a worker produces a cut, it can send it over to the first stage
to improve its approximation without affecting the other processors.

```tikz {id="figure-tree-multicut-parallel"}
% First stage
{ [svgclass=stage1]
  \node[opt]   (t1) {};
}

% Second Stage
{ [svgclass=stage2]
  \matrix [matrix of nodes,
           nodes in empty cells,
           every node/.style = {opt},
           row sep = 4mm,
          ] (t2) [right = 2.5cm of t1] {
      \\  \\  \\  \\
  };
}

{ [svgclass=cut-pool]
  \path (t1) -- (t2-1-1) node[clink, midway] (m1) {}
        (t1) -- (t2-2-1) node[clink, midway] (m2) {}
        (t1) -- (t2-3-1) node[clink, midway] (m3) {}
        (t1) -- (t2-4-1) node[clink, midway] (m4) {};
}

% Connectivity
\graph {
  (t1) -- {
    (m1) -- (t2-1-1),
    (m2) -- (t2-2-1),
    (m3) -- (t2-3-1),
    (m4) -- (t2-4-1),
  }
};

% Overlays for animations
{ [svgclass=send-state]
  \foreach \c in {1,...,\ncores} {
    \draw (t1) -- (m\c);
  }
}

{ [svgclass=send-cut]
  \foreach \c in {1,...,\ncores} {
    \draw (m\c) -- (t2-\c-1);
  }
}

% Processors
\pgfonlayer{background}
    \node[draw, dotted, rectangle, rounded corners, opacity=0.8, fit=(t2-1-1) (t2-2-1)] {};
    \node[draw, dotted, rectangle, rounded corners, opacity=0.8, fit=(t2-3-1) (t2-4-1)] {};
\endpgfonlayer
```


When the Future is Non-Convex {#ncvx}
=============================

It is time to leave the peaceful and colorful land of convexity
to enter the dark and haunted land of general, not necessarily convex, functions.
Recall from the [previous post](/posts/cuts) that for an arbitrary deterministic function,
the best cut we can get is only tight for its convex relaxation (dual function).
As will see, this makes the story a bit more complicated.

First and foremost, we must modify our random cut inequality
to take the dualization into account:[^dual-random]

$$ Q(x) \ge \check{Q}(x_0) + \inner<\Lambda, x - x_0>.$$

[^dual-random]: We define the dual $\check{Q}$ of a random function $Q$
as its dual for each scenario: $(\check{Q})^s = \widecheck{(Q^s)}$.
This is just a notation to make everything cleaner.

By using the previous section's approach, we decompose $Q(x_0)$ in scenarios
and calculate the average cut, which is an underapproximation for the expected function,

$$ \E{Q}(x) \ge \E{\check{Q}}(x_0) + \inner<\E{\Lambda}, x - x_0>.$$

<figure id="figure-cuts-conv" class="diagram-container">
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

Despite being a valid cut, it is, in general, _not tight_ --- not even for the dual.
As a matter of fact, it is not the best possible cut
because by decomposing $Q$ in scenarios, we ended losing information about how the non-convexities interact.
The problem is that we end up calculating cuts for the average of duals $\E{\check{Q}}$
while the cuts we really want are for the dual of the averages $\widecheck{\E{Q}}$.
This remark is important enough to deserve its own lemma.

:::{.Lemma data-title="Convexification by Averages"}
The average of convex relaxations is less than the average's relaxation.

$$ \E{\check{Q}} \le \widecheck{\E{Q}} \le \E{Q}.$$
:::

:::Proof
$\E{\check{Q}}$ is a convex underapproximation of $\E{Q}$ because
the expected value preserves inequalities and convexity:

- **Convex**: Each $\check{Q}$ is convex, and their average $\E{\check{Q}}$ is a non-negative linear combination of them;
- **Underapproximation:** $\check{Q} \le Q \implies \E{\check{Q}} \le \E{Q}$.

However, the convex relaxation $\widecheck{\E{Q}}$ is defined as the _largest_ convex underapproximation of $Q$.
Therefore, it is everywhere above $\E{\check{Q}}$.
:::

<figure id="figure-econv" class="diagram-container">
  <caption>
    We illustrate this theorem in the figure below,
    where you can see the ordering among the functions.
  </caption>
  <svg class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

Despite being short,
there is a lot to unpack in the theorem above.[^jensen]
One interpretation of it --- which we already remarked ---
is that each non-convex $Q^s$ has rims and bumps in different points.
By taking the average, these corners may cancel out
leaving $\E{Q}$ closer to being convex than it would appear by looking at each scenario separately.
In fact, in some special situations, the average could even be convex!


```tikz
{ [yscale = 1.5]
  \coordinate (A) at (0,0);

  \draw (A) -- ++(2,-2) -- ++(1,1) -- ++(1,-1) -- ++(2,2);
  \draw[color=green!80] ([xshift=1cm]A) -- ++(2,-2) -- ++(1,1) -- ++(1,-1) -- ++(2,2);

  \draw ([xshift=8cm]A)++(0.5, 0) -- ++(1.5,-1.5) -- ++(3,0) -- ++ (1.5,1.5);

  \draw[->, bend right] (6.5, -1) to[out=30, in=150] node[above] {$\mathbb{E}$} ++(2.5,0);
}
```

[^jensen]: An abstract view of this theorem
is that at each point $x$, the mapping $f \mapsto \check{f}(x)$
is a concave function.
What we just proved is the pointwise Jensen's inequality for it.

<!-- Another view is that the operators $Q \mapsto \check{Q}$ and $\mathbb{E}$ do not commute -->

For solving optimization problems,
the important part is the theorem's consequence for cuts.
To approximate the expected cost-to-go as tightly as possible,
we need a way to directly approximate its convex relaxation.

Linked formulation
------------------

Both the single and multicut approaches,
when applied to a non-convex second stage
only yield cuts for $\E{\check{Q}}$,
because they calculate an average cut by decomposing the problem into scenarios.
To get tighter cuts,
we need an approach that takes into account all cuts at the same time.

The idea is to build one huge optimization problem representing the entirety of $\E{Q}$
by linking all $Q^s$ together.
Start by recalling the definition of the problem for each scenario,

$$
  \begin{array}{rl}
    Q^s(x) = \min\limits_{y^s} & c_2^s(y^s) \\
    \textrm{s.t.}   & (x, y^s) \in Y^s,
  \end{array}
$$

Where we purposefully write $y^s$ for the decision variable
to mark its dependence on the scenario.
The expected cost-to-go is

$$ \begin{aligned}
  \E{Q}(x) &= \sum_{s \in \Scenarios} p_s Q^s(x) \\
  & = \begin{array}{rl}
    \sum\limits_{s \in \Scenarios} p_s \min\limits_{y^s} & c_2^s(y^s) \\
    \textrm{s.t.}   & (x, y^s) \in Y^s.
  \end{array}
\end{aligned}
$$

Since the constants $p_i$ are non-negative and independent from the minimization,
we can effectively "commute" the expectation with the minimum to arrive at an optimal value function.

$$
\boxed{
\begin{array}{rl}
  \E{Q}(x) = \min\limits_{y^s} & \sum\limits_{s \in \Scenarios} p_s c_2^s(y^s) \\
    \textrm{s.t.}   & (x, y^s) \in Y^s.
\end{array}
}
$$

This is the __linked formulation__ for the expected value of an optimal value function.
Since it is a huge non-convex program, rest assured that it can be slow as hell to solve.
Nevertheless, one can use this formulation's dual to calculate the tightest cut possible for $\E{Q}$,
i.e., any standard method for tight cuts will return a multiplier $\lambda$ satisfying[^lagrangian]

$$ \E{Q}(x) \ge \widecheck{\E{Q}}(x_0) + \inner<\lambda, x - x_0>.$$

[^lagrangian]: For example, when the non-convexities arise from integer variables, you can use [Lagrangian or Strengthened Benders cuts](/posts/cuts-mip).

We can, therefore, use this cost-to-go formulation
to build a stochastic programming algorithm.
In comparison to the previous decomposed formulations,
in this case we represent the second stage as a single node containing all scenarios.

<!-- Linked Cut -->
```tikz {id="figure-tree-linked"}
% First stage
{ [svgclass=stage1]
  \node[opt]   (t1) {};
}

% Cut pool
{ [svgclass=cut-pool]
  \node[clink] (m) [right = 0.5cm of t1] {};
}

% Second Stage
{ [svgclass=stage2]
  \matrix [matrix of nodes,
           nodes in empty cells,
           draw, rectangle, rounded corners,
           every node/.style = {opt},
           row sep = 1mm,
          ] (t2) [right = 2cm of m] {
      \\  \\  \\  \\
  };
}

% Connectivity
\draw (t1) -- (m) -- (t2);

{ [svgclass=send-state]
  \draw (t1) -- (m);
}
{ [svgclass=send-cut]
  \draw (m) -- (t2);
}
```

From the first stage's point of view,
this is indistinguishable from a single cut calculation,
because at each iteration we produce only one cut.
We also need to adapt the stopping criterion
because, for non-convex problems, there is no guarantee of convergence.
There is hope to find a good solution after enough iterations, nonetheless.

```julia
function solve_linked(stage1, stage2; tol = 1e-8, max_iterations :: Int)
  ub, lb   = +Inf, -Inf
  iter     = 1

  while ub - lb > tol || iter > max_iterations
    # First stage
    v1, x = solve(stage1)
    # Second stage
    v2, ys, λ = solve(linked(stage2), x)   # Solve linked formulation for 2nd stage
    # Update approximation for E[Q]
    addcut!(stage1, Cut(v2, λ, x))
    # Convergence criteria
    lb    = opt1
    ub    = stage1.cost(x) + v2
    iter += 1
  end

  return x, ys  # Calculated optima
end
```

Notice from this code that we lost the embarrassingly parallel nature of the second stage.
Although the linked formulation could gain from parallelism in the solver,
there is no room for parallelizing directly in the scenarios.
The second stage becomes a big opaque block.

Mixing and Matching Approaches
------------------------------

Before we close this section,
it is worth comparing the linked versus the decomposed approach.
As is common in optimization, when the problem at hand is not convex,
we end up in a dilemma of speed against accuracy.[^mip-cuts]
Fortunately, methods never get jealous and one does not need to commit to any single one.

[^mip-cuts]: Much rather the decision of [Benders vs Lagrangian cuts for MIP](/posts/cuts-mip).

Let's take a high-level look into how to combine our many approaches.
First, remember that we aren't really interested in the cuts themselves.
They are only a tool towards our real objective: an (approximately) optimal solution $(x, y)$.
So, we can play a bit with how to represent the cost-to-go.
An idea, thus, is to solve using a decomposed formulation but,
once in a while --- when the solution stagnates, for example ---
calculate a linked cut to improve our overall search space.

Another more sophisticated approach would be to take advantage of a parallel machine
to calculate linked and decomposed cuts concurrently.
It will need at least three workers:
one for the first stage, one for the linked second stage,
and the others for the scenarios.

First, notice that we can write the first stage in a way that accepts both single and multicuts.
By denoting $\Cuts^s$ the cut pool for scenario $s$ and $\Cuts^\mathbb{E}$ the pool of aggregated cuts,
we represent the first stage as

$$
\begin{array}{rl}
   \min\limits_{x,t, t_s} & c_1(x) + t \\
   \textrm{s.t.}  & x \in X, \\
                  & t = \sum_{s \in \Scenarios} p_s t_s \\
                  & t \ge b + \inner<\lambda, x>,\; \forall (\lambda, b)\in \Cuts^\mathbb{E} \\
                  & t_s \ge b_s + \inner<\lambda_s, x>,\; \forall s \in \Scenarios, (\lambda_s, b_s) \in \Cuts^s.
\end{array}
$$

The variable $t$ represents the expected cost-to-go
and, for consistency, is constrained to equal the average of the scenariowise cost-to-go.
Since the second stage decisions do not communicate among themselves,
the graph for this mixed approach can have separate nodes representing each cut method.
In the figure, we illustrate an approach that concurrently calculates linked and multicuts.

<!-- Parallel Mixed Approach -->
```tikz {id="figure-tree-mixed"}
% First stage
{ [svgclass=stage1]
  \node[opt]   (t1) {};
}

% Cut pool
{ [svgclass=cut-pool-E]
  \node[clink] (m) [right = 0.5cm of t1] {};
}

% Decomposed
{ [svgclass=stage2-decomposed]
  \matrix [matrix of nodes,
           nodes in empty cells,
           every node/.style = {opt},
           row sep = 3mm,
          ] (t2) [right = 1.4cm of m] {
      \\  \\  \\  \\
  };
}

% Linked
{ [svgclass=stage2-linked]
  \matrix [matrix of nodes,
           nodes in empty cells,
           draw, rectangle, rounded corners,
           every node/.style = {opt},
           column sep = 1mm,
          ] (linked) [below = 1.4cm of t1] {
      & & & \\
  };
}

{ [svgclass=cut-pool]
  \path (m) -- (t2-1-1) node[clink, midway] (m1) {}
        (m) -- (t2-2-1) node[clink, midway] (m2) {}
        (m) -- (t2-3-1) node[clink, midway] (m3) {}
        (m) -- (t2-4-1) node[clink, midway] (m4) {};
}

% Connectivity
\graph {
  (t1) -- (m) -- {
    (m1) -- (t2-1-1),
    (m2) -- (t2-2-1),
    (m3) -- (t2-3-1),
    (m4) -- (t2-4-1),
  }
};

\node (imp) [below = of m] {};
\draw (m.south) -| (imp.center) -| (linked.north);

{ [svgclass=send-state]
  \draw (t1) -- (m);
}
{ [svgclass=send-cut-linked]
  \draw (m.south) -| (imp.center) -| (linked.north);
}
{ [svgclass=send-mid]
  \foreach \c in {1,...,\ncores} {
    \draw (m) -- (m\c);
  }
}
{ [svgclass=send-cut-mcut]
  \foreach \c in {1,...,\ncores} {
    \draw (m\c) -- (t2-\c-1);
  }
}
```

<script type="module">
  import * as figures from "./figures.js";
  figures.main();
</script>
