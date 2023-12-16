---
title: Approximation by a Thousand Cuts
subtitle: An interactive guide to polyhedral representations
keywords: [math]
date: 2023-09-22
suppress-bibliography: true
---

<style>
/* CSS for styling */
.diagram-container {
  flex: auto 1 1;
  max-width: 100%;
}

svg.diagram {
  width: 100%;
  height: 100%;
  background-color: hsl(147 0% 96%);
  border-radius: 15px;
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

.not-good {
  fill: #de82a2;
}

.hyperplane {
  stroke: rgb(255, 165, 0);
  stroke-width: 1pt;
}

.mark {
  fill: rgb(255, 165, 0);
}

.half-space {
  fill: rgba(255, 165, 0, 0.5);
}

.function-graph {
  stroke: black;
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

.draggable {
  cursor:move;
}

.draggable:hover {
  cursor:move;
  fill: #ffd700;
}
</style>

\def\R{\mathbb{R}}
\def\hole\cdot
\def\inner<#1,#2>{\left\langle#1,\,#2\right\rangle}
\def\op#1{\operatorname{\mathrm{#1}}}
\def\graph{\op{graph}}
\def\epi{\op{epi}}
\def\d#1{\op{d}\!{#1}}

As the late hours of the night embrace you,
you find yourself seated at your workbench,
the substantial dose of caffeine you recently consumed
still coursing through your veins with unwavering energy.
Then, in a sudden sparkle of inspiration,
you stumble upon it--—the elusive function you so much needed.
It's a remarkable little function $f : \R^n \to \R$,
that fits perfectly with all the simulations you intend to run.

You already envision it:
the number crunching, the evaluations, the convergence.
There is a problem, however.
This function, you realize, is as real-valued as it gets,
constructed with infinite precision,
while all your code and simulations reside
in the digital world of a computer's memory,
capable of storing only a finite number of bits.
What can one do in such a situation?

Fortunately, mathematicians all over the world
have already studied a myriad of methods for approximating functions in a computer.
The answer depends, most of all, on what your objectives are,
because each kind of approximation only preserves some characteristics,
and is, thus, only appropriate for certain applications.
If being accurate in some known points is your primary concern,
polynomial interpolation may be your answer.
On the other hand, if you worry about extrapolating into new datasets,
a neural network might prove to be a better fit.
Or if your aim is not evaluation but measurement through filters,
projection into the Fourier domain or another functional basis could be the path to pursue.

In this post,
we will explore _approximations by cuts_,
a technique for representing functions that is suitable for optimization problems.
This is a well-known tool within the Operations Research community and,
I believe, is also full of potential in various other domains of engineering and mathematics.

Before we delve deeper into the world of cuts,
you can stop and play with the interactive diagram below.
Each click on a point in the first panel improves the approximation by cuts on the other one.

```{=html}
<div>
  <div id="function-cuts" class="multi-figure-container">
    <figure class="diagram-container">
    </figure>
    <figure class="diagram-container">
    </figure>
  </div>
  <br/>
  <button id="reset-function-cuts" type="button">Clear cuts</button>
</div>
```


Approximation by cuts
=====================

Cutting planes have widespread usage on the Operations Research community
because they play really well with optimization problems.
Nevertheless, I unfortunately don't see them being used quite as much in other branches
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


```tikz {name="function-tight-cuts"}
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
The magic begins when you combine many cuts to represent $f$ as a _polyhedral function_.
Taking an approximation of $f$ by cuts $C = \{\inner<a,\hole> + b\}$
amounts to choosing the largest cut at each given point:

$$ f(x) \approx \max\limits_{(a,b) \in C} \inner<a_,x> + b. $$

This formula also explains why we refer to affine functions as "cuts".
Think about the graph of $f$.
The graph of a cut is a hyperplane dividing the space in two halves:
points above and below it.
To form the polyhedral function, we slice the space, one cut at a time,
in order to carve a polyhedron containing the graph of $f$.

Click anywhere in the figure below to carve the space
until the function's shape is recognizable.
You start with no cut and, as you click, the polyhedral approximation
improves until the function's graph become recognizable.
A fun game is to try to do this with the least amount of cuts possible.

<figure class="diagram-container">
  <svg id="function-epigraph-carving" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <button id="reset-epigraph-carving" type="button">Clear cuts</button>
</figure>

Why I like cuts and you should too
----------------------------------

What are the nice properties that an approximation by cuts has?
Why is it used so much by the optimization and operations research folks?
Let's go through some of the reasons.

- **Underapproximation**:
    Since all cuts are below $f$, their maximum also has to be.
    Thus, cutting planes are a good tool for estimating lower bounds,
    which is a necessity in optimization.

- **Easy to improve**:
    Approximations by cuts can be refined without much hassle.
    Whenever you obtain a new cut $\phi$, all you have to do is update your bag of cuts
    from $C$ to $C \cup \{\phi\}$, and it's done.
    This allows for algorithms based on iterative refinement of the approximation.

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

- **Linear Programming formulation**:
    Minimizing a polyhedral function is among the easiest kinds of problems to optimize,
    because it can be represented as a
    [Linear Program](https://en.wikipedia.org/wiki/Linear_programming),
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
which is a necessity for any optimizer that "sandwiches" the optimal value
between two bounds.
Let's now put all these properties to good use.

### Example: Kelley's cutting planes algorithm {#example-sandwich-algorithm}

Imagine that you must minimize a convex function,
but, unfortunately,
you only have access to a linear programming solver such a Gurobi, Xpress or GLPK.
How do you do that?

Fortunately, one can minimize an arbitrary convex function
by iteratively solving linear programs.
For it to work, we will have to suppose (by now)
that we have access to some oracle capable of both evaluating a function and calculating a tight cut for it.
This is not cheating though,
because in the section [Cuts from Derivatives] we will learn how to write such an oracle.

Besides the function $f$, the algorithm needs three more inputs:
a tolerance $\epsilon$, an initial point (can be any point) to evaluate,
and a uniform bound $\forall x,\, M \le f(x)$.
Since we are minimizing, this bound must exist for the problem to be well-posed.
We begin with a polyhedral approximation $\tilde{f}_0$ which equals $M$ everywhere.
The idea is to, at each iteration, give the oracle a point $x_n$.
This will return an evaluation $f(x_n)$, which serves as an upper bound
for $\min f$ because all evaluations are larger than the minimum.
You also get a tight cut $c_n$ that can be used to improve your approximation to $\tilde{f}_n$.
This way, we squeeze the optimal value between a sequence of bounds:

$$ \min_n f(x_n) \ge \min_x f(x) \ge \min_x \tilde{f}_n(x). $$

The algorithm can also be more directly described in Julia code:

```julia
function minimize(f::Function, initial_bound, x = 0; tolerance)
  opt = Polyhedral(initial_bound)
  ub  = +Inf
  lb  = initial_bound

  while ub - lb > tolerance
    # Ask oracle for evaluation and cut at x
    fx, cut = oracle(f, x)

    # Improve upper bound
    ub = min(ub, fx)

    # Improve lower bound
    add_cut!(opt, cut)
    x, lb = solve_lp(opt)   # Also updates the best solution
  end

  return x, fx              # Optimal point and optimal value
end
```

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

My intuition for convex sets is that they are sets without holes nor wobbles.
This encompasses a lot of the good ol' shapes you are familiar with,
such as planes, balls, and polyhedra.


:::multi-figure-container
```tikz {class=diagram-container}
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

```tikz {class=diagram-container}
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
:::

In this post, we will also talk a lot about hyperplanes and their relation to convex sets.
It is useful to think about a hyperplane interchangeably as a geometrical and an analytical object.
We can view it as an affine function's zero set

$$H = \{ x \mid \inner<a,x> + b = 0 \}$$

Or as the affine function itself.
The important part is that a hyperplane divides the space in two halves,
depending on whether the affine function is non-negative or not.

This division is specially important for convex sets,
because, as a consequence of their regular shape,
one can always pass a hyperplane between them
in such a way that each lies in one of the halves.

::: {.Theorem data-title="Separating Hyperplane"}
For any pair of disjoint non-empty convex sets $X$, $Y$,
there is a hyperplane dividing the space in a way that
each one lies in one of its sides.

In other words, there exists an affine function $\inner<a,\hole> + b$
such that for all $x \in X$ and $y \in Y$,
$$\inner<a,x> + b \le 0 \quad \text{and}\quad \inner<a,y> + b \ge 0.$$
:::

The proof to this result is more technical than elucidating[^hahn-banach],
so we are going to skip over it on this post.
You can find a great presentation with all details on @{boyd_convex_2004}'s book.
Fortunately, this is one of these results were we can get a good intuition from a picture:
due to their absence of wobbles, one can pass a plane between two convex sets without bumping into anything.

Have fun trying to drag the sets below to a position with no separating hyperplane.

[^hahn-banach]: For the Functional Analysts lurking around: in infinite dimension,
it is equivalent to the Hahn-Banach Theorem.

<figure class="diagram-container">
  <svg id="set-separating-hyperplane" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

Let's turn our focus to the case where one of the sets is a single point.
An interesting consequence of this theorem is that given any point $p$ not in the set,
we can find an affine function that is zero in this point and contains
a convex set in one of its half-spaces.
It is really similar to a cut, but for a set.
For example, in the image below, you can choose a separating hyperplane between
your mouse and the convex set in the middle.

<figure class="diagram-container">
  <svg id="set-point-hyperplane" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>


Now, what happens when we choose our point to be in the set's boundary?
In this case, we acquire a _supporting hyperplane_.
That is, a tangent hyperplane that has the entire set in one of its sides.

:::Definition
A **supporting hyperplane** at a point $x$ in the boundary of $X$
is a tangent hyperplane that touches $X$ at $x$ and is
non-negative at every other point of $X$.
:::

This is starting to look a bit like cuts, right?
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
If $X$ has nonempty interior, there is a hyperplane separating $\{x_0\}$ from the interior of $X$,
and if it is empty, the entirety of $X$ lies inside in an affine subspace of $\R^n$,
and we can take this subspace as our hyperplane.
:::

Notice that this theorem has the equivalent formulation as
$$\forall x \in X,\, \inner<a,x> \ge \inner<a,x_0>.$$

<figure class="diagram-container">
  <svg id="set-supporting-hyperplane" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

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

$$ f\left(\lambda x + (1 - \lambda) y\right)  \le \lambda f(x) + (1 - \lambda) f(y).$$

Nonetheless, convex functions are notorious for having a multitude of equivalent definitions.
Hence, for our purposes it will be better to choose a more geometric one.

:::Definition
A function is **convex** if its epigraph is a convex set.
:::

But what is this _epigraph_ thing after all?
Well, everybody knows a function's graph,

$$ \graph(f) = \left\{ (x, y) \in \R^{n+1} \mid f(x) = y \right\}. $$

The epigraph adjoins the suffix _epi-_, from Greek ['ἐπί']{lang=grc}
meaning ["above" or "on top of"](https://outils.biblissima.fr/fr/eulexis-web/?lemma=%E1%BC%90%CF%80%CE%AF&dict=LSJ),
to denote the set of all points _above the graph_.

$$ \epi(f) = \left\{ (x, y) \in \R^{n+1} \mid f(x) \le y \right\}. $$

<figure class="diagram-container">
  <svg id="function-epigraph" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

Besides sharing an etymology with _epic_,
the epigraph also lets us easily translate results from convex sets to convex functions.
For example, the supporting hyperplane theorem translates into a statement about tight cuts for convex function!

:::Theorem
A convex function $f$ has a tight cut at any point in its domain.
:::

Visually this theorem looks like the figure below.
You can hover it to view the cut for each point.

<figure class="diagram-container">
  <svg id="function-supporting-cut" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

:::Proof
The graph of $f$ lies in the boundary of its epigraph, which is convex.
Therefore, from the supporting hyperplane theorem, for any $x_0$,
the epigraph of $f$ has a supporting hyperplane touching it at $(x_0, f(x_0))$.
Since the tangent to a graph is non-vertical,
the last component of this hyperplane cannot be zero.
We can represent this supporting hyperplane as

$$ \begin{aligned}
  \inner<a,x> + bf(x) &\ge \inner<a,x_0> + bf(x_0) \\
  f(x) &\ge f(x_0) + \inner<-\frac{{1}}{b} a,x - x_0>.
\end{aligned} $$

By defining $\lambda = -\frac{{1}}{b} a$, we arrive at

$$f(x) \ge f(x_0) + \inner<\lambda,x - x_0>.$$
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
function oracle(f, a)
  fa  = f(a)
  dfx = derivative(f, a)

  return fa, Cut(a, fa, dfa)    # Cut(x) = fa + dot(dfa, x - a)
end
```

Cuts from Lagrangian duality {#lagrangian-duality}
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
There is a small trick, however, that lets us write any function
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

The advantage of concentrating on optimal value functions
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
    L(x; \lambda) = \min\limits_{u, y} & c(u) + \inner<\lambda, x - y> \\
    \textrm{s.t.}   & (y, u) \in X.
  \end{array}
$$

While in the original problem $y$ was fixed to whatever argument the function receives,
in the new problem it can be any feasible value.
Nonetheless, the cost is augmented by a new penalty term on the difference between $y$ and $x$.
The factor $\lambda$ is called a _dual variable_ or _Lagrange multiplier_
and represents how much $y$ is averse to diverting from $x$.

In the original feasible set where $x = y$,
the term $\inner<\lambda, x - y>$ vanishes and both problems have the same objective function.
However, the relaxed problem has a larger feasible region to explore in its search for the minimum,
allowing it the opportunity to achieve lower costs.
In other words, the Lagrangian is an underapproximation to the primal problem
whenever it is feasible for $x$:

$$ f(x) \ge L(x; \lambda). $$

Also notice that the optimization procedure is itself no longer dependent on $x$,
and we can remove it from the objective value.

$$
  \begin{array}{rl}
    L(x; \lambda) = \inner<\lambda, x> + \min\limits_{u, y} & c(u) - \inner<\lambda, y> \\
    \textrm{s.t.}   & (y, u) \in X.
  \end{array}
$$

Thus, for each fixed $\lambda$, the relaxation $L(\cdot; \lambda)$ is an affine function.
So, do affine functions everywhere below $f$ make you think of something?

In general, we are interested in cuts centered around a point, say $x_0$.
Let's add and subtract $\inner<\lambda, x_0>$ from the relaxation's definition
to put it in this form.

$$
  \begin{array}{rl}
    L(x; \lambda) = \inner<\lambda, x - x_0> + \min\limits_{u, y} & c(u) + \inner<\lambda, x_0 - y> \\
    \textrm{s.t.}   & (y, u) \in X.
  \end{array}
$$


Notice that the minimization on the right-hand side
is precisely the relaxation at the point $x_0$.
Despite being always true for affine functions,
it still yields a neat relation for the relaxation,

$$ L(x; \lambda) =  L(x_0; \lambda) + \inner<\lambda, x - x_0>. $$

Using that $L$ is always an underapproximation for $f$,
we get to the desired cut equation:

$$ f(x) \ge L(x; \lambda) =  L(x_0; \lambda) + \inner<\lambda, x - x_0>. $$

<figure class="diagram-container">
  <svg id="function-lagrangian" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
  <input type="range" id="slider-lagrangian-lambda" min="-5" max="5" step="0.1" value="1" style="width: 100%"/>
  <label for="slider-lagrangian-lambda"> Multiplier <span id="slider-lambda-value">$\lambda = 1$</span>.</label>
</figure>

Notice in the figure above that for many points,
there is some multiplier $\lambda$ for which the relaxation touches the primal function.
Unfortunately, we don't have control of where this happens,
because we first fix the inclination and only then solve the relaxation.
Hence, a natural question to ask is, given a parameter $x$,
what inclination yields the tightest relaxation at this point?
This is called the _dual value function_ and amounts to a minimax problem:

$$
\begin{aligned}
  \check{f}(x) &= \max\limits_{\lambda} L(x ; \lambda) \\
               &= \max\limits_{\lambda} \min\limits_{(y, u) \in X} c(u) + \inner<\lambda, x - y>.
\end{aligned}
$$

That thing looks scary but, fortunately for us, all solvers are pretty good at it.
In fact, for the most common types of optimization programs,
the dual problem has a known closed form
and all state-of-the-art algorithms in (continuous) optimization[^market-opt]
solve for the primal and the dual problems at the same time.
Who would say that optimizing two function is easier than a single one?

Since $\check{f}$ is the maximum of lower approximations to $f$,
it is also a lower approximation, a result known as _weak duality_.

$$ \boxed{f(x) \ge \check{f}(x)}$$

It is also guaranteed to be convex,
for it is the maximum of affine functions.
A convex lower approximation made of cuts, it looks like we are getting to something.
Moreover, one can prove that it is not just some ordinary convex function,
but the tightest convex function everywhere below $f$.
So the cuts for $\check{f}$ are also the best ones possible for $f$ at a certain point.
The best part is that these cuts come for free from solving the optimization problem
because they are formed by the dual value $\check{f}(x)$ and its associated multiplier $\lambda_x$.

[^market-opt]: And consequently all free or commercial solvers in the market.

<figure class="diagram-container">
  <svg id="function-lagrangian-dual" class="diagram" viewBox="-400 -200 800 400" width="100%" height="100%">
  </svg>
</figure>

:::Theorem
There is a cut for $f$ at the point $x_0$ defined by the dual value $\check{f}(x_0)$
and optimal dual multiplier $\lambda_0$. That is,

$$ \forall x, f(x) \ge \check{f}(x_0) + \inner<\lambda_0, x - x_0>.$$
:::

:::Proof
The dual value function is the maximum among all Lagrangian relaxations.
Let's look at it centered at $x_0$.

$$ \check{f}(x) = \max\limits_{\lambda} L(x_0; \lambda) + \inner<\lambda, x - x_0>.$$

The maximum above selects the best $\lambda$ for the chosen parameter $x$.
Therefore, its solution is above any other choice of $\lambda$ we can make,
including our desired $\lambda_0$.

$$ \check{f}(x) \ge L(x_0; \lambda_0) + \inner<\lambda_0, x - x_0>.$$

Now the term $L(x_0; \lambda_0)$ is the relaxation at $x_0$
with the optimal multiplier $\lambda_0$ at this point,
which is precisely the definition of $\check{f}(x_0)$.
This yields a tight cut for the dual function.

$$ \check{f}(x) \ge \check{f}(x_0) + \inner<\lambda_0, x - x_0>.$$

From weak duality, it follows that it is also a cut for $f(x)$.

$$ f(x) \ge \check{f}(x) \ge \check{f}(x_0) + \inner<\lambda_0, x - x_0>.$$
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
But remember that we discussed above that $\check{f}$ is the _tightest_ convex function
approximating $f$ from below.
Thus, they should be equal whenever $f$ is convex.
This is an important result called _strong duality_,
which states that $\check{f}$'s epigraph is the closed convex hull of $f$'s epigraph.[^lsc-cvx]
Therefore, now we also know how to calculate tight cuts
for the points of non-differentiability of a convex function!

$$ f\;\text{convex} \implies \forall x, f(x) \ge f(x_0) + \inner<\lambda_0, x - x_0>.$$

[^lsc-cvx]: Technically, the theorem requires $f$ to be a proper convex _lower semicontinuous_ function.
But in practice, convexity tends to be the only part you need to worry about.

As a bonus, if you like automatic differentiation as much as I do,
you also just learnt how to calculate the derivative for any function
performing convex optimization during its execution.
Suppose that $f$ is differentiable at $x_0$.
Since the derivative is unique and the cut provides us with a tangent,
it follows that the derivative equals the optimal multiplier $\nabla f(x_0) = \lambda_0$.
Thus, by solving a parameterized (convex) optimization problem,
you gain both an evaluation and a derivative.
Now all you have to do is plug it into the chain rule et voilà!

Farewell
========

Today you gained a new tool to your repertoire of functional representations.
We have learned how cutting planes are a computationally amenable structure
that harmoniously integrate with convexity and optimization,
and how we can use them to represent functions for minimization.
And, best of all, if you are working with convex optimization or calculating any derivatives,
you already have some cuts lying around for free!

I am also planning some further posts
were we will explore how to combine cuts with value and policy iteration
to extend dynamic programming to infinite dimensional state spaces,
and how to put to good use the various available ways to calculate cuts for mixed integer programs.

Good convergence for y'all and stay tuned!

Acknowledgements
================

I want to thank Ivani Ivanova and [Pedro Xavier](https://pedromxavier.github.io)
for commenting and proofreading this post's first draft.

<script type="module">
  import * as figures from "./figures.js";

  const f_ncvx = x => 0.5*(x - 1.5)*(x - 1)*(x + 0.5)*(x + 1.5);
  const f_cvx  = x => x*x+1;
  const df_cvx = x => 2*x;

  figures.figureFunctionCuts("#function-cuts", f_cvx, df_cvx, -2, 2);

  figures.figureFunctionEpigraphCarving("#function-epigraph-carving", f_cvx, df_cvx, -2, 2);

  figures.figureSetSeparatingHyperplane("#set-separating-hyperplane");

  figures.figureSetPointHyperplane("#set-point-hyperplane");

  figures.figureSetSupportingHyperplane("#set-supporting-hyperplane");

  figures.figureFunctionEpigraph("#function-epigraph", f_ncvx, -1.8, 2);

  figures.figureFunctionSupportingCut("#function-supporting-cut", f_cvx, df_cvx, -2, 2);

  figures.figureLagrangian("#function-lagrangian", f_ncvx, -1.8, 2);

  figures.figureLagrangianDual("#function-lagrangian-dual", f_ncvx, -1.8, 2);
</script>
