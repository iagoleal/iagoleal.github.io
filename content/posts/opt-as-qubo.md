---
title: Your Favorite Problem is an Ising Model
keywords: [math, optimization, quantum-computing]
date: 2025-10-21
thumbnail: ""
theme: math
description:
  Quantum Computers are advancing and one should know
  how to fit classical problems into it.
suppress-bibliography: true
css: "/css/plots.css"
---

```{=tex}
\pgfdeclarelayer{background}
\pgfdeclarelayer{foreground}
\pgfsetlayers{background,main,foreground}

\tikzset{
  atom/.pic={
      \node (atom) at (0, 0) {};
      \node[rectangle, shading=axis, right color=white, left color=gray!40, shading angle=180, draw, minimum width = 1.2cm, minimum height = 1.2cm, rounded corners] (qbox) at (atom) {};
      \node[circle, fill=black, inner sep = 0.7mm] at (atom) {};
      \path[draw]
        (atom) ellipse [x radius = 0.5cm, y radius = 0.2cm]
        (atom) ellipse [x radius = 0.5cm, y radius = 0.2cm, rotate = 60]
        (atom) ellipse [x radius = 0.5cm, y radius = 0.2cm, rotate = -60];
  }
}
```

Quantum computing is on the rise and with it the promise
of faster solvers for currently hard problems.
These machines come in many colours and flavours --- each with its own (dis)advantages ---
but for the folks in operations research there a kind that is particularly exciting:
the _Quantum Annealer_.

Forget about your Hadamard gates, circuits, and quantum Turing completeness.
For our purposes, the quantum annealer is a black box
capable of hastily solving the Ising model, a very specific kind of optimization problem.
Somewhat like the diagram below.


```tikz {tikzlibrary="quotes,decorations.markings,shapes.geometric"}
{ [every node/.style = {align = center},
   writeup/.style = { midway, above, align=center, font=\small },
  ]
  \node[minimum width=1.2cm] (q) at (0, 0) {};
  \pic at (q) {atom};

  \node[align=center] (i) at (-3, 0) {};
  \node[align=center] (s) at (+3, 0) {};

  { [thick,decoration={
      markings,
      mark=at position 0.5 with {\arrow{>}}}
      ] 
    \draw[postaction={decorate}]  (i.east) -- (q.west) node [writeup] {Ising\\Model};
    \draw[postaction={decorate}]  (q.east) -- (s.west) node [writeup] {Optimum};
  }
}
```

Today, I don't want to discuss how these solvers work[^later-post]
but rather the question of how to put them to good use.
After all, if the problems I care about are all classical,
what is the difference this quantum machine can in my life?
Since we are doing optimization,
let's focus on MILP --- a NP-hard problem that is everywhere ---
and see how a quantum computer to solve it.

$$
\begin{equation}
\tag{MILP}
  \begin{array}{rl}
    \min\limits_{x} & c^\top x \\
    \textrm{s.t.}   & A x = b, \\
                    & x \ge 0, \\
                    & x \in \R^m \times \Z^k.
  \end{array}
\end{equation}
$$

[^later-post]: But it may become the topic of a future post. Stay tuned!

Unfortunately, the Ising model is rather different
from the kind of optimization problems we generally want to solve.
Hence, to take advantage of these machines
it is necessary to employ a preprocessing step that "rewrites" our programs into the appropriate format.
This is today's theme.

  ```tikz {tikzlibrary="quotes,decorations.markings,shapes.geometric"}
{ [every node/.style = {align = center},
   writeup/.style = { midway, above, align=center, font=\small },
  ]
  \node[minimum width=1.2cm] (q) at (0, 0) {};
  \pic at (q) {atom};

  \node[rectangle, fill=green!40, draw=green!60, very thick, minimum height=1.2cm] (r) at (-3, 0) {Rewrite};
  \node[align=center] (i) at (-6, 0) {};
  \node[align=center] (s) at (+3, 0) {};

  { [thick,decoration={
      markings,
      mark=at position 0.5 with {\arrow{>}}}
      ] 
    \draw[postaction={decorate}]  (i.east) -- (r.west) node [writeup] {MILP};
    \draw[postaction={decorate}]  (r.east) -- (q.west) node [writeup] {Ising\\Model};
    \draw[postaction={decorate}]  (q.east) -- (s.west) node [writeup] {Optimum};
  }
}
```

Even if you haven't jumped on the quantum bandwagon,
these rewriting techniques can still be a useful addition to your toolbelt.
this post still should have something for you.
There are other (classical) hardware tailor-made to solving the Ising model,
such as [Fujitsu's Digital Annealers](https://www.fujitsu.com/us/services/business-services/digital-annealer/).
Or even algorithms and solvers for classical machines such as [Simulated Annealing](https://en.wikipedia.org/wiki/Simulated_annealing)
or my own [TenSolver.jl](https://github.com/SECQUOIA/TenSolver.jl).

The Ising Model in a Graph
==========================

The _Ising Model_ consists of a graph where each node $i$ is a particle
that can be in one of two states $s_i \in \{-1, +1\}$ called their _spin_.
The graph's edges represent an interacting pair of particles
and we say that they are _aligned_ whenever their spin is equal.
To make equations simpler,
alignment is concisely represented by the product $s_i s_j \in \{-1, +1\}$.

:::Missing
Figure with graph and spin system.
Every time you click in a node, the spin flips.
:::

For each edge there is an interaction energy $J_{ij} \in \R$
indicating how costly it is for them to remain aligned.
A positive factor $J_{ij} > 0$
means that the particles require energy to stay aligned,
while a negative factor $J_{ij} < 0$ means that the particles "pull each other",
and thus want to align.
The system's total energy is the sum of all interaction energies
considering the particles' alignment,
$$H(s) \coloneqq \sum_{(i,j)} J_{ij} s_i s_j.$$

:::Missing
Figure with spin system and weights.
Every time you click in a node, the spin flips.
:::

One also considers a _magnetic field_ interacting with this system.
It acts on each particle with a factor $h_i \in \R$
that biases it towards one of the states.
With this field, the total energy becomes
$$H(s) \coloneqq \sum_{(i, j)} J_{ij} s_i s_j + \sum_{i} h_i s_i.$$

:::Missing
Figure with graph and spin system + external field.
Every time you click in a node, the spin flips.
:::


We can write the Ising model as a MIP by enumerating its nodes from $1$ to $N$,
and defining an upper-triangular matrix $J$
whose components are $J_{ij}$ for $i > j$ and $(i, j) \in \mathrm{Edges}$ or zero otherwise.
This way, finding the minimum energy configuration of an Ising model
amounts to the integer quadratic optimization program
$$
\begin{equation}
\tag{Ising}
  \begin{array}{rl}
    \min\limits_{s} & s^\top J s + h^\top s \\
    \textrm{s.t.} & s \in \{-1, +1\}^N.
  \end{array}
\end{equation}
$$

As you soon shall see, we can convert any MILP to this form.

QUBO: Quadratic Unconstrained Binary Optimization
-------------------------------------------------

The Ising model has an intuitive physical interpretation
but from a more computational point of view,
the $\pm1$ variables are not so common.
Their product $s_i s_j$ represent variable alignments while in OR we tend to consider variable activation,
formulated with the product of binary (0 or 1) variables.
Thus, it is customary to use an equivalent formulation with a Boolean domain.
These are _Quadratic Unconstrained Binary Optimization_ problems,
or QUBO for short, and have the exact same computational expressivity and hardness to solve as the Ising model:
$$
\begin{equation}
\tag{QUBO}
  \begin{array}{rl}
    \min\limits_{x} & x^\top Q x \\
    \textrm{s.t.} & x \in \{0, 1\}^N.
  \end{array}
\end{equation}
$$

To go from an Ising problem to a QUBO one,
all you need is a change of variables $s = 2x - 1$,
where we also write $1$ for the vector whose components are all one.
Then, open the objective function and rearrange the terms:
$$ \begin{aligned}
  &&(2x - 1)^\top J (2x - 1) + h^\top (2x - 1) \\
  &=& 4 x^\top J x - 2x^\top J 1 - 1^\top J (2 x) - 1^\top J 1 + 2h^\top x -h^\top 1 \\
  &=& \underbrace{x^\top (4 J) x}_{\text{quadratic}}
     - \underbrace{2((J + J^\top) 1 + h)^\top x}_{\text{linear}}
     + \underbrace{(1^\top J 1 - h^\top 1)}_{\text{constant}}
  \end{aligned}
$$
The constant part does not affect the optimization process and
you can get rid of the linear terms by noticing that for binary variables,
$x_i^2 = x_i$.
This lets you absorb the linear part into the quadratic one's diagonal by defining
$$ Q =  4J  -2\mathrm{Diag}\left((J + J^\top) 1 + h\right) $$

Of course, the equivalence's other direction is equally straightforward.

Knowing this equivalence,
in the next section we will focus on transforming
combinatorial problems to a QUBO format.
The QUBO to Ising can be done as a final postprocessing.


Rwriting MILP into QUBO and Ising
=================================

Now that you know what an Ising model is and why it is useful,
it's time to learn how to convert more "classical" problems into it.
Because of their modeling expressivity and ubiquity,
we will focus on mixed-integer linear programs.
Let's just add one restriction to make our life easier:
all variables should be bounded.[^bigM]
This way, the problem takes the form below.
$$
  \begin{array}{rl}
    \min\limits_{x} & c^\top x \\
    \textrm{s.t.}   & A x = b, \\
                    & L_i \le x_i \le U_i, \\
                    & x \in \R^m \times \Z^k.
  \end{array}
$$

[^bigM]: This restriction is common in real-world problems and is somewhat similar to a big M.

Binarization
------------

Inequalities to Equalities
--------------------------

Penalization
------------




References:

https://personal.lse.ac.uk/anthony/Quadratization_web.pdf

A Brief introduction of Fourier Analysis on the Boolean Cube

https://github.com/JuliaQUBO/ToQUBO.jl
