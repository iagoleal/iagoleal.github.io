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
  \begin{array}{rl}
    \min\limits_{x} & c^\top x \\
    \textrm{s.t.}   & A x = b, \\
                    & x \ge 0, \\
                    & x \in \R^m \times \Z^k.
  \end{array}
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

The Ising Model in a Graph
==========================

Let's start off with what we are interested.
From a physics point of view,
the Ising model consists of a bunch of particles with pairwise interactions between them.
Each particle can be in one of of two states $\{\uparrow, \downarrow\}$.


Figure with graph and spin system.
Every time you click in a node, the spin flips.

QUBO <-> ising
--------------

The Ising model has an intuitive physical interpretation
but, for us, operations research folks, the $\pm1$ variables are not so common.


QUBO: Quadratic Unconstrained Binary Optimization
=================================================

- QUBO or Ising Model

Rwriting MILP into QUBO and Ising
=================================

For operations researchers



Discrete Opt

Natural numbers

Binary Strings

Objective is polynomial

Feasibility is polynomial

Penalization



References:

https://personal.lse.ac.uk/anthony/Quadratization_web.pdf

A Brief introduction of Fourier Analysis on the Boolean Cube

https://github.com/JuliaQUBO/ToQUBO.jl
