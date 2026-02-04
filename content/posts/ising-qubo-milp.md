---
title: Your Favorite Problem as an Ising Model
keywords: [math, optimization, quantum-computing]
date: 2026-01-29
thumbnail: "thumbnail.png"
theme: [math, interactive]
description:
  Quantum Computers are advancing and one should know
  how to fit classical problems into them.
suppress-bibliography: true
css: ["/css/plots.css", "index.css"]
---

\def\ceil#1{\lceil #1 \rceil}
\def\floor#1{\lfloor #1 \rfloor}
\def\B{\{0, 1\}}
\def\Is{\{-1, +1\}}
\def\norm#1{\left\lVert#1\right\rVert}
\def\abs#1{\left|#1\right|}

```{=tex}
\pgfdeclarelayer{background}
\pgfdeclarelayer{foreground}
\pgfsetlayers{background,main,foreground}

\usetikzlibrary{shadows,animations}

\tikzset{
  atom/.pic={
    % Box
    \node[rectangle, draw=gray!60, fill=white, rounded corners=6pt, minimum width=1.2cm, minimum height=1.2cm, drop shadow] (qbox) at (0,0) {};
    % Orbits (darker)
    \draw[black!60, thin] (0,0) ellipse [x radius=0.48cm, y radius=0.18cm];
    \draw[black!60, thin, rotate=60] (0,0) ellipse [x radius=0.48cm, y radius=0.18cm];
    \draw[black!60, thin, rotate=-60] (0,0) ellipse [x radius=0.48cm, y radius=0.18cm];
    % Nucleus
    \fill[black!80] (0,0) circle (0.09cm);
  },
  writeup/.style = { midway, above=1mm, align=center, font=\small },
}
```

Quantum computing is on the rise and with it the promise
of faster solvers for currently hard problems.
These machines come in many colours and flavours --- each with its own (dis)advantages ---
but for the folks in operations research a particularly exciting kind is
the _Quantum Annealer_.

Forget about your Hadamard gates, circuits, and quantum Turing completeness.
For our purposes, the quantum annealer is a black box
capable of hastily solving the Ising model, a very specific kind of optimization problem.
Somewhat like the diagram below.


```tikz {tikzlibrary="quotes,decorations.markings,shapes.geometric"}
{ [every node/.style = {align = center},
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
    \draw[postaction={decorate}]  (q.east) -- (s.west) node [writeup] {Solution};
  }
}
```

Today, I don't want to discuss how these machines work[^later-post]
but rather how to put them to good use.
After all, if the problems you care about are all classical,
what is the difference this quantum machine can make in your life?
Since we are doing optimization,
let's focus on MILP --- an NP-hard problem found everywhere ---
and see how a quantum computer helps us solve it.

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
_This is today's theme_.

  ```tikz {tikzlibrary="quotes,decorations.markings,shapes.geometric"}
{ [every node/.style = {align = center},
  ]
  \node[minimum width=1.2cm] (q) at (0, 0) {};
  \pic at (q) {atom};

  \node[
    rectangle,
    draw=gray!60,
    fill=white,
    rounded corners=6pt,
    minimum width=1.6cm,
    minimum height=1.2cm,
    thick,
    drop shadow,
    font=\sffamily,
  ] (r) at (-3, 0) {\textcolor{black!80}{rewrite}};

  \node[align=center] (i) at (-6, 0) {};
  \node[align=center] (s) at (+3, 0) {};

  { [thick, black!80, decoration={
      markings,
      mark=at position 0.5 with {\arrow{Stealth}}}
      ]
    \draw[postaction={decorate}]  (i.east) -- (r.west) node [writeup] {MILP};
    \draw[postaction={decorate}]  (r.east) -- (q.west) node [writeup] {Ising\\Model};
    \draw[postaction={decorate}]  (q.east) -- (s.west) node [writeup] {Solution};
  }
}
```

Even if you haven't jumped on the quantum bandwagon,
these rewriting techniques can still be a useful addition to your toolbelt.
There are other (classical) hardware tailor-made to solving the Ising model,
such as [Fujitsu's Digital Annealers](https://www.fujitsu.com/us/services/business-services/digital-annealer/).
Or even algorithms and solvers for traditional machines such as [Simulated Annealing](https://en.wikipedia.org/wiki/Simulated_annealing)
or my own [TenSolver.jl](https://github.com/SECQUOIA/TenSolver.jl).

The Ising Model in a Graph
==========================

The _Ising Model_ consists of a graph where each node $i$ is a particle
that can be in one of two states $s_i \in \{-1, +1\}$ called their _spin_.
The graph's edges represent an interacting pair of particles
and we say that they are _aligned_ whenever their spin is equal.
To make equations simpler,
we use the product of states $s_i s_j$ as a concise way to represent alignment.
Notice that for $\pm 1$ variables it satisfies
$$
s_i s_j = \begin{cases}
  +1,& s_i = s_j \\
  -1,& s_i \ne s_j.
\end{cases}
$$

In the figure below, we represent an Ising model with 6 particles.
The graph is interactive and you can click on the nodes to flip their spin.
Nodes rotating counterclockwise represent spin $+1$ particles,
and rotating clockwise represent spin $-1$ particles.

<figure id="figure-spin" class="diagram-container">
  <svg class="diagram" viewBox="0 0 1000 500" width="100%" height="100%">
  </svg>
</figure>


The interaction energies $J_{ij} \in \R$
indicate how costly it is for the particles to remain aligned.
A positive factor $J_{ij} > 0$
means that the particles require energy to stay aligned,
while a negative factor $J_{ij} < 0$ means that the particles "pull each other" for alignment,
as summarized below.

| Interaction   | Interpretation               |
|:--------------|:-----------------------------|
| $J_{ij} < 0$  | Profit from alignment        |
| $J_{ij} = 0$  | No interaction               |
| $J_{ij} > 0$  | Require energy for alignment |

The system's total energy --- called its _Hamiltonian_ --- is the sum of all interaction energies weighted by the particles' alignment,
$$H(s) \coloneqq \sum_{(i,j)} J_{ij} s_i s_j.$$

<figure id="figure-ising" class="diagram-container">
  <svg class="diagram" viewBox="0 0 1000 500" width="100%" height="100%">
  </svg>
</figure>

One also considers external influences $h_i \in \R$
biasing each particle towards one of the states.
Keeping up with the physics theme, we call it a _magnetic field_.
Again, the field--particle interaction depends on its sign,

| Field       | Interpretation     |
|:------------|:-------------------|
| $h_{i} < 0$ | Pulls towards $+1$ |
| $h_{i} = 0$ | Indifferent        |
| $h_{i} > 0$ | Pulls towards $-1$ |


With this field, the total energy becomes
$$H(s) \coloneqq \sum_{(i, j)} J_{ij} s_i s_j + \sum_{i} h_i s_i.$$

<figure id="figure-ising-complete" class="diagram-container">
  <svg class="diagram" viewBox="0 0 1000 500" width="100%" height="100%">
  </svg>
</figure>


We can write the Ising model as a MIP by enumerating its nodes from $1$ to $N$,
and defining an upper-triangular $J$ whose components are
$J_{ij}$ for $(i, j) \in \mathrm{Edges}$ and zero otherwise.
This way, finding the minimum energy configuration of an Ising model
amounts to the integer quadratic optimization program (IQP)
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

The Ising model has an intuitive physical interpretation,
but from a more computational point of view,
the $\pm1$ variables are not so common.
The products $s_i s_j$ represent variable alignments
while in OR it's more natural to consider variable activation,
formulated with the product of binary (0 or 1) variables.
Thus, it is customary to use an equivalent formulation with a Boolean domain.
These are _Quadratic Unconstrained Binary Optimization_ problems,
or QUBO for short, and have the exact same computational expressivity and hardness to solve as the Ising model:
$$
\begin{equation}
\tag{QUBO}
  \begin{array}{rl}
    \min\limits_{z} & z^\top Q z \\
    \textrm{s.t.} & z \in \{0, 1\}^N.
  \end{array}
\end{equation}
$$

To go from an Ising problem to a QUBO one,
all you need is a change of variables $s = 2z - 1$,
where we also write $1$ for the vector whose components are all one.
Then, open the objective function and rearrange the terms:
$$ \begin{aligned}
  &&(2z - 1)^\top J (2z - 1) + h^\top (2z - 1) \\
  &=& 4 z^\top J z - 2z^\top J 1 - 1^\top J (2 z) - 1^\top J 1 + 2h^\top z -h^\top 1 \\
  &=& \underbrace{z^\top (4 J) z}_{\text{quadratic}}
     - \underbrace{2((J + J^\top) 1 + h)^\top z}_{\text{linear}}
     + \underbrace{(1^\top J 1 - h^\top 1)}_{\text{constant}}
  \end{aligned}
$$
The constant part does not affect the optimization process and
you can get rid of the linear terms by noticing that for binary variables,
$x_i^2 = x_i$.
This lets you absorb the linear part into the diagonal of the matrix by defining
$$ Q =  4J  -2\mathrm{Diag}\left((J + J^\top) 1 + h\right) $$
Of course, the equivalence's other direction is equally straightforward.

<figure id="figure-qubo" class="diagram-container">
  <svg class="diagram" viewBox="0 0 1000 500" width="100%" height="100%">
  </svg>
</figure>

Knowing this equivalence,
in the next section we focus on transforming
combinatorial problems to QUBO format.
The QUBO to Ising can be done as a final postprocessing.


Rewriting MILP into QUBO and Ising
=================================

Now that you know what an Ising model is and why it is useful,
it's time to learn how to convert "classical" problems into it.
Because of their modeling expressivity and ubiquity,
we focus on mixed-integer linear programs.
Let's just add one restriction to make our life easier:
all variables should be bounded.
This way, the problem takes the form below.
$$
  \begin{array}{rl}
    \min\limits_{x} & c^\top x \\
    \textrm{s.t.}   & A x = b, \\
                    & L_i \le x_i \le U_i, \\
                    & x \in \R^m \times \Z^k.
  \end{array}
$$

This amounts to constraining the feasibility region to a large parallelepiped.
This restriction is common in real-world problems since every resource or action has its limits.
For most problems,
you can estimate appropriate bounds that every variable should satisfy,
so it is not a big deal of an assumption.

Binarize All the Variables
--------------------------

The first step in our conversion is to make all variables binary.
There are many other available encodings with their own pros and cons.
For reasons of scope, we focus on the (in my opinion) most straightforward and useful ones:
one-hot and binary expansion.
But there are great surveys in the literature about all methods.[@qubojl] [@tamura_performance_2021]

### One-Hot Encoding

This is an old friend for anyone who's ever done some machine learning.
The idea is to create disjoint states representing
each possible value together with a restriction that only one of them can be "on" at a time.

Consider a variable $x$ taking values
in a set of $K$ possibilities,
yielding an abstract constraint
$$x \in \{Y^{1},\ldots, Y^{K}\}.$$
To binarize it,
we associate a new variable $z_j \in \B$ to each $Y^j$,
with the constraints that exactly one $z_j$ equals $1$
and that $x$ equals to corresponding $Y^j$ value,
$$
\begin{aligned}
  \textstyle\sum_{j = 1}^{K} z_j &= 1, \\
  \textstyle\sum_{j = 1}^{K} Y^{j} z_j &= x, \\
z_j &\in \B.
\end{aligned}
$$

These constraints are an integer programming way of restating
what we said in the previous paragraph.
The variable $x$ can take any of the $Y^{j}$ values
but is constrained to only choose one of them.

This is a general framework we can apply to any kind of variable.
Suppose $x \in \Z$ bounded by $L \le x \le U$.
There are exactly ${K \coloneqq \floor{U} - \ceil{L}} + 1$
values it can take.
All you must do is to apply the procedure to
$V = \{\ceil{L}, \ceil{L} + 1,\ldots, \floor{U}\}$.

The graph below is a visualization of one-hot encoding
for an integer variable $-4 \le x \le 5$.
Click the buttons to see how the value change.

<figure id="figure-one-hot" class="diagram-container">
</figure>

For a bounded real decision variable $x$,
we have to make a compromise regarding precision.
Discretize the interval $[L, U]$ into $K$ points of choice,
$$ L \le Y^{1} < Y^{j} < Y^{K} \le U.$$
A common choice is uniformly with $Y^{j} =  L + j\frac{U - L}{K}.$
Now we can proceed as before by implementing the "choice" constraints
for the $Y^{j}$ values.

### Binary Expansion Encoding

For variables taking multiple values, the one-hot encoding can produce too many variables.
An alternative is to use a binary expansion,
since it only requires a logarithm amount of variables.
Again, let's start with an integer decision variable $x \in \Z$
bounded as $L \le x \le U$
and call $K \coloneqq \floor{\log_2 (U - L)}$.
To represent all integers from $L$ to $U$ in binary,
we need a boolean vector $z$ with $K + 1$ components.
The straightforward representation amounts to $x = L + \sum_{j = 0}^{K} 2^j z_j$.

Notice, however, that if the most significant bit is on,
the variable may go above its upper bound $U$.
To prevent that, we shift the last coefficient by ($2^{K-1} - 1$),
capping the values.
This corrects the encoding to
$$ x = L + \sum_{j = 0}^{K-2} 2^j z_j + (U - 2^{K-1} + 1) z_{K-1}.$$

The graph below is a visualization of binary encoding
for an integer variable $-4 \le x \le 5$.
You can play with turning the bits on and off to see how the value changes.


<figure id="figure-binary" class="diagram-container">
</figure>

For real numbers, we can add negative power of 2 to represent the number's fractional part.
Suppose you want to allocate $R$ bits for this and call $\Delta = \sum_{j=1}^R 2^{-j}$.
The representation taking account the upper bound correction is
$$ x = L + \sum_{j = -R}^{K-2} 2^j z_j + (U - 2^{K-1} + 1 - \Delta) z_{K-1}.$$
Similarly to one-hot encoding,
you can eliminate the $x$ variable by substituting this constraint everywhere in the program.

Equality Penalization
---------------------

At this point,
we are left with a pure binary LP with only equality constraints.
$$
  \begin{array}{rl}
    \min\limits_{x} & c^\top z \\
    \textrm{s.t.}   & A z = b, \\
                    & z \in \{0, 1\}^N.
  \end{array}
$$

This problem becomes a QUBO by turning the constraints into a quadratic regularization term.
More formally,
we choose a _penalty factor_ $\rho > 0$ and move the constraint into the objective as
$$
  \begin{array}{rl}
    \min\limits_{x} & c^\top z  + \rho (Az - b)^\top (A z - b)\\
    \textrm{s.t.}   & z \in \{0, 1\}^N.
  \end{array}
$$

This is already a QUBO!
But we can find an explicit formula for the $Q$ matrix with some high school algebra.
$$ \begin{array}{rcl}
  & &c^\top z  + \rho (Az - b)^\top (A z - b)\\
  &=& c^\top z + \rho \Big[ z^\top (A^\top A) z - 2b^\top A z + b^\top b \Big] \\
  &=& z^\top (\rho A^\top A) z + (c - 2\rho A^\top b )^\top z + \rho b^\top b
  \end{array}
$$

The constant $b^\top b$ is irrelevant for the minimization
and you can safely ignore it.
And, as before, being binary makes the linear term equivalent to a quadratic diagonal.
Taking all this into account,
the final Q matrix is
$$ Q = \rho A^\top A + \mathrm{Diag}(c - 2\rho A^\top b ).$$

The only thing missing is how to properly choose the parameter $\rho$.
We want to preserve the original solutions,
so it should be large enough to force the minimum to "cancel it"
by being at an originally feasible point.
That is, whenever $Az = b$ the penalization term disappears and we are left with the original objective.

More rigorously,
let $M$ be the maximum attainable objective value,
$M = \max_{z \in \{0,1\}^N} |c^\top z|$
and $v$ be the constraint's _minimal squared violation_,
$$ v =
  \begin{array}{rl}
    \min\limits_{z} & \norm{Az - b}^2_2 \\
                    & A z \ne b \\
    \textrm{s.t.}   & z \in \{0, 1\}^N,
  \end{array}
$$
Choosing any penalty $\rho > \frac{M}{v}$
guarantees that the QUBO objective function coincides with the MILP.

Furthermore,
we can always estimate $M \le \sum_i \abs{c_i}$.
Also, if $A$ and $b$ have only integer components,
the minimal violation must be at least one, i.e., $v \ge 1$.
Thus, for integer programs a safe choice of penalty is
$\rho  = \textstyle\sum_i \abs{c_i} + 1$.

The discussion above works for a general equality constraint,
but for more specific constraint there are simpler penalizations.
I recommend the paper by @{glover_quantum_2019} for more details.

Inequalities to Equalities
--------------------------

If your MILP also has inequality constraints,
you can use _slack variables_ to put it into QUBO form.
Consider a constraint of the form $M x \le w$.
This is the same as an equality constraint together with an additional variable
representing the "inequality gap",
$$
\begin{aligned}
  M x + s &= w, \\
  s &\ge 0.
\end{aligned}
$$
Moreover, since we're assuming $x$ to be bounded,
we can calculate upper bounds for the components $s_i$
by checking the maximum possible value for ${w_i - \sum_{j}M_{ij}x_j}$.
Binarize $s$ and penalize the equality constraint to get a QUBO.


Conclusion
==========

Although the Ising Model is in general NP-hard,
there are some recent solver and hardware advancements
that treat it very well.
In particular, it is a formulation much more amenable to parallelism
and non-classic computing paradigms than your everyday MILP.
So, when you find a hard problem that doesn't scale with your hardware,
it is worth it to try these conversions.

As a parting remark,
keep in mind that these rewriting steps are all automatable.
There are ready-to-use software
that take a MILP and compiles it into a QUBO/Ising model
while transparently calculating all discretizations and penalty factors for you.
If you are using Julia,
I recommend checking [ToQUBO.jl](https://github.com/JuliaQUBO/ToQUBO.jl) for that.
And if you are not using it,
I recommend converting your models to Julia just so you can put this lib into good use.
It is this great.
In fact,
the accompanying paper [@qubojl] is a great place to further understand the techniques we discussed here.

```{=bibtex}
@misc{qubojl,
  title      = {{QUBO}.jl: {A} {Julia} {Ecosystem} for {Quadratic} {Unconstrained} {Binary} {Optimization}},
  shorttitle = {{QUBO}.jl},
  url        = {http://arxiv.org/abs/2307.02577},
  language   = {en},
  urldate    = {2023-07-07},
  publisher  = {arXiv},
  author     = {Xavier, Pedro Maciel and Ripper, Pedro and Andrade, Tiago and Garcia, Joaquim Dias and Maculan, Nelson and Neira, David E. Bernal},
  month      = jul,
  year       = {2023},
  note       = {arXiv:2307.02577 [quant-ph]},
  keywords   = {Quantum Physics, Mathematics - Optimization and Control},
}


@article{glover_quantum_2019,
  title      = {Quantum {Bridge} {Analytics} {I}: a tutorial on formulating and using {QUBO} models},
  volume     = {17},
  issn       = {1619-4500, 1614-2411},
  shorttitle = {Quantum {Bridge} {Analytics} {I}},
  url        = {http://link.springer.com/10.1007/s10288-019-00424-y},
  doi        = {10.1007/s10288-019-00424-y},
  language   = {en},
  number     = {4},
  urldate    = {2025-11-01},
  journal    = {4OR},
  author     = {Glover, Fred and Kochenberger, Gary and Du, Yu},
  month      = dec,
  year       = {2019},
  pages      = {335--371},
}


@article{tamura_performance_2021,
  title    = {Performance {Comparison} of {Typical} {Binary}-{Integer} {Encodings} in an {Ising} {Machine}},
  volume   = {9},
  issn     = {2169-3536},
  url      = {https://ieeexplore.ieee.org/document/9435359/},
  doi      = {10.1109/ACCESS.2021.3081685},
  urldate  = {2025-11-01},
  journal  = {IEEE Access},
  author   = {Tamura, Kensuke and Shirai, Tatsuhiko and Katsura, Hosho and Tanaka, Shu and Togawa, Nozomu},
  year     = {2021},
  keywords = {binary-integer encoding, combinatorial optimization problem, Computational modeling, Encoding, Ising machine, Ising model, Linear programming, Optimization, Physics, quadratic knapsack problem, quadratic unconstrained binary optimization, Stationary state},
  pages    = {81032--81039},
}
```

<script type="module">
  import * as figures from "./figures.js";

  const states = [1, 0, 0, 0, 1, 1].map(Boolean);

  const J = [
    [[0, 1], 2],
    [[1, 2], -1],
    [[1, 3], -3],
    [[2, 3], 6],
    [[3, 4], -2],
    [[4, 5], 1],
    [[2, 5], 2],
  ];

  const Jh = Array.from(J);
  Jh.h = [-5, 0, 2, 1, -2, 4];

  const Q = figures.isingToQubo(J, Jh.h);

  new figures.Diagram("#figure-spin", states, J)
    .graph()
    .popup();

  new figures.Diagram("#figure-ising", states, J)
    .graph()
    .weights()
    .energyLabel("H(s)")
    .popup();

  new figures.Diagram("#figure-ising-complete", states, Jh)
    .graph()
    .externalField()
    .weights()
    .energyLabel("H(s)")
    .popup();

  new figures.Diagram("#figure-qubo", states, Q, "qubo")
    .graph()
    .weights()
    .energyLabel("x^T Q x")
    .popup();

  new figures.EncodingElement("#figure-one-hot", -4, 5, 2, "onehot")
    .buttons()
    .labelNvar()
    .label();

  new figures.EncodingElement("#figure-binary", -4, 5, 2, "binary")
    .buttons()
    .labelNvar()
    .label();
</script>
