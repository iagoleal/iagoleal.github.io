---
title: Finite Automata as Quantum Tensor Networks
keywords: [automata, quantum computing]
date: 2025-04-22
description:
  Finite automata accept a description using linear algebra
  that we can translate into a system of tensors or quantum circuits.
  These provide examples of quantum systems only requiring
  a polynomial amount of information to represent and simulate classicaly.
---

\def\Pow{\mathcal{P}}

\def\States{\mathcal{S}}
\def\Actions{\mathcal{A}}
\def\nStates{A}
\def\nActions{S}
\def\Accepting{\mathcal{F}}

\def\C{\mathbb{C}}
\def\N{\mathbb{N}}

\def\match{\mathtt{match}}


```{=tex}

\usetikzlibrary{chains,shapes.geometric}

\colorlet{sgrey}{gray!80}
\colorlet{sorange}{orange!70}
\colorlet{sgreen}{green!40}

\tikzset{
  tensor/.style = {
    fill = sgreen, draw=black, circle, thick, minimum size=0.8cm,
  },
  vec/.style = {
    tensor,
    fill = white,
    isosceles triangle,
    isosceles triangle apex angle = 75,
    minimum size = 0,
    minimum width = 0.5cm,
    inner sep = 0.5mm,
  },
  covec/.style = {
    state,
    shape border rotate = 180,
  },
}
```

Quantum states are notoriously hard to represent in a classical computer.
For example, a general quantum computing system with $N$ qubits requires $2^N$ complex coefficients,
which is already prohibitively large for moderately sized $N$.
Thankfully, a lot of systems that one may encounter in the real world
are describable with much less information.

You may have noticed from my previous posts that I am really into finite automata (FA).
They exist in an intersection between languages and controllable systems that is simply awesome.
So, as you may have expected from the title, this is yet another post about them.
Our plan today is to take a look at quantum systems and circuits
that measure whether a finite automaton accepts a fixed-size input string.
As we will see, for $N$ characters and an automaton with $A$ symbols and $S$ states,
the relevant quantum systems reduce from the exponential $A^N$,
to a much more amenable $O(N \cdot A \cdot S^2)$ coefficients to represent!
Therefore, they are great candidates for simulations in classical computers.

In the course of this post,
we also explore different forms of representing FAs.
Namely, as vectors, tensor networks and quantum circuits.
Also, besides the obvious prerequisites on tensors and automata,
a bit of familiarity with [Bra-ket notation](https://en.wikipedia.org/wiki/Bra%E2%80%93ket_notation)
and String Diagrams will be useful for understanding this post,
nevertheless, I'll try to introduce any new concept as needed.
Please drop a message if you find anything to be missing or confusing!


Automata in Vector Form
=======================

Start by fixing a (nondeterministic) finite automaton with

- Finite state set $\States$;
- Finite alphabet set $\Actions$;
- Initial state $s_0 \in \States$;
- Accepting states $\Accepting \in \Pow\States$;
- Transition function $t \colon \Actions \times \States \to \Pow \States$.

Also, for practicality,
let's name a function $\match \colon \Actions^\star \to \{0, 1\}$
that checks whether the automaton recognizes a string.

We've already discussed the dynamics of FA in a [previous post](/posts/automata-monads/),
so I won't spend much time on it.
What's important is that these machines follow their transition $t$
to check if a string takes it from $s_0$ to a state in $\Accepting$.
How can we represent this using only linear algebra?

To translate this system on finite sets to vectors and matrices,
let's make use of the _free vector space_.
For a finite set $X$, we write its free vector space as $\C^X$,
which is isomorphic to the functions $X \to \C$,
so let's just treat it as these functions.[^free-vect]
You can think of those as complex-weighted _superpositions_ of elements of $X$.

Since this post's theme is quantum mechanics,
let's use some of the field's notation.
We denote vectors inside funny triangles called _kets_,
such as $\ket{\psi} \in \C^X$.
Also, elements of the dual have the triangles inverted in a _bra_:
$\bra{\phi} \colon \C^X \to \C$.
Joining them into a _bra-ket_ $\braket{\phi|\psi}$ amounts to function application
and computes a complex number.
Also, yes, the pun is intended. Those physicists...

[^free-vect]: You can also use formal linear combinations of elements of $X$,
and everything works the same. For finite sets, they are isomorphic.

For each element $x \in X$,
there is an indicator function $\ket{x}$
and, as expected, these form an orthonormal basis of $\C^X$.[^braket-notation]
That is,
there are $c_x \in \C$ such that for any vector
$$ \ket{\psi} = \sum_{x \in X} c_x \ket{x}.$$

[^braket-notation]: In the usual mathematical notation,
we generally write the vector $\ket{x}$ as $e_x$ or even simply $x$.

For the automaton, the states and alphabet turn into vector spaces $\C^\States$ and $\C^\Actions$,
while the initial state is the vector $\ket{s_0} \in C^\States$,
but we are still missing vectorial versions of the accepting states and transition function.
For that, we employ the useful trick of looking at subsets as binary functions:
$$ \Pow{X} \cong \{0, 1\}^X \subseteq \C^X.$$
This way, subsets become superpositions where each element is either present or not,
i.e., vectors with only binary components.
In particular, the accepting set turns into
$$ \ket{\Accepting} \coloneqq \sum_{f \in \Accepting} \ket{f} \in \C^\States.$$

So we already see an interesting characteristic of the vectorial approach:
it unifies elements and subsets---or analogously, determinism and nondeterminism.
Moreover, checking if the automaton accepts a state $s \in \States$
becomes an inner product.
All accepted states have a nonzero projection into $\ket{\Accepting}$:
$$ \braket{\Accepting | s} = \bigg[ x \in \Accepting\bigg].
$$

How cool is that?
Keep this at the back of your head while we turn our attention to the transition.
The previous isomorphism and some currying on its type yields
$$ \begin{array}{rcl}
  && \Actions \times \States  \to \Pow \States \\
  &\cong&
  \Actions \times \States \to (\States \to \{0, 1\})\\
  &\cong&
  \Actions \to (\States \to (\States \to \{0, 1\})) \\
  &\cong&
  \Actions \to (\States \times \States \to \{0, 1\}) \\
  &\subseteq&
  \Actions \to (\States \times \States \to \C) \\
  &\cong&
  \Actions \to \C^{\States \times \States} \\
  &\cong&
  \Actions \to (\C^{\States} \overset{\text{Linear}}{\to} \C^\States)
\end{array}
$$

Although this seems like a lot of steps, they are mostly bookkeeping.
The important part is that the transition $t$ becomes a family of matrices
indexed by $\Actions$.
This way, we define the linear operators
$$ T^{\alpha} \coloneqq \sum_{s, s' \in \States} t(s, \alpha)(s')\ket{s'}\bra{s},\, \text{ for } \alpha \in \Actions.$$

For a state $s$,
this represents the transition from $(s, \alpha)$ as $\ket{s'} = T^\alpha \ket{s}$.
Even when $s$ is nondeterministic (a subset instead of an element),
linearity guarantees that the transition matrices act as they should.
Thus, for a finite string[^kleene-star] $\sigma = \alpha \cdots \omega \in \Actions^\star$,
we construct a matrix $T^\sigma \coloneqq T^\omega \cdots T^\alpha$ which takes a state
and takes it through the string's dynamics.
To check whether the automaton accepts $\sigma$,
we can start at $\ket{s_0}$, apply each $T^\alpha$ and check the projection onto $\bra{\Accepting}$:
$$\mathtt{match}(\alpha \cdots \omega) = \bigg[ \braket{\Accepting | T^\omega \cdots T^\alpha | s_0} \neq 0 \bigg].$$

As a final treat, notice that the only property of nondeterminism that we actually use
is that $\Pow \States$ is representable as complex functions.
Thus, the above discussion still works for other kind of automata with this property
such as deterministic, probabilistic or quantum.

[^kleene-star]: The notation $X^*$ denotes the [Kleene Star](https://en.wikipedia.org/wiki/Kleene_star),
not the dual vector space.
Math likes to reuse symbols, I know.


A Simple Example
----------------

Alright, this is all too abstract, so let's apply this formulation to a simple automaton
and see what happens.
Our object of study is a machine recognizing whether a binary string
represents an integer divisible by three.
The alphabet is binary $\Actions = \{0, 1\}$
while the states are the possible remainders $\States = \{0, 1, 2\}$.
It is represented in the diagram below.

```tikz {tikzlibrary="automata"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]}]
  \node[state, initial, accepting, initial text= ] (q_0)                {$0$};
  \node[state]                                     (q_1) [right=of q_0] {$1$};
  \node[state]                                     (q_2) [right=of q_1] {$2$};

  \path[->] (q_0) edge[bend left]  node [above] {1} (q_1)
            (q_1) edge[bend left]  node [below] {0} (q_2)
            (q_2) edge[bend left]  node [below] {0} (q_1)
            (q_1) edge[bend left]  node [below] {1} (q_0)
            (q_0) edge[loop above] node {0} ()
            (q_2) edge[loop above] node {1} ()
            ;
}
```

In vector form, it starts at $\ket{0}$ and accepts any string that also finishes at $\ket{0}$.
We can extract the transition matrices by looking at the diagram.
Since this automaton is deterministic, they are permutations,

$$
T^0 = \begin{bmatrix}
1 & 0 & 0 \\
0 & 0 & 1 \\
0 & 1 & 0
\end{bmatrix}
,\quad
T^1 = \begin{bmatrix}
0 & 1 & 0 \\
1 & 0 & 0 \\
0 & 0 & 1
\end{bmatrix}
$$


As an example,
the string is $110$ represents $6$ and is accepted because

$$
\begin{array}{rcl}
&&
\Braket{0 | \begin{bmatrix}
  1 & 0 & 0 \\
  0 & 0 & 1 \\
  0 & 1 & 0
  \end{bmatrix}
  \begin{bmatrix}
  0 & 1 & 0 \\
  1 & 0 & 0 \\
  0 & 0 & 1
  \end{bmatrix}
  \begin{bmatrix}
  0 & 1 & 0 \\
  1 & 0 & 0 \\
  0 & 0 & 1
  \end{bmatrix}
  | 0} \\
&=&
\Braket{0 | \begin{bmatrix}
  1 & 0 & 0 \\
  0 & 0 & 1 \\
  0 & 1 & 0
  \end{bmatrix}
  | 0} \\
&=&
\braket{0 | 0} \\
&=& 1
\end{array}
$$



Automata in Tensor Form
=======================

Our vector discussion was well and good but it still lacks something.
Since the strings in $\Actions^\star$ had to be fixed beforehand,
the dynamics are not linear on it.
We made an open system parameterized on $s_0$ and $\Accepting$
while we want those to be remain constant with varying input strings.
Let's make this notation precise.
For $N \in \N$,
the tensor product $\bigotimes_{i=1}^N \C^\Actions$
is a vector space with $N$-length strings as basis.
Does it have a subspace spanned only by recognized functions?
If so, its projection would be a linear functional
$\bra{\phi} \colon \bigotimes_{i=1}^N \C^\Actions \to \C$
which is non-zero only on the span of matched strings.

In theory, it is simple to do.
Just define $\bra{\phi}$ as the matched set's indicator:
$$ \bra{\phi} = \sum_{\sigma \in \Actions^N} \match(\sigma) \bra{\sigma}.$$
Although this is enough for an existence proof,
actually constructing $\bra{\phi}$ requires the problem to be already solved.
Furthermore, this is computationally expensive, requiring $A^N$ complex coefficients.
Using _Tensor Networks_, this amounts to saying that we only know a black box for this tensor.

```tikz
\draw[fill=sgreen, rounded corners, thick] (0, 0) rectangle (4, 1);
\foreach \x in {0.4, 1.2, 2.8, 3.6} {
  \draw[thick] (\x, 1) -- (\x, 1.5);
}
\node at (2, 1.25) {$\cdots$};
\node at (2, 0.5) {$\phi$};
```

We can do much better than this though.
This is one of the main points in this post,
so it deserves to be enshrined as a theorem.
The remainder of this section will be dedicated to finding such compaction formulation.

:::Theorem
Given a (nondeterministic) finite automaton with $A$ symbols and $S$ states,
there is a tensor network projecting length $N$ strings into the recognized ones
while requiring only $O(NAS^2)$ coefficients to represent.
:::





-----------------------------------------------------------------------------------------


Let's now look at (nondeterministic) finite automata (FA) and how to represent them as quantum systems.
More concretely, the idea is to construct a $N$-site operator that classifies
whether a string with $N$ tokens from the alphabet is accepted by the FA.

First of all, some notation.



By interpreting the diagram of an FA as a graph,
a common way to represent its transition function
is as a family of adjacency matrices $U_\alpha$ for $\alpha \in A$.
Each $U_\alpha$ represents the edges labeled with letter $\alpha$.
For us, it is more interesting to join all those matrices into a 3-tensor $U$
whose components are

$$ U_{\alpha s s'} = \begin{cases}
  1,& s' \in T(s, \alpha) \\
  0,& \text{otherwise}.
\end{cases} $$

This is a tensor with $(\#S)^2 \cdot (\#A)$ components.
Also, notice that with this construction, $U_\alpha = U \ket{\alpha}$.

For a string of $N$ characters $\{\alpha_1,\ldots,\alpha_N\}$,
the evolution of an automata can be described in vector form
as a sequence of matrix applications

$$ \ket{s_N} = U_{\alpha_N} \ldots U_{\alpha_1}\ket{s_0}$$

Now, $\ket{s_n}$ is a vector of possible states the system can be.
To check whether the machine accepts the input string,
we can take its (usual) inner product with the operator defined as
$\bra{F} = \sum_{f \in F} \bra{f}$.
If it is nonzero, the we are in a final state and the machine accepts the string,

$$ \braket{F | s_n} > 0. $$


```tikz
{ [ start chain
  , every on chain/.style=join
  , every join/.style=thick
  , node distance=5mm
  ]

  \node [on chain] {};
  \foreach \i in {5,...,1}
    \node [tensor, on chain] {$U_{\alpha_\i}$};
  \node [vec, on chain] {$s_0$};
}
```

```tikz
{ [ start chain
  , every on chain/.style=join
  , every join/.style=thick
  , node distance=5mm
  ]

  \node [on chain] {};
  \foreach \i in {5,...,1} {
    \node [tensor, on chain] {$U$};
    { [start branch = U going above]
      \node [vec, on chain, shape border rotate=90] {$\alpha_\i$};
    }
  }
  \node [vec, on chain] {$s_0$};
}
```

Turning into a Quantum Circuit
------------------------------





References
==========

Preparing QC:
* https://quantum-journal.org/papers/q-2023-11-07-1171/pdf/
* https://arxiv.org/pdf/1803.11537

https://en.wikipedia.org/wiki/Quantum_finite_automaton

