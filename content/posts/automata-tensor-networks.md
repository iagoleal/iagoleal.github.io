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

\def\brac#1{\llbracket #1 \rrbracket}


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
    vec,
    shape border rotate = 180,
  },
}
```

You may have noticed from my previous posts that I am really into finite automata (FA).
They exist in an intersection between languages and controllable systems that is simply awesome.
So, as you may have expected from the title, this is yet another post about them.
Our plan today is to take a look at quantum systems and circuits
that measure whether a finite automaton accepts a fixed-size input string.

Quantum states are notoriously hard to represent in a classical computer.
For example, a general quantum computing system with $N$ qubits requires $2^N$ complex coefficients,
which is prohibitively large for even moderately sized $N$.
Thankfully, many of systems that one may encounter in the real world
are describable with much less information.
As we will see, for $N$ characters and an automaton with $A$ symbols and $S$ states,
the relevant quantum systems reduce from the exponential $A^N$,
to a much more amenable $O(N A S^2)$ coefficients to represent---making them great candidates for simulations in classical machines.

In the course of this post,
we also explore different forms of representing FAs.
Namely, as vectors, tensor networks and quantum circuits.
Also, besides the obvious prerequisites on tensors and automata,
a bit of familiarity with [Bra-ket notation](https://en.wikipedia.org/wiki/Bra%E2%80%93ket_notation)
and String Diagrams will be useful for understanding this post,
nevertheless, I'll try to introduce any new concept as needed.
Please drop a message if you find anything to be missing or confusing!


Vector Spaces for Automata
==========================

Start by fixing a (nondeterministic) finite automaton with

- Finite state set $\States$;
- Finite alphabet set $\Actions$;
- Initial state $s_0 \in \States$;
- Accepting states $\Accepting \in \Pow\States$;
- Transition function $t \colon \Actions \times \States \to \Pow \States$.

Also, for practicality,
let's name a function $\match \colon \Actions^\star \to \{0, 1\}$
that checks whether the automaton recognizes a string.
We write brackets $\brac{\text{--}} \colon \mathtt{Bool} \to \C$
whenever using truth values as the complex numbers $0$ and $1$.

Since, we've already discussed the dynamics of FA in a [previous post](/posts/automata-monads/),
I won't spend much time on it.
What's important is that these machines follow their transition $t$
to check if a string takes it from $s_0$ to a state in $\Accepting$.
How can we represent this using only linear algebra?

To translate this system on finite sets to vectors and matrices,
let's make use of the _free vector space_.
For a finite set $X$, we write its free vector space as $\C^X$,
and you can think of it as complex-weighted _superpositions_ of elements of $X$.
To be practical, in this post $\C^X$ will just mean the complex-valued functions $X \to \C$.[^free-vect]

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
$$ \braket{\Accepting | s} = \brac{ x \in \Accepting }. $$

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
$$ T^{\alpha} \coloneqq \sum_{s, s' \in \States} \brac{s' \in t(\alpha, s)}\ket{s'}\bra{s},\, \text{ for } \alpha \in \Actions.$$

For a state $s$,
this represents the transition from $(s, \alpha)$ as $\ket{s'} = T^\alpha \ket{s}$.
Even when $s$ is nondeterministic (a subset instead of an element),
linearity guarantees that the transition matrices act as they should.
Thus, for a finite string[^kleene-star] $\sigma = \alpha \cdots \omega \in \Actions^\star$,
we construct a matrix $T^\sigma \coloneqq T^\omega \cdots T^\alpha$ which takes a state
and takes it through the string's dynamics.
To check whether the automaton accepts $\sigma$,
we start at $\ket{s_0}$, apply each $T^\alpha$ and check the projection onto $\bra{\Accepting}$:
$$\mathtt{match}(\alpha \cdots \omega) = \brac{ \braket{\Accepting | T^\omega \cdots T^\alpha | s_0} \neq 0 }.$$

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
$\bra{\mathtt{match}_N} \colon \bigotimes_{i=1}^N \C^\Actions \to \C$
which is non-zero only on the span of matched strings.

In theory, it is simple to do.
Just define $\bra{\mathtt{match}_N}$ as the matched set's indicator:
$$ \bra{\mathtt{match}_N} = \sum_{\sigma \in \Actions^N} \match(\sigma) \bra{\sigma}.$$
Although this is enough for an existence proof,
actually constructing $\bra{\mathtt{match}_N}$ requires the problem to be already solved.
Furthermore, this is computationally expensive, requiring $A^N$ complex coefficients.
Using _Tensor Networks_, this amounts to saying that we only know a black box for this tensor.

```tikz
\draw[fill=sgreen, rounded corners, thick] (0, 0) rectangle (4, 1);
\foreach \x in {0.4, 1.2, 2.8, 3.6} {
  \draw[thick] (\x, 1) -- (\x, 1.5);
}
\node at (2, 1.25) {$\cdots$};
\node at (2, 0.5) {$\mathtt{match}_N$};
```

We can do much better than this though.
To be fair,
this is among this post's main points and deserves to be enshrined as a theorem.
The remainder of this section will be dedicated to finding such compact formulation.

:::Theorem
Given a (nondeterministic) finite automaton with $A$ symbols and $S$ states,
there is a tensor network requiring only $O(NAS^2)$ coefficients to represent
that determines whether the FA accepts any string with length $N$.
:::

You can think of a _tensor network_ as a graph of lazily evaluated tensors.[^vect]
They are in one-to-one correspondence with Einstein summation notation
because of instead of writing a tensor as its components,
they represent it as the result of contracting other tensors.
Previously, we represented the automaton
as an inner product $\braket{\Accepting | T^\omega \cdots T^\alpha | s_0}$.
By looking at all elements as tensors, we get a linear tensor network.

[^vect]: For my categorist readers: They are exactly the string diagrams
in the category of $\mathtt{FinVect}(\C)$ of finite dimensional complex vector spaces.

```tikz
{ [ start chain
  , every on chain/.style=join
  , every join/.style=thick
  , node distance=5mm
  ]

  \node [covec, on chain] {$\Accepting$};
  \foreach \i in {5,...,1}
    \node [tensor, on chain] {$T^\alpha$};
  \node [vec, on chain] {$s_0$};
}
```

This is a "closed system" inasmuch as there are no free wires to connect an external input.
The string $\sigma$ is fixed and this network represents a single complex number.
How can we make this system linear on $\sigma$?

The answer is to look again at the transition function's type
but smartly uncurry it as a 3-tensor,

$$ \begin{array}{rcl}
  && \Actions \times \States  \to \Pow \States \\
  &\cong&
  \Actions \times \States \times \States \to \{0, 1\} \\
  &\subseteq&
  \Actions \times \States \times \States \to \C \\
  &\cong&
  \C^\Actions \otimes \C^\States \otimes \C^\States
\end{array}
$$

Let's call this tensor $T$.
Notice that contracting it with pure characters
gives rise to our previous matrices: $T^\alpha = T \ket{\alpha}$,
and we get a linear dependence on the input string.
Substitute this into our previous tensor network.

```tikz
{ [ start chain
  , every on chain/.style=join
  , every join/.style=thick
  , node distance=5mm
  ]

  \node [covec, on chain] {$\Accepting$};
  \foreach \i in {5,...,1} {
    \node [tensor, on chain] {$T$};
    { [start branch = U going above]
      \node [vec, on chain, shape border rotate=90] {$\alpha_\i$};
    }
  }
  \node [vec, on chain] {$s_0$};
}
```

By isolating the part of the network that does not depend on
the tensor product of characters $\bigotimes \ket{\alpha_i}$,
we arrive at an expression for the matching tensor.

```tikz
\draw[fill=sgreen, rounded corners, thick] (0, 0) rectangle (4, 1);
\foreach \x in {0.4, 1.2, 2.8, 3.6} {
  \draw[thick] (\x, 1) -- (\x, 1.5);
}
\node at (2, 1.25) {$\cdots$};
\node at (2, 0.5) {$\mathtt{matcher}_N$};

\node (eq) at (5, 0.5) {$=$};

{ [ start chain
  , every on chain/.style=join
  , every join/.style=thick
  , node distance=5mm
  ]

  \node [covec, on chain, right = of eq] {$\Accepting$};
  \foreach \i in {5,...,1} {
    \ifnum \i=3
    \node[on chain] {$\cdots$};
    \else
      \node [tensor, on chain] {$T$};
      { [start branch = U going above]
        \node [on chain] {};
      }
    \fi
  }
  \node [vec, on chain] {$s_0$};
}
```

This kind of tensor network is known as a _Matrix Product State_ (MPS)
among physicists and as a _Tensor Train_ among numerical analysts.
Although I'm quite a fan of trains, let's go with MPS for this post.
They are very compact representation for an $N$-tensor.
In our application, there are $N$ copies of
$T$---each requiring $A S^2$ coefficients---plus the $S$ coefficients
for each of $\bra{\Accepting}$ and $\bra{s_0}$.
Therefore, we got down from the exponential $A^N$ to only $NAS^2 + 2S$ degrees of freedom.
By precontracting $T$ with the initial and final states, we can reduce it even further to
$NAS^2 - 2AS(S-1)$.

```tikz
{ [ start chain
  , every on chain/.style=join
  , every join/.style=thick
  , node distance=5mm
  ]

  \node [tensor, on chain, fill=sorange] {}; { [start branch = U going above] \node [on chain] {}; }
  \node [tensor, on chain] {}; { [start branch = U going above] \node [on chain] {}; }
  \node[on chain] {$\cdots$};
  \node [tensor, on chain] {}; { [start branch = U going above] \node [on chain] {}; }
  \node [tensor, on chain, fill=sorange] {}; { [start branch = U going above] \node [on chain] {}; }
}
```

Turning into a Quantum Circuit
------------------------------

Tensor networks are great for quantum simulations,
but what if we want to represent our automaton on an actual quantum computer?
We will need to represent it as a (quantum circuit)[https://en.wikipedia.org/wiki/Quantum_circuit] (QC).
For this post's purposes, a QC is a tensor network where all tensors are unitary.
In a single-sided circuit, we also allow it to have a pure state as input.
Fortunately for us, MPS have a property called _gauge freedom_
which allow them to be rewritten using only isometries.
With this, it is possible to rewrite any MPS as a quantum circuit.





References
==========

Preparing QC:
* https://quantum-journal.org/papers/q-2023-11-07-1171/pdf/
* https://arxiv.org/pdf/1803.11537

https://en.wikipedia.org/wiki/Quantum_finite_automaton

