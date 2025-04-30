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


```{=tex}

\usetikzlibrary{chains,shapes.geometric}

\colorlet{sgrey}{gray!80}
\colorlet{sorange}{orange!70}
\colorlet{sgreen}{green!40}

\tikzset{
  tensor/.style = {
    fill = sgreen, draw=black, circle, thick, minimum size=0.8cm,
  },
  state/.style = {
    tensor,
    fill = white,
    isosceles triangle,
    isosceles triangle apex angle = 75,
    minimum size = 0,
    minimum width = 0.5cm,
    inner sep = 0.5mm,
  },
  costate/.style = {
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

Start by fixing a (non-deterministic) finite automaton with

- Finite state set $\States$;
- Finite alphabet set $\Actions$;
- Initial state $s_0 \in \States$;
- Accepting states $\Accepting \in \Pow\States$;
- Transition function $t \colon \Actions \times \States \to \Pow \States$.

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
Since this post's theme is quantum mechanics,
let's use some of the field's notation.
We denote vectors inside funny triangles called _kets_,
such as $\ket{\psi} \in \C^X$.
Also, elements of the dual have the triangles inverted in a _bra_:
$\bra{\phi} \colon \C^X \to \C$.
Joining them into a _bra-ket_ $\braket{\phi|\psi}$ amounts to function application
and computes a complex number.
Also, yes, the pun is intended. These physicists...

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
This way, subsets become those vectors with only binary components.
In particular, the accepting states turn into
$$ \ket{\Accepting} \coloneqq \sum_{f \in \Accepting} \ket{f} \in \C^\States.$$

So we already see an interesting characteristic of the vectorial approach:
it unifies elements and subsets---or analogously, determinism and nondeterminism.
Moreover, checking if the automaton accepts an state $s \in \States$
becomes an inner product
$$ \braket{\Accepting | s} = \begin{cases}
  1,& x \in \Accepting, \\
  0,& x \not \in \Accepting.
\end{cases}
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
Even when $s$ is non-deterministic (a subset instead of an element),
linearity guarantees that the transition matrices act as they should.
Thus, for a finite string[^kleene-star] $\sigma = \alpha \cdots \omega \in \Actions^\star$,
we construct a matrix $T^\sigma \coloneqq T^\omega \cdots T^\alpha$ which takes a state
and takes it through the string's dynamics.
To check whether the automaton accepts $\sigma$,
we can start at $\ket{s_0}$, apply each $T^\alpha$ and check if the inner product with $\bra{\Accepting}$ is nonzero:
$$\mathtt{match}(\alpha \cdots \omega) = \braket{\Accepting | T^\omega \cdots T^\alpha | s_0} \neq 0.$$

As a final treat, notice that the only property of nondeterminism that we actually used
is that $\Pow \States$ is representable as complex functions.
Thus, the above discussion still works for other kind of automata with this property
such as deterministic, probabilistic or quantum.

[^kleene-star]: The notation $X^*$ denotes the [Kleene Star](https://en.wikipedia.org/wiki/Kleene_star),
not the dual vector space.
Math likes to reuse symbols, I know.



Automata in Tensor Form
=======================

Let's now look at (nondeterministic) finite automata (FA) and how to represent them as quantum systems.
More concretely, the idea is to construct a $N$-site operator that classifies
whether a string with $N$ tokens from the alphabet is accepted by the FA.

First of all, some notation.


:::Theorem
Given a (nondeterministic) finite automaton $(S, A, t)$ with transition matrix,
there is a tensor network $\bra{}$ such that for any state $\ket{\psi}$,
$\braket{M, \psi}$ equals the probability of $\psi$ satisfying the automaton.
:::

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
If it is non-zero, the we are in a final state and the machine accepts the string,

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
  \node [state, on chain] {$s_0$};
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
      \node [state, on chain, shape border rotate=90] {$\alpha_\i$};
    }
  }
  \node [state, on chain] {$s_0$};
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

