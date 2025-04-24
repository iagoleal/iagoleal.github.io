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

\def\States{\mathcal{S}}
\def\Actions{\mathcal{A}}

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


Automata in Vector Form
=======================


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

By interpreting the diagram of a FA as a graph,
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

https://quantum-journal.org/papers/q-2023-11-07-1171/pdf/

https://en.wikipedia.org/wiki/Quantum_finite_automaton
