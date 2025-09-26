---
title: Finite Automata as Quantum Tensor Networks
keywords: [automata, quantum computing, tensor networks]
date: 2025-05-14
suppress-bibliography: true
thumbnail: "mps-word.png"
theme: math
description:
  Finite automata accept a description using linear algebra
  that we can translate into a system of tensors or quantum circuits.
  These provide examples of quantum systems only requiring
  a polynomial amount of information to represent and simulate classicaly.
---

\def\Pow{\mathcal{P}}

\def\S{\mathcal{S}}
\def\A{\mathcal{A}}
\def\H{\mathcal{H}}
\def\nStates{A}
\def\nActions{S}
\def\Accepting{\mathcal{F}}

\def\C{\mathbb{C}}
\def\N{\mathbb{N}}

\def\match{\mathtt{match}}

\def\brac#1{\llbracket #1 \rrbracket}
\def\norm#1{\left\lVert#1\right\rVert}


```{=tex}

\usetikzlibrary{chains,shapes.geometric}

\colorlet{sgrey}{gray!80}
\colorlet{sorange}{orange!70}
\colorlet{sgreen}{green!40}
\colorlet{sblue}{green!40}

\definecolor{lightgreen}{HTML}{90EE90}
\definecolor{palegreen}{HTML}{98FB98}
\definecolor{thistle}{HTML}{D8BFD8}

\tikzset{
  tensor/.style = {
    fill = palegreen, draw=black, circle, very thick, minimum size=0.8cm,
  },
  unitary/.style = {
    tensor, rectangle, rounded corners=1pt, fill = thistle
  },
  isometry/.style = {
    unitary,
    trapezium,
    trapezium left angle = 90,
    trapezium right angle = 75,
  },
  vec/.style = {
    tensor,
    node font = \tiny,
    fill = white,
    isosceles triangle,
    isosceles triangle apex angle = 75,
    minimum size = 0.1cm,
    minimum width = 0.5cm,
    inner sep = 0.5mm,
  },
  plug/.style = {
    tensor,
    node font = \tiny,
    fill = white,
    isosceles triangle,
    isosceles triangle apex angle = 75,
    minimum size = 0.2cm,
    inner sep = 0.1mm,
    shape border rotate = #1,
  },
  plug/.default = 0,
  covec/.style = {
    vec,
    shape border rotate = 180,
  },
  tn chain/.style = {
    start chain,
    every on chain/.style=join,
    every join/.style=very thick,
    node distance=5mm,
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
the relevant quantum systems requires only $O(N A S^2)$ coefficients to represent.
That's an exponential improvement compared to the general case.

In the course of this post,
we also explore different forms of representing FAs.
Namely, as vectors, tensor networks and quantum circuits.
Also, besides the obvious prerequisites on tensors and automata,
a bit of familiarity with [Bra-ket notation](https://en.wikipedia.org/wiki/Bra%E2%80%93ket_notation)
and String Diagrams will be useful for understanding this post.
Nevertheless, I'll try to introduce any new concept as needed.
Please drop a message if you find anything to be missing or confusing!


Vector Spaces for Automata
==========================

Start by fixing a (nondeterministic) finite automaton with

- Finite state set $\S$ with $S$ elements;
- Finite alphabet set $\A$ with $A$ elements;
- Initial state $s_0 \in \S$;
- Accepting states $\Accepting \in \Pow\S$;
- Transition function $t \colon \A \times \S \to \Pow \S$.

Also, for practicality,
let's name a function $\match \colon \A^\star \to \{0, 1\}$
that checks whether the automaton recognizes a string.
We write brackets $\brac{\text{--}} \colon \mathtt{Bool} \to \C$
for using truth values as the complex numbers $0$ and $1$.

How to represent this system on finite sets using only linear algebra?
The translation to vectors and matrices makes use of the _free vector space_,
which you can think of as complex-weighted _superpositions_ of elements of $X$.

:::Definition
For a finite set $X$, its _free vector space_, denoted as $\C^X$,
is the space of complex-valued functions $X \to \C$.[^free-vect]
:::

[^free-vect]: You can also use formal linear combinations of elements of $X$,
and everything works the same. For finite sets, they are isomorphic.

Since this post's theme is quantum mechanics,
let's use some of the field's notation.
We denote vectors inside funny triangles called _kets_,
such as $\ket{\psi} \in \C^X$.
Also, elements of the dual have the triangles inverted in a _bra_:
$\bra{\phi} \colon \C^X \to \C$.
Joining them into a _bra-ket_ $\braket{\phi|\psi}$ amounts to function application
and computes a complex number.
Also, yes, the pun is intended. Those physicists...

For each element $x \in X$,
there is an indicator function $\ket{x}$
and, as expected, these form an orthonormal basis of $\C^X$.[^braket-notation]
That is,
there are $c_x \in \C$ such that for any vector
$$ \ket{\psi} = \sum_{x \in X} c_x \ket{x}.$$

[^braket-notation]: In the usual mathematical notation,
we generally write the vector $\ket{x}$ as $e_x$ or even simply $x$.


The Brute-Force Approach
========================

For the automaton, the states and alphabet turn into vector spaces $\C^\S$ and $\C^\A$.
Let's make our problem statement precise in this language.
For $N \in \N$,
the tensor product $\bigotimes_{i=1}^N \C^\A$
is a vector space with $N$-length strings as basis.
Does it have a subspace spanned only by recognized functions?
If so, its projection would be a linear functional
$\bra{\mathtt{match}_N} \colon \bigotimes_{i=1}^N \C^\A \to \C$
which is non-zero only on the span of matched strings.

In theory, it is simple to construct such functional.
Just define $\bra{\mathtt{match}_N}$ as the matched set's indicator:
$$ \bra{\mathtt{match}_N} \coloneqq \sum_{\sigma \in \A^N} \match(\sigma) \bra{\sigma}.$$

Although this is enough for an existence proof,
actually constructing $\bra{\mathtt{match}_N}$ requires the problem to be already solved.
Furthermore, this is computationally expensive, requiring $A^N$ complex coefficients.
Using _Tensor Networks_, this amounts to saying that we only know a black box for this tensor.

```tikz
\draw[fill=lightgreen, rounded corners, very thick] (0, 0) rectangle (4, 1);
\foreach \x in {0.4, 1.2, 2.8, 3.6} {
  \draw[very thick] (\x, 1) -- (\x, 1.5);
}
\node at (2, 1.25) {$\cdots$};
\node at (2, 0.5) {$\mathtt{match}_N$};
```

We can do much better than this though.
To be fair,
this is among this post's main points and deserves to be enshrined as a theorem.

:::Theorem
Given a (nondeterministic) finite automaton with $A$ symbols and $S$ states,
there is a tensor network requiring only $O(NAS^2)$ coefficients to represent
that determines whether the FA accepts any string with length $N$.
:::

To find such a compact formulation, we need to use more of the automaton's structure.

A Simple Example: Divisiblity by 3
----------------------------------

Alright, this is all too abstract, so let's apply this formulation to a simple automaton
and see what happens.
Our object of study is a machine recognizing whether a binary string
represents an integer divisible by three.
The alphabet is binary $\A = \{0, 1\}$
while the states are the possible remainders $\S = \{s_0, s_1, s_2\}$.
It is represented in the diagram below.

```tikz {tikzlibrary="automata"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]}]
  \node[state, initial, accepting, initial text= ] (q_0)                {$s_0$};
  \node[state]                                     (q_1) [right=of q_0] {$s_1$};
  \node[state]                                     (q_2) [right=of q_1] {$s_2$};

  \path[->] (q_0) edge[bend left]  node [above] {1} (q_1)
            (q_1) edge[bend left]  node [above] {0} (q_2)
            (q_2) edge[bend left]  node [below] {0} (q_1)
            (q_1) edge[bend left]  node [below] {1} (q_0)
            (q_0) edge[loop above] node {0} ()
            (q_2) edge[loop above] node {1} ()
            ;
}
```

Let's construct $\bra{\mathtt{match}_4}$ from its truth table.
For each bitstring, we check whether it is accepted and use that as component for the tensor.
$$ \begin{array}{rcl|rcl|rcl|rcl}
0000 &\to& 0 & 0100 &\to& 0 & 1000 &\to& 0 & 1100 &\to& 1 \\
0001 &\to& 0 & 0101 &\to& 0 & 1001 &\to& 1 & 1101 &\to& 0 \\
0010 &\to& 0 & 0110 &\to& 1 & 1010 &\to& 0 & 1110 &\to& 0 \\
0011 &\to& 1 & 0111 &\to& 0 & 1011 &\to& 0 & 1111 &\to& 1
\end{array} $$

Finally, construct the matcher with the table above as coefficients.
Omitting zeros, it is
$$ \bra{\mathtt{match}_N} = \bra{0011} + \bra{0110} + \bra{1001} + \bra{1100} + \bra{1111}.$$

I suppose you can see how badly this process grows with the string length.

The Vector Dynamics Approach
============================

Since, we've already discussed the dynamics of FA in a [previous post](/posts/automata-monads/),
I won't spend much time on it.
What's important is that these machines follow their transition $t$
to check if a string takes it from $s_0$ to a state in $\Accepting$.
Let's put it into vector form.

The initial state is simple: it becomes the vector $\ket{s_0} \in C^\S$.
For vectorial versions of the accepting states and transition function,
let's employ the useful trick of looking at subsets as binary functions:
$$ \Pow{X} \cong \{0, 1\}^X \subseteq \C^X.$$
This way, subsets become superpositions where each element is either present or not,
i.e., vectors with only binary components.
In particular, the accepting set turns into
$$ \ket{\Accepting} \coloneqq \sum_{f \in \Accepting} \ket{f} \in \C^\S.$$

So we encounter an interesting characteristic of the vectorial view:
it unifies elements and subsets---or analogously, determinism and nondeterminism.
Moreover, checking if the automaton accepts a state $s \in \S$
becomes an inner product,
because accepted states have a nonzero projection into $\ket{\Accepting}$:
$$ \braket{\Accepting | s} = \brac{ x \in \Accepting }. $$

How cool is that?
Keep this at the back of your head while we turn our attention to the transition.
The previous isomorphism and some currying on its type yields
$$ \begin{array}{rcl}
  && \A \times \S  \to \Pow \S \\
  &\cong&
  \A \times \S \to (\S \to \{0, 1\})\\
  &\cong&
  \A \to (\S \to (\S \to \{0, 1\})) \\
  &\cong&
  \A \to (\S \times \S \to \{0, 1\}) \\
  &\subseteq&
  \A \to (\S \times \S \to \C) \\
  &\cong&
  \A \to \C^{\S \times \S} \\
  &\cong&
  \A \to (\C^{\S} \overset{\text{Linear}}{\to} \C^\S)
\end{array}
$$

Although this seems like a lot of steps, they are mostly bookkeeping.
The important part is that the transition $t$ becomes a family of matrices
indexed by $\A$.
This way, we define the linear operators
$$ T^{\alpha} \coloneqq \sum_{s, s' \in \S} \brac{s' \in t(\alpha, s)}\ket{s'}\bra{s},\, \text{ for } \alpha \in \A.$$

For a state $s$,
this represents the transition $s \xrightarrow{\alpha} s'$ as $\ket{s'} = T^\alpha \ket{s}$.
Even when $s$ is nondeterministic (a subset instead of an element),
linearity guarantees that the transition matrices act as they should.
Thus, for a finite string[^kleene-star] $\sigma = \alpha \cdots \omega \in \A^\star$,
we construct a matrix $T^\sigma \coloneqq T^\omega \cdots T^\alpha$
which takes a state through the string's dynamics.
To check whether the automaton accepts $\sigma$,
we start at $\ket{s_0}$, apply each $T^\alpha$ and check the projection onto $\bra{\Accepting}$:
$$\mathtt{match}(\alpha \cdots \omega) = \brac{ \braket{\Accepting | T^\omega \cdots T^\alpha | s_0} \neq 0 }.$$

As a final treat, notice that the only property of nondeterminism that we actually use
is that $\Pow \S$ is representable as complex functions.
Thus, the above discussion still works for other kind of automata with this property
such as deterministic, probabilistic or quantum.

[^kleene-star]: The notation $X^*$ denotes the [Kleene Star](https://en.wikipedia.org/wiki/Kleene_star),
not the dual vector space.
Math likes to reuse symbols, I know.


Back to Our Example
-------------------


Recall our state machine recognizing whether a binary string
represents an integer divisible by three.
It had alphabet $\A = \{0, 1\}$ and states $\S = \{0, 1, 2\}$.
In vector form, it starts at $\ket{0}$ and accepts any string also finishing at $\ket{0}$.
We can extract the transition matrices by looking at the FA's edges,

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

Now, this is all the data we need to run the system.
As an example,
the string $0110$ represents $6$ and is accepted because

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
  \begin{bmatrix}
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


The Tensor Network Approach
===========================

Our vector discussion was well and good but it still lacks something.
Since the strings in $\A^\star$ had to be fixed beforehand,
the dynamics are not linear on them.
We made an open system parameterized on $s_0$ and $\Accepting$
while we want those to remain constant with varying input strings.

You can think of a _tensor network_ as a graph of lazily evaluated tensors.[^vect]
They are in one-to-one correspondence with Einstein summation notation
since instead of writing a tensor as its components,
they represent it as the result of contracting other tensors.
Previously, we represented the automaton
as an inner product $\braket{\Accepting | T^\omega \cdots T^\alpha | s_0}$.
By looking at all its elements as tensors, we find a tensor network.

[^vect]: For my categorist readers: They are exactly the string diagrams
in the category of $\mathtt{FinVect}(\C)$ of finite dimensional complex vector spaces.

```tikz
{ [ tn chain ]

  \node [covec, on chain] {$\Accepting$};
  \foreach \i / \a in {5/\omega,4/\gamma,3/x,2/\beta,1/\alpha} {
    \ifnum \i=3
      \node[on chain] {$\cdots$};
    \else
      \node [tensor, on chain] {$T^\a$};
    \fi
  }
  \node [vec, on chain] {$s_0$};
}
```

This is a "closed system" inasmuch as there are no free wires to connect an external input.
The string $\sigma$ is fixed and this network represents a single complex number.
How can we make this system linear on $\sigma$?
The answer is to look again at the transition function's type
but smartly uncurry it as a 3-tensor,

$$ \begin{array}{rcl}
  && \A \times \S  \to \Pow \S \\
  &\cong&
  \A \times \S \times \S \to \{0, 1\} \\
  &\subseteq&
  \A \times \S \times \S \to \C \\
  &\cong&
  \C^\A \otimes \C^\S \otimes \C^\S
\end{array}
$$

Let's call this tensor $T$.
Notice that contracting it with pure characters
gives rise to our previous matrices: $T^\alpha = T \ket{\alpha}$,
and we get a linear dependence on the input string.
Substitute this into our previous tensor network.

```tikz {id="mps-word" png=true}
{ [ tn chain, ]

  \node [covec, on chain] {$\Accepting$};
  \foreach \i / \a in {5/\omega,4/\gamma,3/x,2/\beta,1/\alpha} {
    \ifnum \i=3
    \node[on chain] {$\cdots$};
    \else
      \node [tensor, on chain] {$T$};
      { [start branch = U going above]
        \node [vec, on chain, shape border rotate=90] {$\a$};
      }
    \fi
  }
  \node [vec, on chain] {$s_0$};
}
```

By isolating the part of the network that does not depend on
the tensor product of characters $\bigotimes \ket{\alpha_i}$,
we arrive at an expression for the matching tensor.

```tikz
\draw[fill=lightgreen, rounded corners, very thick] (0, 0) rectangle (4, 1);
\foreach \x in {0.4, 1.2, 2.8, 3.6} {
  \draw[very thick] (\x, 1) -- (\x, 1.5);
}
\node at (2, 1.25) {$\cdots$};
\node at (2, 0.5) {$\mathtt{match}_N$};

\node (eq) at (5, 0.5) {$=$};

{ [ tn chain, ]

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

This kind of tensor network is known as a [_Matrix Product State_](https://tensornetwork.org/mps/) (MPS)
among physicists and as a _Tensor Train_ among numerical analysts.
Although I'm quite a fan of trains, let's go with MPS for this post.
They are a very compact representation for an $N$-tensor.
In our application, there are $N$ copies of
$T$---each requiring $A S^2$ coefficients---plus the $S$ coefficients
for each of $\bra{\Accepting}$ and $\bra{s_0}$.
Therefore, we got down from the exponential $A^N$ to only $NAS^2 + 2S$ degrees of freedom.
By precontracting $T$ with the initial and final states, we can reduce it even further to
$NAS^2 - 2AS(S-1)$.

```tikz
{ [ tn chain ]
  \node [tensor, on chain, fill=sorange] {}; { [start branch = U going above] \node [on chain] {}; }
  \node [tensor, on chain] {}; { [start branch = U going above] \node [on chain] {}; }
  \node[on chain] {$\cdots$};
  \node [tensor, on chain] {}; { [start branch = U going above] \node [on chain] {}; }
  \node [tensor, on chain, fill=sorange] {}; { [start branch = U going above] \node [on chain] {}; }
}
```

MPS also have very fast contraction algorithms,
requiring only $NAS^3$ operations for calculating an inner product.

Before closing this section,
notice that the MPS view "loses" the automaton's dynamic nature.
There is no more order of contractions, just linked tensors that statically
represent the whole process.


Turning into a Quantum Circuit
==============================

Tensor networks are great for quantum simulations,
but what if we want to represent our automaton on an actual quantum computer?
We will need to represent it as a [quantum circuit](https://en.wikipedia.org/wiki/Quantum_circuit) (QC).
For this post's purposes, a QC is a tensor network where all tensors are unitary.
You can then employ the usual methods to turn those gates (unitaries) into Pauli matrices or whatever.
Since it is a single-sided circuit, we will use $\ket{0}$ as its input.
Fortunately for us, MPS have a property called _gauge freedom_
which allow them to be rewritten using only isometries.
With this, it is possible to rewrite any MPS as a quantum circuit!

Our approach follows @preparing_quantum_2023 [Sec. 4.1] and @Huggins_2019 [Sec. IV.C]
and uses the QR decomposition to obtain unitaries.
The only hypothesis we need is that $S = A^k$, making $\C^\S = \bigotimes_{i=1}^k \C^\A$.
In general, we are working with qubits (so $A = 2$)
and all gates should be between them, so the states should match this.
If your FA does not match, just pad the state vectors and matrices with zeros.
To unclutter the notation, let's sometimes write how many tensor products
each wire represents.

Throughout this section,
we put the following lemma to good use.
It is a standard linear algebra construction.

:::{.Lemma data-title="Unitary Dilation"}
For an isometry $V \colon \C^m \to \C^n$ with $n = l m$,
there is a unitary matrix $U \colon \C^l \otimes \C^m \to \C^n$
such that
$$ U(\ket{\psi} \otimes \ket{0}) = V \ket{\psi}.$$

Or in tensor networks,

```tikz
\node[isometry] (V) {$V$};
\draw[very thick] (V.east) |- ++(0.5, 0);
\draw[very thick] (V.west) |- ++(-0.5, 0);

\node[right = of V] (eq) {$=$};

\node[unitary, minimum height = 1.5cm, right = of eq] (U) {$U$};
\draw[very thick] (U.east) |- ++(0.7, 0);
\draw[very thick] (U.150)  |- ++(-0.3, 0) node[plug=180] {};
\draw[very thick] (U.210) |- ++(-0.7, 0);
```
:::

Now, to decompose some states!
First of all, a quantum circuit must be normalized.
A global complex constant does not change its properties as a quantum state,
so let's work with
$$\ket{\psi} = \frac{1}{\norm{\mathtt{match}_N}_2} \ket{\mathtt{match}_N}.$$

The next step is to use the aforementioned gauge invariance
to rewrite $\ket{\psi}$ as an equivalent MPS where all tensors but the last are isometries.
We achieve this orthogonalization process via a series of QR decompositions.[^svd]

[^svd]: In the literature, people generally use the SVD for this step
because it allows truncating the inner dimensions optimally.
Since we are interested on the exact quantum circuit, both SVD and QR work.

For this next part, let's view the first tensor as 3-tensor with incoming
rank 0.

```tikz
{ [ tn chain]
  {[every join/.style = {very thick, "$k$"} ]
    \node [tensor, on chain] {}; { [start branch = U1 going above] }
    \node [tensor, on chain] {}; { [start branch = U2 going above] }
    \node[on chain] {$\cdots$};
    \node [tensor, on chain] {}; { [start branch = U3 going above] }
    \node [tensor, on chain] {}; { [start branch = U4 going above] }
  }

  \foreach \i in {1,...,4} {
    \begin{scope}[continue branch = U\i]
      \node [on chain] {$1$};
    \end{scope}
  }
}

\node [left = 5mm of chain-1] (p) {};
\draw[dotted] (p) edge ["{$0$}" midway] (chain-1);
```

We will repeated the same process below for every node but the last one.
Start by taking a tensor on the MPS and reshaping it into a $A^{(l+1)} \times A^k$ matrix.

```tikz
{ [ ]
\node[tensor] (V) {};
\draw[very thick] (V.north)  edge ["$1$"] ++(0, 0.5);
\draw[very thick] (V.west)   edge ["$l$"'] ++(-0.5, 0);
\draw[very thick] (V.east)   edge ["$k$"] ++(0.5, 0);

\node[right = of V] (eq) {$\to$};

\node[tensor, right = of eq] (U) {};
\draw[very thick] (U.180-45) edge ["$1$"'] ++(-0.5, 0);
\draw[very thick] (U.180+45) edge ["$l$"'] ++(-0.5, 0);
\draw[very thick] (U.east)   edge ["$k$"]  ++(0.5, 0);
}
```

If $l < k$,
apply a QR factorization to turn this matrix into a product between a unitary $U$
and a triangular matrix $R$.
From that and some reshaping, we get

```tikz
{ [on grid, node distance = 2cm]
\node[tensor] (T) {};
\draw[very thick] (T.north)  edge ["$1$"] ++(0, 0.5);
\draw[very thick] (T.west)   edge ["$l$"'] ++(-0.5, 0);
\draw[very thick] (T.east)   edge ["$k$"] ++(0.5, 0);

\node[right = of T] (eq) {$=$};

\node[unitary, right = of eq]     (V) {$U$};
\node[tensor,  right = 2cm of V] (R) {$R$};

\draw[very thick] (V.north)  edge ["$1$"] ++(0, 0.5);
\draw[very thick] (V.west) edge ["$l$"'] ++(-0.5, 0);
\draw[very thick] (V.east) edge ["$l+1$" {midway, above}] (R);

\draw[very thick] (R.east)  edge ["$k$"]  ++(0.5, 0);
}
```

When $l = k$, perform a _thin QR decomposition_ instead
to turn the tensor into a product between
an isometry $V$ and a triangular $R$.

```tikz
{ [on grid, node distance = 2cm]
\node[tensor] (T) {};
\draw[very thick] (T.north)  edge ["$1$"] ++(0, 0.5);
\draw[very thick] (T.west)   edge ["$k$"'] ++(-0.5, 0);
\draw[very thick] (T.east)   edge ["$k$"] ++(0.5, 0);

\node[right = of T] (eq) {$=$};

\node[isometry, right = of eq]    (V) {$V$};
\node[tensor,  right = 2cm of V] (R) {$R$};

\draw[very thick] (V.north)  edge ["$1$"] ++(0, 0.5);
\draw[very thick] (V.west) edge ["$k$"'] ++(-0.5, 0);
\draw[very thick] (V.east) edge ["$k$" {midway, above}] (R);

\draw[very thick] (R.east)  edge ["$k$"]  ++(0.5, 0);
}
```

With the aforestated lemma,
this isometry can be dilated into a unitary $U$ contracted with $\ket{0} \in \C^\A$.
Doing that and reshaping again.

```tikz
{ [on grid, node distance = 2cm]
\node[tensor] (T) {};
\draw[very thick] (T.north)  edge ["$1$"] ++(0, 0.5);
\draw[very thick] (T.west)   edge ["$k$"'] ++(-0.5, 0);
\draw[very thick] (T.east)   edge ["$k$"] ++(0.5, 0);

\node[right = of T] (eq) {$=$};

\node[unitary, right = of eq]    (V) {$U$};
\node[tensor,  right = 2cm of V] (R) {$R$};

\draw[very thick] (V.north)  edge ["$1$"] ++(0, 0.5);
\draw[very thick] (V.west) edge ["$k$"'] ++(-0.5, 0);
\draw[very thick] (V.east) edge ["$k$" {midway, above}] (R);

\draw[very thick] (R.east)  edge ["$k$"]  ++(0.5, 0);

\draw[very thick]
  (V.south) edge["1" {midway, left}] node[plug=-90, at end] {} ++(0, -0.5)
;
}
```

To orthogonalize a MPS, repeat this procedure for each tensor
keeping the unitary and contracting $R$ with the next site.
Notice that $l$ starts at zero and increases at every step
until it equals the bond dimension $k$.

```tikz {tikzlibrary="fit"}
{ [ tn chain ]
  \node [unitary, on chain] (V1) {$U_1$}; { [start branch = U going above] \node [on chain] {}; }
  \node[on chain] {$\cdots$};
  \node [unitary, on chain] (V) {$U_i$}; { [start branch = U going above] \node [on chain] {}; }
  \node [tensor, on chain] (R) {$R_i$};
  \node [tensor, on chain] (T) {}; { [start branch = U going above] \node [on chain] {}; }
  \node[on chain] {$\cdots$};
  \node [tensor, on chain] {}; { [start branch = U going above] \node [on chain] {}; }

  \node[fit=(R) (T), draw, dotted, "Contract" below] {};

}
\draw[very thick]
  (V.south) edge node[plug=-90, at end] {} ++(0, -0.3)
;
```

In Julia pseudocode, this process looks like this.

```julia
function orthogonalize!(psi::MPS) :: MPS
  N = length(psi)   # How many tensors in psi

  for i in 1:(N-1)
    if psi[i].prev < psi[i].next   # Compare the left dim with the right dim
      Q, R   = qr(psi)
      psi[i] = dilate(Q)   # Turn into unitary with |0>
    else
      Q, R   = thin_qr(psi)
      psi[i] = Q
    end

    psi[i+1] = contract(R, psi[i+1])
  end

  return psi
end
```

After orthogonalization, All tensors in the MPS become isometries except for the last one.
This MPS represents the same tensor but is much more structured.

```tikz
{ [ tn chain ]
  \node [unitary, on chain] (V1) {$U_1$}; { [start branch = U going above] \node [on chain] {}; }
  \node[on chain] {$\cdots$};
  \node [unitary, on chain] (Vi) {}; { [start branch = U going above] \node [on chain] {}; }
  \node [tensor, on chain, fill=sorange] {$T$}; { [start branch = U going above] \node [on chain] {}; }
}
\draw[very thick]
  (Vi.south) edge node[plug = -90, at end] {} ++(0, -0.3)
;
```

Now, what do we do with this last tensor?
Remember that the first step was normalizing the MPS such that $\braket{\psi|\psi} = 1$.
This is what is going to save the day!
After orthogonalizing, the normalization "moves" to just the last tensor.
To see that, build the inner product.

```tikz
{ [ tn chain ]
  \node [unitary, on chain] (V1) {$U_1$}; { [start branch = U going above] \node [on chain] {}; }
  \node[on chain] {$\cdots$};
  \node [unitary, on chain] (Vi) {}; { [start branch = U going above] \node [on chain] {}; }
  \node [tensor, on chain, fill=sorange] {$T$}; { [start branch = U going above] \node [on chain] {}; }
}
\draw[very thick]
  (Vi.south) edge node[plug = -90, at end] {} ++(0, -0.3)
;

{ [ tn chain ]
  \node [unitary, on chain, above = of V1] (U1) {$U_1^\dag$};
  \node[on chain] {$\cdots$};
  \node [unitary, on chain] (Ui) {};
  \node [tensor, on chain, fill=sorange] {$T^\dag$};
}

\draw[very thick]
  (Ui.north) edge node[plug = 90, at end] {} ++(0, 0.3)
;
```

Notice that being an isometry in this context means that

```tikz
{ [on grid, node distance = 1.5cm]
\node[unitary]        (U)  {$U$};
\node[unitary, above = of U] (Ut) {$U^\dag$};

\draw[very thick]
  (U) edge node (Z) [midway] {} (Ut)
  (U.south)  edge node[plug = -90, at end] {} ++(0, -0.3)
  (Ut.north) edge node[plug = 90,  at end] {} ++(0,  0.3)
  (U.east)   edge ++(0.5, 0)
  (Ut.east)  edge ++(0.5, 0)
  (U.west) [in = 180, out = 180] edge (Ut.west);
;

\node[right = of Z] (eq) {$=$};

\node[right = 3cm of U] (a) {};
\node[above = of a] (b) {};

\draw[very thick]
  (a.west) [in = 180, out = 180] edge (b.west);
;

}
```

With the first unitary $U_1$ satisfying the same without the arch to the left.
By iterating this equation, the inner product is left as

```tikz
{ [on grid, node distance = 1.5cm]
\node[tensor,fill = sorange]        (U)  {$T$};
\node[tensor,fill = sorange, above = of U] (Ut) {$T^\dag$};

\draw[very thick]
  (U) edge node (Z) [midway] {} (Ut)
  (U.west) [in = 180, out = 180] edge (Ut.west);
;

\node[right = of Z] (eq2) {$=$};

\node[right = of eq2, tensor]  {$1$};

\node[left = of Z] (eq1) {$=$};

\node[vec, shape border rotate = -90, left = 3cm of U] (p)  {$\psi$};
\node[vec, shape border rotate =  90, above = of p] (pt) {$\psi$};

\draw[very thick] (p) -- (pt);
}
```

We conclude that $T$ is normalized when viewed as a vector in $\C^\S \otimes \C^\A$.
What is nice about that?
Well, a normalized vector is equivalent to a 1-column isometry in $\C \to \C^\S \otimes \C^\A$,
so we can complete it to a unitary by plugging $k+1$ kets to it!

Putting it all together,
the new MPS takes the following form:

```tikz {tikzlibrary="fit"}
{ [start chain, every join/.style = very thick, node distance = 5mm]
  \node [unitary, on chain] (V1) {}; { [start branch = U1 going above] }
  \node [unitary, on chain] (V2) {}; { [start branch = U2 going above] }
  \node[on chain] {$\cdots$};
  \node [unitary, on chain] (V3) {}; { [start branch = U3 going above] }
  \node [unitary, on chain] (V4) {}; { [start branch = U4 going above] }

  \foreach \i in {1,...,4} {
    \begin{scope}[continue branch = U\i]
      \node [on chain, join] {$1$};
    \end{scope}
  }
}

{ [every above delimiter/.style = {yshift = -2mm},]
  \node [below = 5mm of V4, above delimiter = \}] {$k+1$};
  \node [below = -0.5mm of V4] {\tiny $\cdots$};
}

% Inner bonds
\draw[very thick]
  (chain-1) edge["$1$"] (chain-2)
  (chain-2) edge["$2$"] (chain-3)
  (chain-3) edge["$k$"] (chain-4)
  (chain-4) edge["$k$"] (chain-5)
;

\draw[very thick]
  (V3.south) edge node[plug = -90, at end] {} ++(0, -0.3)
  (V4.-120)  edge node[plug = -90, at end] {} ++(0, -0.3)
  (V4.-60)   edge node[plug = -90, at end] {} ++(0, -0.3)
;
```

In case it is not clear why the MPS above is a quantum circuit, worry not.
All it takes is rotating it to the horizontal and sliding the boxes.
Here's an example with $N = 5$ and $k = 2$
where each wire is one qubit.

```tikz {tikzlibrary="fit"}
\matrix (c) [
  matrix of nodes, nodes in empty cells,
  column sep = 0.7cm, row sep = 4mm,
  text width = 0.5cm,
  every node/.style = {minimum height = 4mm},
  ]
{
  &  & & & & & \\
  &  & & & & & \\
  &  & & & & & \\
  &  & & & & & \\
  &  & & & & & \\
};

\draw[very thick]
  (c-1-1) |- (c-1-7)
  (c-2-1) |- (c-2-7)
  (c-3-1) |- (c-3-7)
  (c-4-1) |- (c-4-7)
  (c-5-1) |- (c-5-7)
;

\node[unitary, fit=(c-1-6)] (U1) {$U_1$};
\node[unitary, fit=(c-2-5)(c-1-5)] {$U_2$};
\node[unitary, fit=(c-3-4)(c-1-4)] {$U_3$};
\node[unitary, fit=(c-4-3)(c-2-3)] {$U_4$};
\node[unitary, fit=(c-5-2)(c-3-2)] {$U_5$};

\foreach \i in {1,...,5} {
  \node[covec] at (c-\i-1) {$0$};
}
```

Now that's a quantum circuit!

Conclusion
----------

Well, we saw today that finite automata have a neat description as quantum systems.
We can cast them as Matrix Product States to perform classical simulations
or even as quantum circuits if you want to run in the real thing.
This representation is just the tip of the iceberg though!

You can play with your FAs by subjecting them to anything
a quantum system would.
For example, MPS can be viewed as _Born machines_ allowing you to extract independent samples
of finite strings from the automaton.
You can also use low-rank approximations (such as SVD truncation)
to compress these states while still being similar to the original FA.

The sky is the limit! Have fun with this new tool.
