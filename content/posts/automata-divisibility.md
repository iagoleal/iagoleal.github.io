---
title: Finite Automata for Divisibility
keywords: [math, automata]
date: 2025-05-08
description:
suppress-bibliography: true
---

\def\A{\mathcal{A}}
\def\S{\mathcal{S}}
\def\F{\mathcal{F}}
\def\suc{\mathtt{suc}}
\def\orbit{\mathcal{O}}

\def\Z{\mathbb{Z}}


Construction
============

Our objective is to construct a deterministic finite automaton (DFA)
$(\S, \A, s_0, \F, \delta)$
which determines the language $L(b, m)$ of numbers written in base-$b$
divisible by $m$.


How do we construct this automaton?
The input consists of digits written in base-$b$,
thus the alphabet is $\A = \{0, \ldots, b-1\}$.
For the states,
the idea is to consider them as the machine's "memory"
remembering the remainder up until now.
So, when dividing a number by $m$, there are $m$ possible remainders
and the state set is $\S = \Z_m = \{0, \ldots, b-1\}$.
Also, for simplicity, we consider the empty string to equal zero,
and use it as the initial state.
You can always add an extra state to deal with this edge case,
but we will ignore this in the post.
Since we are interested in divisibility,
we only accept strings ending at the state $0$.

The seemingly most complicated part is the transition function.
What should it be?
Let's consider a word $x = x_N \ldots x_0 \in \A^\star$ and write $[x]_b$ for the number it represents,
$$ [x]_b \coloneqq \sum_{i = 0}^N x_i b^i.$$

The main insight is to look at how concatenating a digit $k$ translates into the number, i.e.,
$$ [x \cdot k]_b = \sum_{i = 0}^N x_i b^{i+1} + k = [x]_b \cdot b + k.$$
By considering this expansion modulo $m$,
$$ [x \cdot k]_b \equiv [x]_b \cdot b + k \mod m,$$
We arrive at an expression for the transition function $\delta \colon \S \times \A \to \S$.
Since the current state is the remainder of the input up until now,
the next remainder amounts to
$$ \boxed{\delta(s, k) = s b + k \pmod m}.$$

The transition table
====================

I don't know about you,
but constructing the transitions from modular arithmetic seems a bit like cheating to me.
It's indeed elegant but makes the transition structure too opaque.
Let's try to find some simpler relations between the states.
Also remember that $\delta$ can be cast as a transition table where column $k$
determines the transition from each state when the DFA reads $k$.
I'll use both views interchangeably.

Instead of directly defining the transition, cast it as a recursive relation
$$ \begin{aligned}
t(0, 0)   &= 0 \\
t(s, k+1) &\equiv t(s, k) + 1 \mod m \\
t(s+1, k) &\equiv t(s, k) + b \mod m
\end{aligned}
$$
The first thing it tells us is that if we know the first column,
all others are easy to find.
Just cyclically compute the number's successor and you're good to go.

For the first column,
it starts at $0$ and adds $b$ for each number in $\Z_m$, producing a sequence
$$0, b, 2b, 3b, \ldots, (m-1)b.$$
This is the _orbit_ $\orbit(b, m)$ of $b$ in $\Z_m$.
The most interesting part is that the orbit's length equals $m$ if and only if $b$ and $m$ are coprime.
In general it has $\tilde{m} \coloneqq \frac{m}{\gcd(b, m)}$ elements before repeating,
meaning that we only have to consider,
$$ \orbit(b, m) = \left\{0, b, 2b, \ldots, (\tilde{m} - 1)b \right\}.$$
For us, this is interesting because when $b$ shares a factor with $m$, the orbit is shorter,
and the table must have repeated rows --- making some states indistinguishable.
Perhaps we can use this to optimize our automata?

A slightly less canonical construction
--------------------------------------

To be sincere, at first, I thought this meant we could construct an automata for $L(b, m)$
using $\orbit(b, m)$ as state set.
I was too naïve, however.
Look for example at $\orbit(10, 5) = \{ 0\}$.
We sure need more than one state, right?

Not everything is lost though.
We can reduce to the orbit with a caveat:
when merging, one must distinguish between accepting and non-accepting states.
Thus, we can only collapse transition rows _greater than_ 0.
This turns as another automata construction with $1 + \frac{m}{\gcd(b, m)}$ states.
More concretely, define the state set to be
$$ \S = \left\{0, 1, \ldots, \tilde{m} \right\},$$
The state $0$ represents the accepting state,
while the others are the equivalence classes of equal rows,
with $\tilde{m}$ representing the non-accepting rows equal to the zeroth one.
Confusing?
Just think that we are collapsing all states $s > 0$ who have the same rows.

For the transition, the base case stays the same:
$$ \delta(0, 0) = 0,$$
For the columns, we need to map 


Are these machines minimal?
===========================

Generally, there are many DFAs recognizing the same language.
Hence, to save on resources,
it is common to look for the one with the least states, called the language's _minimal_ DFA.

So, do you think the automata we just constructed are minimal?
As it stands, only when $\gcd(b, m) = 1$.
For example, to check if a decimal number is divisible by 1000,
our first construction yields a DFA with 1000 states
while the orbit-based one still needs 101 states.
Yet, just checking the powers of 10 would be enough.

```tikz {tikzlibrary="automata"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]}]
  \node[state, initial, accepting, initial text= ] (q_0)                        {0};
  \node[state]                                     (q_1) [below left = of q_0]  {1};
  \node[state]                                     (q_2) [below right= of q_1]  {10};
  \node[state]                                     (q_3) [below right= of q_0]  {100};

  \path[->] 
    (q_0) edge[loop above] node              {\texttt{0}}     ()
    (q_0) edge[]           node [above left] {\texttt{[1-9]}} (q_1)
    (q_1) edge[]           node [above]      {\texttt{0}}     (q_2)
    (q_1) edge[loop left]  node []           {\texttt{[1-9]}} ()
    (q_2) edge[]           node [above]      {\texttt{0}}     (q_3)
    (q_2) edge[bend left]  node []           {\texttt{[1-9]}} (q_1)
    (q_3) edge[]           node [above]      {\texttt{0}}     (q_0)
    (q_3) edge[]           node [above]      {\texttt{[1-9]}} (q_1)
  ;
}
```

This idea generalizes for any case where $m = b^k$.
The minimal automaton for $L(b, b^k)$ has $\Z_k$ as state space,
which only contains $1 + \log_b m = 1 + k$ states,
an exponential decrease compared to the previous construction!
What it basically does is keeping track of the number of final zeros on the string,

$$ \begin{aligned}
t(0, 0) &= 0 \\
t(s, 0) &= s + 1 \pmod k \\
t(s, k) &= 1
\end{aligned}
$$




------------------------------------------------------------------------------------------------------------------------------------------

https://content.wolfram.com/sites/19/2010/02/Sutner.pdf


$$ t(s+1, 0) \equiv t(s, 0) + b \mod m $$

Starting from zero,


