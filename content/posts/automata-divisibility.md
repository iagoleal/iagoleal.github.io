---
title: Finite Automata for Divisibility
keywords: [math, automata]
date: 2025-05-08
description:
suppress-bibliography: true
---

<style>
.Missing {
  text-align: center;
  width:  100%;
  height: 300px;
  background-color: gray;
  border: black 1px;
}
</style>

\def\S{\mathcal{S}}

\def\A{\Sigma}
\def\F{\mathcal{F}}

\def\orbit{\mathcal{O}}

\def\Z{\mathbb{Z}}

\def\Sb{\tilde{\mathcal{S}}}


Construction
============

Our objective is to construct a deterministic finite automaton (DFA)
$A_1 = (\S, \A, s_0, \F, \delta)$
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

The main insight is to look at how concatenating a digit $k$ translates into numbers, i.e.,
$$ [x \cdot k]_b = \sum_{i = 0}^N x_i b^{i+1} + k = [x]_b \cdot b + k.$$
By considering this expansion modulo $m$,
$$ [x \cdot k]_b \equiv [x]_b \cdot b + k \mod m,$$
We arrive at an expression for the transition function $\delta \colon \S \times \A \to \S$.
Since the current state is the remainder of the input up until now,
the next remainder amounts to
$$ \boxed{\delta(s, k) = s b + k \pmod m}.$$

:::Missing
Example for this automaton
:::


The transition table
====================

I don't know about you,
but constructing the transitions from modular arithmetic seems a bit like cheating to me.
It's indeed elegant but makes the transition structure too opaque.
Let's try to find some simpler relations between the states.
Also remember that $\delta$ can be cast as a transition table where column $k$
determines the transition from each state when the DFA reads $k$.
I'll use both views interchangeably.

:::Missing
Transition table for small automaton
:::

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
Perhaps we can use this to improve our automata?

A slightly less canonical construction
--------------------------------------

To be sincere, at first, I thought this meant we could construct an automata for $L(b, m)$
whose state set is $\orbit(b, m)$.
I was too naïve, however.
Look for example at $\orbit(10, 5) = \{ 0\}$.
We sure need more than one state, right?

Not everything is lost though.
We can reduce to the orbit with a caveat:
when merging, one must distinguish between accepting and non-accepting states.
Thus, we can only collapse transition rows _greater than_ 0.
From this, there is another automata construction with $1 + \frac{m}{\gcd(b, m)}$ states.
More concretely, define $A_2 \coloneqq (\Sb, \A, s_0, \F, \tilde{\delta})$,
where the state set is
$$ \Sb = \left\{0, 1, \ldots, \tilde{m} \right\} \cong \{0\} \sqcup \Z_{\tilde m},$$
We will shortly make use of this view as a cyclic group augmented by a point $0$.

The state $0$ represents the accepting state,
while the others are the equivalence classes of equal rows,
with $\tilde{m}$ representing the non-accepting rows equal to the zeroth one.
Confusing?
Just think that we are collapsing all states $s > 0$ who have the same rows.

To map the states from our original automaton to this new one,
we keep $0$ fixed while cycling the other elements from $1$ to $\tilde{m}$,
thus "collapsing" anything larger than $\tilde{m}$:
$$\begin{aligned}
  \phi &\colon \Z_m \to \tilde{\S} \\
  0    &\mapsto 0 \\
  x    &\mapsto (x - 1 \bmod \tilde m) + 1
\end{aligned}
$$

If the second formula seems confusing,
just know that it is the construction you would use to cycle an array in a language with 1-based indexes,
such as Lua, Julia, or Fortran.

The transition is thus just the composition between the previous one and this collapsing function,
$$ \boxed{\tilde{\delta} = \phi \circ \delta}.$$

:::Missing
New transition table for 
:::

Notice that if $b$ and $m$ are coprime,
$\phi$ is injective but not surjective and
the automaton $A_2$ has an unreachable state $\tilde{m}$.
On the other hand, when they do share a factor,
the full orbit takes the whole codomain function is surjective --- although not necessarily injective.
This way, we get a smaller automaton recognizing the same language.


Are these machines minimal?
===========================

Generally, there are many DFAs recognizing the same language.
Hence, to save on resources,
it is common to look for the one with the least states, called the language's _minimal_ DFA.

So, do you think any of our constructions produces the minimal automaton?
As it stands, the first one only does when $\gcd(b, m) = 1$
and the second is also not always guaranteed.
For example, to check if a decimal number is divisible by 1000,
our first construction $A_1$ yields a DFA with 1000 states
while the orbit-based $A_2$ still needs 101 states.
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
which only contains $1 + k = 1 + \log_b m$ states,
an exponential decrease compared to the previous constructions!
What it basically does is keeping track of the number of final zeros on the string,

$$ \begin{aligned}
t(0, 0) &= 0 \\
t(s, 0) &= s + 1 \pmod k \\
t(s, k) &= 1
\end{aligned}
$$

So, what is the size of the minimal automaton for $L(b, m)$?
The answer turns out to be quite complicated!
@alexeev_2004 proved that the minimum necessary amount of states is
$$ \min_{N \ge 0}\left[ \frac{m}{\gcd(m, b^N)} + \sum_{i=0}^{N - 1} \frac{b^i}{\gcd(b^i, m)} \right].$$
Quite a mouthful!
The proof is complicated (but quite readable!)
and does not elucidate how to actually construct these automata
--- our true interest here.
Thus, at this point, I think it is easier and faster to just feed our previous automata
to Hopcroft's and minimize them.
I'll leave this task for you.




------------------------------------------------------------------------------------------------------------------------------------------

https://content.wolfram.com/sites/19/2010/02/Sutner.pdf

