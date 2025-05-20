---
title: Arithmancia Automatorum
subtitle: A Painfully Explicitly Construction of The Minimal DFA for Divisibility
keywords: [math, automata, modular arithmetic]
date: 2025-05-08
description:
  Did you know that Finite Automata can recognize divisibility?
  To find one such automaton, the procedure is simple
  but constructing the minimal one requires a lot of modular arithmetic.
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

\def\divides{\mid}
\def\ceil#1{\lceil #1 \rceil}
\def\Ceil#1{\left\lceil #1 \right\rceil}

\def\ca#1{\colorbox{lightgreen}{$#1$}}
\def\cb#1{\colorbox{thistle}{$#1$}}

\def\by#1#2{\left.{#1}\right|_{{#2}}}

\def\layer#1#2{#1 \triangleright #2}

```{=tex}
\definecolor{lightgreen}{HTML}{90EE90}
\definecolor{thistle}{HTML}{D8BFD8}

\tikzset{
  sdiv/.style = {fill = lightgreen},
  spow/.style = {fill = thistle},
}
```


Recently I've been thinking a lot about integer divisibility.
It was all well and good until, for an unrelated project,
a needed an interesting but relatively simple finite automaton to as an example,
and stumbled into a construction for dividing numbers with DFAs.
This made me fall into a rabbit hole and this post redacts everything I brought back from inside that madness.
I don't promise it will be useful.
I don't promise it will be sane.
But I at least hope it will be novel to you, my dear reader.

Today we explore the answers to two related questions.

:::Aside
Can a DFA determine the language $L(b, m)$ of numbers written in base $b$
which are divisible by $m$?
:::

The answer is a resounding **yes**
and the construction, which we tackle in the next section,
is standard and very simple to come up with.
This takes us to the next and much darker question.

:::Aside
What is the minimal DFA recognizing $L(b, m)$?
:::

At first, I thought it would be a simple formula just using some
$\gcd$'s or something like that but it turned out to be much more involved.
After 3 attempts, documented on the proceeding sections,
I discovered that in 2004 @alexeev_2004 proved a formula for how many states this automaton must have
but with no comments on how to actually construct the transitions.

So today let's explore how to write such automata
in a painfully explicit way.
As a starter, the standard construction.

Notation
--------

By $a \divides b$ we mean that $a$ divides $b$.
The greatest common divisor has the usual $\gcd$ notation,
and since we'll make heavy use of their ratios, define
$$ {\by a b} \coloneqq {\frac a {\gcd(a, b)}}.$$
This one is a non-standard notation,
but helps with the mess of symbols we're about to confront.
I write $\ceil{\cdot}$ for the ceiling function,
i.e., the smallest integer greater than a real number.
For a sequence of digits $\omega$,
$$ [\omega]_b \coloneqq \sum_{i = 0}^N \omega_i b^i.$$


Sometimes I'll also use regex notation to declutter some figures.
For example, $\texttt{[abcd]}$ means one of $a, b, c,$ or $d$
and $\texttt{[1-9]}$ means all digits from 1 till 9.


First Construction: Modular Transitions
=======================================

Our objective is to construct a deterministic finite automaton
$A_1 = (\S, \A, s_0, \F, \delta)$
which determines the language $L(b, m)$ of base-$b$ numbers
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
Let's consider a word $\omega = \omega_N \ldots \omega_0 \in \A^\star$
and write $[\omega]_b$ for the number it represents,
$$ [\omega]_b \coloneqq \sum_{i = 0}^N \omega_i b^i.$$

The main insight is to look at how concatenating a digit $k$ translates into numbers, i.e.,
$$ [\omega \cdot k]_b = \sum_{i = 0}^N \omega_i b^{i+1} + k = [\omega]_b \cdot b + k.$$
By considering this expansion modulo $m$,
$$ [\omega \cdot k]_b \equiv [\omega]_b \cdot b + k \mod m,$$
We arrive at an expression for the transition function $\delta \colon \S \times \A \to \S$.
Since the current state is the remainder of the input up until now,
the next remainder amounts to
$$ \boxed{\delta(s, k) = s b + k \pmod m}.$$
<!-- $$ \boxed{s \xrightarrow{k} (s b + k \bmod m)}.$$ -->

Examples
--------

Below is an automaton checking for divisibility by 3 in decimal.


```tikz {tikzlibrary="automata"}
{ [shorten >=1pt, node distance=3cm, on grid, auto, >={Stealth[round]},
    every edge quotes/.style = {font = {\scriptsize\tt}}
  ]
  \node[state, initial, accepting, initial text= ] (0) []  {$0$};
  \node[state]                                     (1) [below left  = of 0] {$1$};
  \node[state]                                     (2) [below right = of 0] {$2$};

  \path[->]
    (0) edge[loop above, "{[0369]}"] ()
    (0) edge[bend right, "{[147]}"'] (1)
    (0) edge[bend left,  "{[258]}"] (2)
    (1) edge[loop left,  "{[0369]}"] ()
    (1) edge[sloped,     "{[258]}"'] (0)
    (1) edge[bend right, "{[147]}"'] (2)
    (2) edge[loop right, "{[0369]}"] ()
    (2) edge[sloped,     "{[147]}"'] (0)
    (2) edge[,           "{[258]}"'] (1)
  ;
}
```

Second Construction: Orbit-based Transitions
============================================

The edges from modular arithmetic are elegant
but make the transition structure too opaque.
Indeed, there is some hard to see redundancy in there.
Let's try to find simpler relations between the states.

Instead of directly defining the transition, cast it as a recursive relation
$$ \begin{aligned}
\delta(0, 0)   &= 0 \\
\delta(s, k+1) &\equiv \delta(s, k) + 1 \mod m \\
\delta(s+1, k) &\equiv \delta(s, k) + b \mod m
\end{aligned}
$$
The first thing it tells us is that if we know the action of $0$,
all others are easy to find.
Just cyclically compute the number's successor and you're good to go.

For zero, it starts at $0$ and sequentially adds $b$.
Or equivalently,
$$\delta(s, 0) = sb \pmod m$$
This produces a sequence
$$0, b, 2b, 3b, \ldots, (m-1)b$$
Called the _orbit_ $\orbit(b, m)$ of $b$ in $\Z_m$.
The most interesting part is that the orbit's length equals $m$ if and only if $b$ and $m$ are coprime.
In general it has $\by{m}{b}$ elements before repeating,
meaning that we only have to consider,
$$ \orbit(b, m) = \left\{0, b, 2b, \ldots, (\by{m}{b} - 1)b \right\}.$$
For us, this is interesting because whenever $b$ shares a factor with $m$,
the orbit is shorter,
making the transitions from certain states indistinguishable.
Perhaps we can use this to improve our automata?


Let's Start Trimming
--------------------

To be sincere, at first, I thought this meant we could construct an automata for $L(b, m)$
whose state set is $\orbit(b, m)$.
I was too naïve, however.
Look for example at $\orbit(10, 5) = \{ 0\}$.
We sure need more than one state, right?

Not everything is lost though.
We can reduce to the orbit with a caveat:
when merging, one must distinguish between accepting and non-accepting states.
Thus, we can only collapse states _greater than_ 0.
Roughly, we collapse $x, y \neq 0$ if $xb \equiv yb \mod m$.
From this, we find another automata with $1 + \frac{m}{\gcd(b, m)}$ states.
More concretely, define an automaton $A_2$ whose states are
$$\S = \ca{\Z_{\by{m}{b}}} \sqcup \{ \cb 0 \}.$$
We view it as a cyclic group augmented by a point $\cb{0}$
and use colors to distinguish between the points from the orbit and this additional state
for accepting.

The state $\cb 0$ is the initial and accepting state,
while the others are equivalence classes of remainders
mapping to the same point on the orbit of $b$,
with $\ca{0}$ representing the non-accepting states mapping to zero.
Confusing?
Just think that we are collapsing all states $s > 0$ who have the same transitions.

To map the states from our original automaton to this new one,
we map $0$ to $\cb 0$ while cycling other states to their position on the orbit,
$$\begin{aligned}
  \phi &\colon \Z_m \to \ca{\Z_{\by{m}{b}}} \sqcup \{ \cb 0 \} \\
  0    &\mapsto \cb 0 \\
  x    &\mapsto \ca{x \bmod \by m b}
\end{aligned}
$$


The transition is thus just the composition between the previous one and this collapsing function,
$\tilde{\delta} = \phi \circ \delta$.
Or explicitly,
$$ \begin{aligned}
\tilde \delta(s, k) = \begin{cases}
  \cb{0}, & m \divides r \\
  \ca{r \bmod \by{m}{b}}, & \text{otherwise}
\end{cases}
  \\ \text{ where } r = sb + k \bmod m
\end{aligned}
$$

Notice that if $b$ and $m$ are coprime,
$\phi$ is injective but not surjective and
the automaton $A_2$ has an unreachable state $\ca 0$.
On the other hand, when they do share a factor,
the full orbit takes the whole codomain and $\phi$ is surjective --- although not necessarily injective.
In this case, we get a smaller automaton recognizing the same language.

Example: Divisibility by 6 in Binary
------------------------------------

By using only modular arithmetic, binary divisibility by 6 requires 5 states
and produces the automaton below.

```tikz {tikzlibrary="automata"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]},
    every edge quotes/.style = {font = {\scriptsize\tt}}
  ]
  \node[state, initial, initial where = above, accepting, initial text= ] (0) []  {$0$};
  \node[state]                 (1) [right = of 0] {$1$};
  \node[state]                 (2) [right = of 1] {$2$};
  \node[state]                 (3) [below = of 0] {$3$};
  \node[state]                 (4) [right = of 3] {$4$};
  \node[state]                 (5) [right = of 4] {$5$};

  \path[->]
    (0) edge["0", loop left] ()
    (0) edge["1"] (1)
    (1) edge["0"] (2)
    (1) edge["1", bend right] (3)
    (2) edge["0", bend right] (4)
    (2) edge["1"] (5)
    (3) edge["0"] (0)
    (3) edge["1", bend right] (1)
    (4) edge["0", bend right] (2)
    (4) edge["1"] (3)
    (5) edge["0"] (4)
    (5) edge["1", loop right] ()
  ;
}
```

Notice in this diagram that the transitions from $s$ and $s + 3$ are indistinguishable.
We thus collapse these states according to
$$\begin{array}{rcl|rcl}
0 &\to & \cb{0} & 3 &\to & \ca{0} \\
1 &\to & \ca{1} & 4 &\to & \ca{1} \\
2 &\to & \ca{2} & 5 &\to & \ca{2}
\end{array}
$$

This produces a reduced automaton recognizing the same language.

```tikz {tikzlibrary="automata"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]},
    every edge quotes/.style = {font = {\scriptsize\tt}}
  ]

  { [every state/.style = spow]
    \node[state, initial, initial where = left, accepting, initial text= ] (0) []  {$0$};
  }
  { [every state/.style = sdiv]
    \node[state]                 (1) [above = of 0] {$1$};
    \node[state]                 (2) [right = of 1] {$2$};
    \node[state]                 (3) [left  = of 1] {$0$};
  }

  \path[->]
    (0) edge["0", loop right] ()
    (0) edge["1"] (1)

    (1) edge["0", bend left] (2)
    (1) edge["1", bend left] (3)

    (2) edge["0", bend left] (1)
    (2) edge["1", loop right] ()

    (3) edge["0"'] (0)
    (3) edge["1", bend left] (1)
  ;
}
```

Notice that except for the transition $\ca 0 \xrightarrow{0} \cb 0$,
the green part is an automaton measuring divisibility by 3.
This is no coincidence. As we will later see,
a binary number is divisible by $6$ if and only if it is a number divisible by $3$
followed by at least one $0$.

Third Construction: Power to The States
=======================================

Generally, there are many DFAs recognizing the same language.
Hence, to save on resources,
it is common to look for the one with the least states, called the language's _minimal_ DFA.

So, do you think any of our constructions produces the minimal automaton?
As it stands, the first one only does when $\gcd(b, m) = 1$
and the second is also not always guaranteed.
For example, to check if a decimal number is divisible by 1000,
our first construction $A_1$ yields a FDA with 1000 states
while the orbit-based $A_2$ still needs 101 states.
Yet, just checking the powers of 10 would be enough.

```tikz {tikzlibrary="automata"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]},
   every edge quotes/.style = {font = {\scriptsize\tt}}
  ]
  \node[state]                                     (q_1) []  {};
  \node[state]                                     (q_2) [below = of q_1]  {};
  \node[state]                                     (q_3) [right = of q_2]  {};
  \node[state, initial, accepting, initial where = above, initial text= ] (q_0) [right = of q_3]  {};

  \path[->]
    (q_0) edge["{0}"    , loop right]  ()
    (q_1) edge["{0}"    , ]            (q_2)
    (q_2) edge["{0}"    , ]            (q_3)
    (q_3) edge["{0}"    , ]            (q_0)
    (q_0) edge["{[1-9]}'", bend right]  (q_1)
    (q_1) edge["{[1-9]}", loop left]   ()
    (q_2) edge["{[1-9]}", bend left]   (q_1)
    (q_3) edge["{[1-9]}'", bend right]  (q_1)
  ;
}
```

This idea generalizes for any case where $m = b^d$.
$L(b, b^d) = 0^\star + \A^\star 0^d$,
the language whose words are either all zero or end with at least $k$ zeros.
One can recognize this by simply keeping a record of the zeros,
which takes $1 + \log_b m = 1 + d$ states,
an exponential decrease compared to the previous constructions!
By using ${d, \ldots, 0}$ as state space,
the transition keeps track of how many zeros are left,
$$ \begin{aligned}
\delta(0, 0) &= 0 \\
\delta(s, 0) &= s - 1 \\
\delta(s, k) &= d
\end{aligned}
$$

Despite the exponential gain,
this process only works for dividing powers of the base --- a very specific case.
Nonetheless, it is possible to mix it with our previous constructions
in a way that extends to other divisors.
The process is kinda convoluted though, so let's do it in steps
by first generalizing to the languages $L(b, xb^d)$ and $L(a^c, a^d)$.

Recognizing $L(b, x b^d)$ with $x$ and $b$ coprime
--------------------------------------------------

Suppose that $m = x b^d$ with $\gcd(b, x) = 1$.
There is a smart way to determine divisibility by $m$ requiring only $x + d$ states.
The idea is that coprime factors can be checked separately.
First, notice that thanks to coprimality $m \divides n$
if and only if $x \divides n$ and $b^d \divides n$.
Furthermore, we know from our previous discussion
that trailing zeros never "lose" divisibility.
Thus, a characterization follows.

:::Lemma
Let $b$ and $m$ be as above.
A number written in base $b$
is divisible by $m$ if and only if it is either only zeros
or a number divisible by $x$ followed by $d$ zeros.
In other words, $L(b, m) = 0^\star + L(b, x)0^d$.
:::

Now, we can build an automaton that first checks for divisibility by $x$
and then proceeds to count trailing zeros.
Let's again use colors to represent those distinct kinds of states.
$$\S = \ca{\Z_x} \sqcup \cb{\Z_d}.$$

The starting and accepting state is $\cb{0}$.
For the transitions,
the green states act as the usual divisibility automaton automaton
except that reading a zero on $\ca{0}$ takes you to the purple states.
$$ \begin{aligned}
\delta(\ca{0}, 0) &= \cb{d-1} \\
\delta(\ca{s}, k) &= \ca{bs + k \pmod x}
\end{aligned}
$$

Meanwhile, the purple state $\cb{s}$ represents a number divisible by $x$
but which still has to read at least $s$ zeros to be divisible by $b^k$.
Think of it as augmenting $\ca{0}$ with a counter.
Reading a zero takes you one step further while reading anything else
takes you back to the green states.

$$ \begin{aligned}
\delta(\cb{0}, 0) &= \cb{0} \\
\delta(\cb{s}, 0) &= \cb{s-1} \\
\delta(\cb{s}, k) &= \delta(\ca{0}, k)
\end{aligned}
$$

As an illustration, all such machines will have this same shape.

```tikz {tikzlibrary="automata,fit"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]},
   every edge quotes/.style = {font = {\scriptsize\tt}},
   every state/.style={node font = \footnotesize, minimum size = 1cm},
  ]
  \node[state, sdiv] (d0) {$0$};
  \node[state, sdiv] (d1) [below left = of d0] {};
  \node[state, sdiv] (d2) [above left = of d0] {};
  \node[color = lightgreen]            (dl) at ($(d1)!0.5!(d2)$) {$\cdots$} ;

  \node[state, spow]     (sq) [right = 3cm of d0] {$d-1$};
  \node[color = thistle] (ll) [right = of sq] {$\cdots$};
  \node[state, spow, initial, accepting, initial where = above, initial text= ] (s0) [right = of ll]  {$0$};


  \node[draw, dotted, fit=(d0) (d1) (d2) (dl), "Divisibility by $x$" below] (D) {};

  \path[->]
    (d0)  edge["{0}"] (sq)
    (s0)  edge["{0}", loop right] ()
    (sq)  edge["{0}"] (ll)
    (ll)  edge["{0}"] (s0)

    (sq) edge["{k}" above, bend left]  (D)
    (ll) edge["{k}" above, out = 225, in = -40]  (D)
    (s0) edge["{k}" above, out = 225, in = -50]  (D)
  ;
}
```


### Example: Divisibility by 300 in Decimal

Write $b = 10$, $m = 300 = 3 \cdot 10^2$.
From the previous discussion,
we can recognize $L(b, m)$ with a 6 state automaton checking for divisibility by 3 and 2 trailing zeros.
Its diagram is below.
Notice that the green part looks the same as our previous automaton for $m = 3$
and that the purple states act the same as $\ca 0$.

```tikz {tikzlibrary="automata"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]},
    every edge quotes/.style = {font = {\scriptsize\tt}, sloped}
  ]
  {[ every state/.style = {sdiv}, node distance=3cm]
    \node[state, initial text= ] (0) []  {$0$};
    \node[state]                                     (1) [above left = of 0] {$1$};
    \node[state]                                     (2) [below left = of 0] {$2$};
  }

  {[ every state/.style = {spow} ]
    \node[state, spow]     (s1) [right = of 0] {$1$};
    \node[state, spow, initial, accepting, initial where = above, initial text= ] (s0) [right = of s1]  {$0$};
  }

  \path[->]
    (0) edge[loop left,  "{[369]}" sloped=false]  ()
    (0) edge[bend right, "{[147]}"]               (1)
    (0) edge[bend left,  "{[258]}"']              (2)
    (1) edge[loop left,  "{[0369]}" sloped=false] ()
    (1) edge[,           "{[258]}"']              (0)
    (1) edge[bend right, "{[147]}"']              (2)
    (2) edge[loop left,  "{[0369]}" sloped=false] ()
    (2) edge[sloped,     "{[147]}"]               (0)
    (2) edge[,           "{[258]}"']              (1)
    ;

  \path[->]
    (0)   edge["{0}"] (s1)
    (s1)  edge["{0}"] (s0)
    (s0)  edge["{0}" sloped=false, loop right] ()
  ;

  \path[->]
    (s1)  edge[bend left,  "{[369]}"'] (0)
    (s1)  edge[bend right, "{[147]}"]  (1)
    (s1)  edge[bend left,  "{[258]}"'] (2)
    (s0)  edge[bend right, "{[369]}"]  (0)
    (s0)  edge[bend right, "{[147]}"]  (1)
    (s0)  edge[bend left,  "{[258]}"'] (2)
  ;
}
```

Recognizing $L(a^c, a^d)$ --- Non-matching Powers
-------------------------------------------------

For the next step, we ditch the coprime factor but let $b = a^c$ and $m = a^d$
be powers that do not necessarily match.
Let's first find a formula for divisibility in this case
and then proceed to construct its automaton.
As we will see, it requires $1 + \ceil{\frac d c}$ states.

The process requires juggling some indices
but I promise to try my best to make it the less painful possible.
For a number $n$,
we look at its base-$b$ representation
$$ n = \sum_{i = 0}^N n_i b^i = \sum_{i = 0}^N n_i a^{ci}.$$

Let's see which of those terms we can ignore or fix modulo $m$.
For this, we break the sum into (possibly empty) groups.
Using Euclidean division, write $d = cq + r$ with $0 \le r < c$
and partition the indices according to $q$:

$$ n = \sum_{i = 0}^{q-1} n_i a^{ci} + n_q a^{cq} + \sum_{j = {q+1}}^N n_j a^{cj}.$$

A case analysis on the groups reveal the form they must take.

- $i > q$: Those terms are all divisible by $a^d$ because
$$ci \ge c(q+1) = cq + c > cq + r = d.$$
And can, thus, be _ignored_ without remorse.

- $i < q$: Notice that $a^{cq + r} \divides n \implies n = \alpha \cdot b^q$.
Since the last $q$ digits of $n$ never sum up to a whole $b^q$ factor,
they _must be zero_.

- $i = q$: The remaining term is $n_q a^{cq}$. The only constraint for $a^d$ to divide it
is that $a^r \divides n_q$.
This is possible because $r < c$ and $0 \le n_q < a^c$.

Putting this together, we get a relatively simple formula.

:::Lemma
Let $b = a^c$ and $m = a^d$ and write $d = cq + r$.
A word $\omega$ represents a base-$b$ number divisible by $m$
if it is either all zeros or has $q$ trailing zeros and the digit before that is divisible by $a^r$.
In other words, $L(a^c, a^d) = 0^\star + \A^\star\omega_00^q$
with $\omega_0$ divisible by $a^r$.
:::

This lets us construct an automaton with $2 + q$ state.
However, notice that using Euclidean division helped with the formulation
but it has a caveat:
whenever $r = 0$, i.e. $c$ divides $d$,
the condition on the last digit vanishes and we end up with a spurious state
checking if a digit is divisible by 1.
A smart trick solves this problem:
instead of division,
consider the ceiling $Q = \ceil{\frac d c}$ and write
$d = c Q - r$, with $0 \le r < c$.

:::Lemma
Let $b = a^c$ and $m = a^d$, $Q = \ceil{\frac d c}$, and write $d = cQ - r$.
A word $\omega$ represents a base-$b$ number divisible by $m$
if it is either all zeros or has $Q-1$ trailing zeros and the digit before that is divisible by $a^{c-r}$.
In other words, $L(a^c, a^d) = 0^\star + \A^\star\omega_00^{Q-1}$
with $\omega_0$ divisible by $a^{c-r}$.
:::

This way, the condition on $\omega_0$ becomes just another zero if the exponents align.

To construct the automaton, let $Q = \ceil{\frac d c}$, write $d = cQ - r$,
and take as states
$$\S = \{ \ca 0 \} \sqcup \cb{\Z_{\ceil{\frac d c}}}.$$
The starting and accepting state is, as usual, $\cb{0}$.
The state $\ca 0$ represents the ignored digits and
the machine leaves it as soon as it encounters something divisible by $a^{c-r}$,
$$
\delta(\ca{0}, k) = \begin{cases}
  \cb{Q-1}, & a^{c-r} \divides k  \\
  \ca{0}, & \text{otherwise}
\end{cases}
$$

After that, it turns into the already familiar zero counting machine
with one addition:
getting a digit divisible by $a^{c-r}$
takes you back to the start of the $purple$ chain.
$$ \begin{aligned}
\delta(\cb{0}, 0) &= \cb{0} \\
\delta(\cb{s}, 0) &= \cb{s - 1} \\
\delta(\cb{s}, k) &= \delta(\ca{0}, k) \\
\end{aligned}
$$

As an illustration,
all such automata roughly have the shape below.

```tikz {tikzlibrary="automata" usepackage="amssymb"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]},
   every edge quotes/.style = {font = {\scriptsize\tt}},
   every state/.style={node font = \footnotesize, minimum size = 1cm},
  ]
  \node[state, sdiv] (sa) {$0$};
  \node[state, spow] (sq) [right = 3cm of sa] {$Q-1$};
  \node[]            (ll) [right = of sq] {$\cdots$};
  \node[state, spow, initial, accepting, initial where = above, initial text= ] (s0) [right = of ll]  {$0$};

  \path[->]
    (s0) edge["{0}", loop right] ()
    (sq) edge["{0}"] (ll)
    (ll) edge["{0}"] (s0)

    (sa) edge["$a^{c-r} \divides \mathtt{k}$"] (sq)
    (sq) edge["$a^{c-r} \divides \mathtt{k}$", out = 75, in = 105, loop, above] ()
    (s0) edge["$a^{c-r} \divides \mathtt{k}$"', out = -225, in = 60]  (sq)
    (ll) edge["$a^{c-r} \divides \mathtt{k}$"', out = -225, in = 45]  (sq)

    (sa) edge["$a^{c-r} \nmid \mathtt{k}$", loop left]  ()
    (sq) edge["$a^{c-r} \nmid \mathtt{k}$", out = 225, in = -30, below, near start]  (sa)
    (ll) edge["$a^{c-r} \nmid \mathtt{k}$", out = 225, in = -45]  (sa)
    (s0) edge["$a^{c-r} \nmid \mathtt{k}$", out = 225, in = -60]  (sa)
  ;
}
```

### Example: Divisibility by 32 in Octal

For $b = 8 = 2^3$ and $m = 32 = 2^5$,
we calculate $5 = 2\cdot3 - 1$.
Thus, the automaton has $3$ states, with transitions from green to purple
happening only when $2^{3-1} = 4$, i.e., $k = 0$ or $4$.

```tikz {tikzlibrary="automata" usepackage="amssymb"}
{ [shorten >=1pt, node distance=3cm, on grid, auto, >={Stealth[round]},
   every edge quotes/.style = {font = {\scriptsize\tt}},
   every state/.style={node font = \footnotesize, minimum size = 1cm},
  ]
  \node[state, sdiv] (sa) {$0$};
  \node[state, spow] (sq) [right = 3cm of sa] {$1$};
  \node[state, spow, initial, accepting, initial where = above, initial text= ] (s0) [right = of sq]  {$0$};

  \path[->]
    (s0)  edge["{0}", loop right] ()
    (sa)  edge["{[04]}"] (sq)
    (sq)  edge["{0}"] (s0)

    (sa) edge["{[123567]}", loop left]  ()
    (sq) edge["{[123567]}", bend left]  (sa)
    (s0) edge["{[123567]}", out = 225, in = -60]  (sa)

    (sq) edge["{4}", out = 75, in = 105, loop, above] ()
    (s0) edge["{4}"', bend right]  (sq)
  ;
}
```


A Power-Counting Automaton
--------------------------

Alright, it's been a long while but all steps are finally ready
to build our third general automaton $A_3$.
The idea is to factor $b = a^c$ and $m = x a^d$ with $\gcd{a, x} = 1$.
Since $x$ and $a^d$ are coprime,
we can join the previous constructions to check divisibility by $m$
using $x + \ceil{\frac d c}$ states.
Also, notice that this factorization is always possible,
even if we need to take some of $x = 1$, $d = 0$, or $c = 1$.
It just won't be of much use then.

:::Lemma
Factor $b = a^c$ and $m = xa^d$, and write
$Q = \ceil{\frac d c}$ such that $d = cQ - r$.
A word represents a base-$b$ number divisible by $m$
if it is either all zeros or of the form $\omega \omega_0 0^{Q-1}$,
with
$$\begin{aligned}
  [\omega \omega_0]_b &\equiv 0 \mod x \\
  \omega_0            &\equiv 0 \mod a^{c-r}.
\end{aligned}
$$
:::

Let's again use colors to represent the distinct kinds of states:
$$\S = \ca{\Z_x} \sqcup \cb{\Z_Q}.$$
The starting and accepting state is $\cb{0}$.
The purple states represent numbers already divisible by $x$
and follow the zero-counting mechanism.
$$ \begin{aligned}
\delta(\cb{0}, 0) &= \cb{0} \\
\delta(\cb{s}, 0) &= \cb{s - 1} \\
\delta(\cb{s}, k) &= \delta(\ca{0}, k) \\
\end{aligned}
$$

The green states store the remainder by $x$ with a detail:
we use $\ca{0}$ for divisibility only by $x$ and $\cb{Q-1}$
for divisibility by $x$ and $a^{c-r}$.
$$
\delta(\ca{s}, k) = \begin{cases}
  \cb{Q-1}, & a^{c-r} \divides k \text{ and } x \divides (bs + k) \\
  \ca{bs + k \pmod x} , & \text{otherwise}
\end{cases}
$$

Despite all the indirections, I hope this DFA makes sense to you!
These are the optimal automata whenever the base $b$ is a prime power,
i.e., $a$ is prime.
Also, notice that when $b$ and $m$ are coprime, there are no purple states
and it becomes the original automaton $A_1$.

Fourth Construction: Layers for Digit Lookup
============================================

For compound numbers, we've improved our original automaton in two different ways.
What do they have in common?
Well, have states clustered into different "meanings"
and both perform some kind of digit lookup besides checking divisibility!
For the powers it is easy, since we divide by a coprime factor and then proceed to count zeros.

The orbit is more interesting to us, though.
Although we got to it by removing redundant states,
another view is that it is a DFA checking divisibility
at the same time that it checks the last digit's admissibility.
I'll explain it further in a bit,
but before constructing these machines with lookup,
let's check the literature to see what is
the minimum amount of states that a divisibility DFA must have.


Are these machines minimal?
---------------------------

So, what is the size of the minimal automaton for the general case $L(b, m)$?
The answer turns out to be rather complicated!
@alexeev_2004 proved that the minimum necessary amount of states is
$$ \min_{N \ge 0}\left[ \sum_{i=0}^{N - 1} \by{b^i}{m} + \by{m}{b^N}  \right].$$
Quite a mouthful!
The paper also proves that the minimum is achieved by the last $N$
before it starts to increase.
I'll skip the proof since it is complicated (but quite readable!)
and does not elucidate much on how to actually construct these automata
--- our true interest here.
You can check the paper, if you're curious.
Let's nevertheless take a look at the possible terms on this minimum.

Each possible term corresponds to a DFA recognizing $L(b, m)$.
Let's compare that with our automata so far.

- $A_1$ corresponds to $N = 0$, with sum $m$.
- $A_2$ corresponds to $N = 1$, with sum
$$ \frac{m}{\gcd(m, b)} + \frac{b^0}{\gcd(m, b^0)} = \frac{m}{\gcd(m, b)} + 1.$$
- $A_3$ corresponds to $N = \ceil{\frac d c}$. By writing $b = a^c$ and $m = x a^d$,
the sum is
$$\frac{x a^d}{\gcd\underbrace{(x a^d, a^{cN})}_{=a^d}} + \sum_{i=0}^{N - 1} \frac{a^{ci}}{\gcd\underbrace{(a^{ci}, xa^d)}_{=a^{ci}}} = x + \textstyle{\ceil{\frac d c}}.$$


On Digits and Orbits
--------------------

You may remember some divisibility rules from school
such as "An even number has an even last digit" or "A number is divisible by 5 if it ends in either 0 or 5".
Why do 2 and 5 have such rules, while 3 or 7 do not?
In general, we can check the last digit of a number by looking at it modulo the base:
$$ x_0 \equiv x \mod b.$$

Now, if it is a multiple of $m$, i.e. $x = \alpha m$,
$$ x_0 \equiv x \equiv \alpha m \equiv \alpha' \gcd(m, b) \mod b.$$

We notice that the last digit $x_0$ is on the orbit $\orbit(m, b)$,
making it a multiple of $\gcd(b, m)$.
This explains the middle school formulas,
since $\gcd(5, 10) = 5$ and $\gcd(2, 10) = 2$,
while $3$ and $7$ are coprime to $10$ and, thus, accept any last digit.
We can expand this analysis to the $l$ last digits by considering $x$ modulo $b^l$
and obtain a similar result.

:::Lemma
The last $l$ digits of a multiple of $m$ written in base $b$ are in
$$\orbit(m, b^l) = \left\{ j \cdot \gcd(m, b^l) \mid  0 \le j < \by{b^l}{m} \right\}.$$
:::

Now suppose there's a power $N$ such that $\gcd(m, b^{N-1}) = m$.
This happens when $m$ is a power of a factor of $b$, for example.
In this case, the condition on the digits is not only necessary but also sufficient
for divisibility, since
$$ x \equiv \sum_{i=0}^{N-1} x_i b^i  \mod m,$$
And we can check that the last $N$ digits are on the orbit $\orbit(m, b^{N-1})$.

How many states does an automaton need to check all such suffixes?
Well, some arithmetic shows that to check each digit,
we need $\sum_{i=0}^{N - 1} \by{b^i}{m}$ states.
Do you remember this term from Alexeev's formula?
Well, this suggests we're getting somewhere.

For a general base and divisor, the above is not enough though.
To deal with them, besides the suffixes, it is also necessary to keep track
of a coprime factor.
We will see in the next section how to construct such a machine.


### Example: Divisibility by 25 in Decimal

Since $b = 10 = 2\cdot5$ and $m = 125 = 5^2$,
we can test divisibility by $m$ by checking a string's last $2$ digits.
The language is $0^\star + \A^\star\orbit(m, b^2)$,
where the orbit is
$$\orbit(25, 10^2) = \{00, 25, 50, 75 \}.$$
We need 4 states in total:
One to discriminate the last digit, two other to discriminate the second-to-last
and a last one to represent nodes without any admissible suffix.

The transitions try to advance the suffix or act exactly the same as a node on the previous layer.

```tikz {tikzlibrary="automata" usepackage="amssymb"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]},
   every edge quotes/.style = {font = {\scriptsize\tt}},
   every state/.style={node font = \footnotesize, minimum size = 1cm},
  ]
  \node[state] (2-0) at (-1, 0) {};

  \node[state] (1-0) [above right = 3cm of 2-0] {};
  \node[state] (1-1) [below right = 3cm of 2-0] {};

  \node[state, initial, accepting, initial where=above, initial text = ] (0-0) at (5, 0) {};

  \path[->]
    (2-0) edge["{[05]}", bend left] (1-0)
    (2-0) edge["{[27]}"', bend right] (1-1)
    (2-0) edge["{[134689]}", loop left] ()
  ;

  \path[->]
    (0-0) edge["0", loop right] ()

    (0-0) edge["{5}"', bend right] (1-0)
    (0-0) edge["{[27]}", bend left] (1-1)
    (0-0) edge["{[134689]}"' near start] (2-0)
  ;

  \path[->]
    (1-0) edge["0"] (0-0)

    (1-0) edge["5", loop above] ()
    (1-0) edge["{[27]}" near start, bend right] (1-1)
    (1-0) edge["{[134689]}", sloped] (2-0)
  ;

  \path[->]
    (1-1) edge["5"] (0-0)

    (1-1) edge["0" near start, bend right] (1-0)
    (1-1) edge["{[27]}", loop below] ()
    (1-1) edge["{[134689]}"', sloped] (2-0)
  ;

}
```

Layering Up for Suffixes
------------------------


---------------------------------------------------------------------------------------

<!-- For the general construction, -->
<!-- the state space is divided into $N+1$ blocks. -->
<!-- The first corresponds to the orbit $\orbit(b^N, m)$ and acts as a remainder for divisibility. -->
<!-- The other blocks have $b^i / \gcd(m, b^i)$ elements and correspond to the powers "trimmed by  -->




------------------------------------------------------------------------------------------------------------------------------------------
