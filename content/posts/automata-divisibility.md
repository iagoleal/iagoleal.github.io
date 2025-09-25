---
title: Arithmancia Automatorum
subtitle: A Painfully Explicitly Construction of The Minimal DFA for Divisibility
keywords: [math, automata, modular-arithmetic]
date: 2025-05-23
description:
  Did you know that Finite Automata can recognize divisibility?
  To find one such automaton, the procedure is simple
  but constructing the minimal one requires a lot of modular arithmetic.
thumbnail: "300base10.svg"
suppress-bibliography: true
---


\def\S{\mathcal{S}}

\def\A{\Sigma}
\def\F{\mathcal{F}}

\def\orbit{\mathcal{O}}

\def\Z{\mathbb{Z}}

\def\Sb{\tilde{\mathcal{S}}}

\def\divides{\mid}
\def\ceil#1{\lceil #1 \rceil}
\def\floor#1{\lfloor #1 \rfloor}
\def\Ceil#1{\left\lceil #1 \right\rceil}

\def\ca#1{\colorbox{lightgreen}{$#1$}}
\def\cb#1{\colorbox{thistle}{$#1$}}

\def\by#1#2{\left.{#1}\right|_{{#2}}}

\def\layer#1#2{#1 \triangleright #2}

\def\rem{\mathrm{rem}}
\def\expect{\mathrm{expect}}

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
a needed an interesting but relatively simple finite automaton to use as an example,
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
We start the journey at the standard construction
and refine it till reaching the minimal one.

Notation
--------

By $a \divides b$ we mean that $a$ divides $b$.
The greatest common divisor has the usual $\gcd$ notation,
and since we make heavy use of their ratios, let's define
$$ {\by a b} \coloneqq {\frac a {\gcd(a, b)}}.$$
This one is a non-standard notation,
but helps with the mess of symbols we're about to confront.
For a rational $x$,
denote by $\floor{x}$ and $\ceil{x}$ its floor and ceiling,
i.e., the tightest integers satisfying $\floor{x} \le x \le \ceil{x}$.
A sequence of digits $\omega$ represents a number in base $b$ defined as
$$ [\omega]_b \coloneqq \sum_{i = 0}^N \omega_i b^i.$$


Sometimes I'll also use regex notation to declutter some figures.
For example, $\texttt{[abcd]}$ means one of $\mathtt{a, b, c,}$ or $\mathtt d$
and $\texttt{[1-9]}$ means all digits from 1 till 9.


Construction I: Modular Transitions
===================================

Our objective is to construct a deterministic finite automaton
$A_1$ accepting the language $L(b, m)$ of base-$b$ numbers
divisible by $m$.
How do we construct this machine?
The input consists of digits written in base-$b$,
thus the alphabet is $\A = \{0, \ldots, b-1\}$.
For the states,
the idea is to consider them as the machine's "memory"
remembering the remainder up until now.
So, when dividing a number by $m$, there are $m$ possible remainders
and the state set is $\S_1 = \Z_m = \{0, \ldots, m-1\}$.
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
$$ [\omega \cdot k]_b = \sum_{i = i}^N \omega_i b^{i} + k = [\omega]_b \cdot b + k.$$
By considering this expansion modulo $m$,
$$ [\omega \cdot k]_b \equiv [\omega]_b \cdot b + k \mod m,$$
We arrive at an expression for the transition function $\delta_1 \colon \S_1 \times \A \to \S$.
Since the current state is the remainder of the input up until now,
the next remainder amounts to
$$\delta_1(s, k) = s b + k \pmod m.$$

Example: Divisibility by 3 in Decimal
-------------------------------------

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

Construction II: Orbit-based Transitions
========================================

The previous construction just using modular arithmetic is indeed elegant
but makes the transition structure too opaque.
There is some hard to see redundancy in there that we're not taking advantage of.
Let's thus try to find simpler relations between the states.

Instead of directly defining the transition, cast it as a recursive relation
$$ \begin{aligned}
\delta_1(0, 0)   &= 0 \\
\delta_1(s, k+1) &\equiv \delta_1(s, k) + 1 \mod m \\
\delta_1(s+1, k) &\equiv \delta_1(s, k) + b \mod m
\end{aligned}
$$
The first thing it tells us is that if we know the action of $0$,
all others are easy to find.
Just cyclically compute the number's successor and you're good to go.

For zero, it starts at $0$ and sequentially adds $b$.
Or equivalently,
$$\delta_1(s, 0) = sb \pmod m$$
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
we need an extra state to distinguish acceptable last digits.
Roughly, we collapse the states according to the relation
$$x \sim y (xb \equiv yb \mod m)$$
but notice that although two remainders may act the same for outgoing transitions,
they may have different behaviours for incoming transitions.
In particular, ending at _zero_ accepts a string
not ending in the equivalence class of zero.
To deal with this, we add an extra accepting state meaning "finishing at true zero".

From this, we find another automata with $1 + \frac{m}{\gcd(b, m)}$ states.
More concretely, define an automaton $A_2$ whose states are
$$\S_2 = \{ \cb 0 \} \sqcup \ca{\Z_{\by{m}{b}}}.$$
We view it as a cyclic group augmented by a point $\cb{0}$
and use colors to distinguish between the points from the orbit and this additional state.

The state $\cb 0$ is the initial and accepting state,
while $\ca r$ is the smallest possible remainder
representing $rb \in \orbit(b, m)$.
Keep in mind that although $\ca{0}$ and $\cb 0$ have the same transitions,
they are distinct.
$\ca 0$ represents the zeroth position on the orbit
where the last transition had a non-zero remainder modulo $m$.
Confusing?
You could think that we are collapsing all states $s > 0$ with the same transitions.

The transition for this automaton checks whether $rb + k$
is divisible modulo $m$, which takes us to the accepting state.
In case it fails, it only has to store the orbit position of the new remainder,
$$
\delta_2(s, k) = \begin{cases}
  \cb{0}, & m \divides r \\
  \ca{r \bmod \by{m}{b}}, & \text{otherwise}
\end{cases} \\
      \text{ where }   r = sb + k \bmod m
$$

Notice that if $b$ and $m$ are coprime,
state $\ca 0$ is unreachable and this automaton is worse than the previous one.
On the other hand, when they do share a factor,
every state is reachable.
In this case, we get a smaller automaton recognizing the same language
with a reduction by $\gcd(b, m)$ on the amount of states,
which could be substantial.

Example: Divisibility by 6 in Binary
------------------------------------

By using only modular arithmetic, binary divisibility by 6 requires 5 states
and produces the automaton below.

```tikz {tikzlibrary="automata,fit"}
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
We thus collapse them into $3$ states and add an extra state to check for final divisibility.
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

Construction III: Power to The States
=====================================

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
the green states act as the usual divisibility automaton
except that reading a zero on $\ca{0}$ starts to read a suffix,
$$ \begin{aligned}
\delta(\ca{0}, 0) &= \cb{d-1} \\
\delta(\ca{s}, k) &= \ca{bs + k \pmod x}
\end{aligned}
$$

Meanwhile, the purple state $\cb{s}$ represents a number divisible by $x$
but which still has to read at least $s$ zeros to be divisible by $b^k$.
Think of it as augmenting $\ca{0}$ with a counter.
Reading a zero takes you one step further while reading anything else
takes you back to checking divisibility by $x$,

$$ \begin{aligned}
\delta(\cb{0}, 0) &= \cb{0} \\
\delta(\cb{s}, 0) &= \cb{s-1} \\
\delta(\cb{s}, k) &= \delta(\ca{0}, k)
\end{aligned}
$$

As an illustration, all such machines will have roughly the same shape.

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
and that the purple states act the same as $\ca 0$
for any input besides $0$.

```tikz {tikzlibrary="automata" id="300base10"}
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
And can thus be _ignored_ without remorse.

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

This lets us construct an automaton with $2 + q$ states.
However, notice that although using Euclidean division helped with the formulation,
it has a caveat:
whenever $r = 0$, i.e. $c$ divides $d$,
the condition on the last digit vanishes and we end up with a spurious state
checking if a digit is divisible by 1.
A smart trick solves this problem:
instead of division,
consider the ceiling[^floor-ceil] $Q = \ceil{\frac d c}$ and write
$d = c Q - r$, with $0 \le r < c$.

[^floor-ceil]: This is the same division procedure,
but instead of $d = c \floor{\frac{d}{c}} + R$,
we use $d = c \ceil{\frac{d}{c}} - r$.

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
takes you back to the start of the suffix-checking chain,
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
The idea is to factor $b = a^c$ and $m = x a^d$ with $\gcd(a, x) = 1$.
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
$$\S_3 = \ca{\Z_x} \sqcup \cb{\Z_Q}.$$
The starting and accepting state is $\cb{0}$.
The purple states represent numbers already divisible by $x$
and follow the zero-counting mechanism.
$$ \begin{aligned}
\delta_3(\cb{0}, 0) &= \cb{0} \\
\delta_3(\cb{s}, 0) &= \cb{s - 1} \\
\delta_3(\cb{s}, k) &= \delta_3(\ca{0}, k) \\
\end{aligned}
$$

The green states store the remainder by $x$ with a detail:
we use $\ca{0}$ for divisibility only by $x$ and $\cb{Q-1}$
for divisibility by $x$ and $a^{c-r}$.
$$
\delta_3(\ca{s}, k) = \begin{cases}
  \cb{Q-1}, & a^{c-r} \divides k \text{ and } x \divides (bs + k) \\
  \ca{bs + k \pmod x} , & \text{otherwise.}
\end{cases}
$$

Despite all the indirections, I hope this DFA makes sense to you!
These are the minimal automata whenever the base $b$ is a prime power,
i.e., $a$ is prime.
Also, notice that when $b$ and $m$ are coprime, there are no purple states
and it becomes the original automaton $A_1$.
While for $d = c$, it becomes the orbit-based automaton $A_2$
--- although the orbit-based construction did not require $b$ to divide $m$,
so it is not a strict generalization.


Construction IV: Layers for Digit Lookup
========================================

For compound numbers, we've improved our original automaton in two different ways.
What do they have in common?
Well, both use states clustered into different "meanings"
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
@alexeev_2004 proved that the states required by a divisibility automaton are at least
$$ \min_{N \ge 0}\left[ \sum_{i=0}^{N - 1} \by{b^i}{m} + \by{m}{b^N}  \right].$$
Quite a mouthful!
The paper also proves that the minimum is achieved by the last $N$
before the total sum starts to increase.
I'll skip the proof since it is complicated (but quite readable!)
and does not elucidate much on how to actually construct the automata's transitions
--- our true interest here.
You can check the paper if you're curious.
Let's nevertheless take a look at the possible terms on this minimum.

Each possible $N$ corresponds to a DFA recognizing $L(b, m)$.
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
In general, we check the last digit of a number by looking at it modulo the base:
$$ x \equiv \sum_{i=0}^T x_i b^i \equiv x_0 \equiv x \mod b.$$

Now, if it is a multiple of $m$, i.e. $x = \alpha m$,
$$ x_0 \equiv x \equiv \alpha m \equiv \alpha' \gcd(m, b) \mod b.$$

We notice that the last digit $x_0$ is on the orbit $\orbit(m, b)$,
making it a multiple of $\gcd(b, m)$.
This explains the middle school formulas,
since $\gcd(5, 10) = 5$ and $\gcd(2, 10) = 2$,
while $3$ and $7$ are coprime to $10$ and, thus, accept any last digit.
We can expand this analysis to the last $p$ digits by considering $x$ modulo $b^p$
and obtain a similar result.

:::Lemma
The last $p$ digits of a multiple of $m$ written in base $b$ are in the orbit
$$\orbit(m, b^p) = \left\{ j \cdot \gcd(m, b^p) \mid  0 \le j < \by{b^p}{m} \right\}.$$
:::

Another interesting observation is that there is an overlap between
$\orbit(x, y)$ and $\orbit(y, x)$.
This is going to be important, so let's write it as another lemma.

:::Lemma
Write $\bar{\orbit}$ for the orbit viewed as a set of integers.
Then, they satisfy
$$\begin{aligned}
\bar{\orbit}(x, y) \cap \bar{\orbit}(y, x) &= \big\{\, j \cdot \gcd(x, y) \mid 0 \le j < \min(\by{x}{y}, \by{y}{x}) \,\big\} \\
  &= \bar{\orbit}(\gcd(x, y),\, \min(x, y))
\end{aligned}
$$
:::


Now suppose there's a power $N$ such that $\gcd(m, b^{N-1}) = m$.
This happens when $m$ is a power of a factor of $b$, for example.
In this case, the condition on the digits is not only necessary but also sufficient
for divisibility, since
$$ x \equiv \sum_{i=0}^{N-1} x_i b^i  \mod m,$$
and we can check that the last $N$ digits are on the orbit $\orbit(m, b^{N-1})$.

How many states does an automaton need to check all such suffixes?
Well, it needs $\by{b^{p-1}}{m}$ states to discriminate whether the $p$-th digit
starts an element of the orbit $\orbit(m, b^p)$.
Some arithmetic shows that it amounts to a total of
$\sum_{i=0}^{N - 1} \by{b^i}{m}$ states for $N$ digits.
Do you remember this term from Alexeev's formula?
This suggests we're getting somewhere.

For a general base and divisor, the above is not enough though.
To deal with them, besides the suffixes, it is also necessary to keep track
of a coprime factor.
We will see in the next section how to construct such a machine.


### Example: Divisibility by 25 in Decimal

Since $b = 10 = 2\cdot5$ and $m = 125 = 5^2$,
to test divisibility by $m$, we check a string's last $2$ digits.
The orbits are
$$\begin{align*}
 \orbit(25, 10^0) &= \{0\} \\
 \orbit(25, 10^1) &= \{0, 5\} \\
 \orbit(25, 10^2) &= \{00, 25, 50, 75 \}
\end{align*}
$$
We need $4 = \frac{1}{1} + \frac{10}{5} + \frac{25}{25}$ states in total:
One to discriminate the last digit, two other to discriminate the second-to-last
and a last one to represent nodes without any admissible suffix.

The transitions try to advance the suffix or act exactly the same as a node on the previous layer.

```tikz {tikzlibrary="automata,graphs" usepackage="amssymb"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]},
   every edge quotes/.style = {font = {\scriptsize\tt}},
   every state/.style={node font = \footnotesize, minimum size = 1cm},
  ]
  \node[state] (2-0) at (0, 0) {};

  \node[state] (1-0) at (3, 2)  {};
  \node[state] (1-1) at (3, -2)   {};

  \node[state, initial, accepting, initial where=above, initial text = ] (0-0) at (6, 0) {};

  \path[->]
    (2-0) edge["{[05]}", bend left] (1-0)
    (2-0) edge["{[27]}"', bend right] (1-1)
    (2-0) edge["{[134689]}", loop left] ()
  ;

  \path[->]
    (1-0) edge["0", bend left] (0-0)

    (1-0) edge["5", loop above] ()
    (1-0) edge["{[27]}" near start, bend right] (1-1)
    (1-0) edge["{[134689]}", sloped] (2-0)
  ;

  \path[->]
    (1-1) edge["5"', bend right] (0-0)

    (1-1) edge["0" near start, bend right] (1-0)
    (1-1) edge["{[27]}", loop below] ()
    (1-1) edge["{[134689]}"', sloped] (2-0)
  ;

  \path[->]
    (0-0) edge["0", loop right] ()

    (0-0) edge["{5}", sloped] (1-0)
    (0-0) edge["{[27]}"', sloped] (1-1)
    (0-0) edge["{[134689]}"' very near start] (2-0)
  ;

}
```

Layering Up for Suffixes
------------------------

Now, it is the time for our final construction --- the most complicated so far.
For each $N \ge 0$ we construct a divisibility automaton checking for $N$-digit suffixes.
What should be its states and transitions?
First, consider the original machine $A_1$
and suppose we calculated remainder $r$ up until now.
By reading $N$ digits, it arrives at
$$ r b^N + \sum_{i=0}^{N-1} \omega_i b^i \pmod m.$$

Thus in a machine with $N$-digit lookup,
two remainders are indistinguishable if they map to the same elements of $\orbit(b^N, m)$.
Like we previously did in the orbit-based automaton,
we merge remainders according to the relation
$$x \sim y = (xb^N \equiv yb^N \bmod m).$$
From this, there are $\by{m}{b^N}$ distinguishable remainders.
Those form the "divisibility part of the automaton".
For the remaining part,
we need a machine capable of identifying $N$-digit suffixes.
From the previous section, it requires $\sum_{i=0}^{N - 1} \by{b^i}{m}$ states
to identify all orbits $\orbit(m, b^i)$,
leaving us with state set
$$\S_4 = \bigsqcup_{i=0}^{N-1} \cb{\Z_{\by{b^i}{m}}} \sqcup \ca{\Z_{\by m {b^N}}}.$$

We again use colors to differentiate between (green) states storing remainders
and (purple) states expecting an input.
Unfortunately, just the colors are not enough. So we index with natural numbers $p \in \{0, \ldots, N\}$,
where $\layer{p}{s}$ means the state $s$ on layer $p$, i.e., expecting $p$ more digits.
The $N$th layer is the green one and represents the remainder before starting to read a suffix.
The purple states index the orbits they are _waiting for_,
with $\cb{\layer p s}$ being the state that requires
the first digit of the $s$-th element in the orbit $\orbit(m, b^p)$,
that is, $s \cdot \gcd(m, b^p)$.

Let's build the transition then.
To make the notation uniform, define some helpers: $p^- = \min(p-1, 0)$ and $p^+ = \max(p+1, N)$
represent, respectively, the previous and next layer.
Also, we define a remainder function taking a state to the remainder it represents modulo $m$.
On the last layer it is just the state,
$$\rem(\ca{\layer N s}) = s,$$
while on the others, it must take the value it expects to a zero remainder in $p$ steps,
$$\rem(\cb{\layer p s})b^p + s\cdot\gcd(m, b^p) \equiv 0 \mod m.$$

This is a modular linear equation.
Rewrite it as an equation on integers and arrange terms to
$$\rem(\cb{\layer p s})b^p + \zeta m = -s\gcd(m, b^p).$$
Fortunately, every term is a multiple of $\gcd(m, b^p)$
and we can solve this equation using
[Bézout's identity](https://en.wikipedia.org/wiki/B%C3%A9zout's_identity),
which you can calculate via the [extended Euclidean algorithm](https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm)
to obtain
$$\alpha b^p + \beta m = \gcd(m, b^p).$$
Thus, we get an expression for the remainder as
$$\rem(\cb{\layer p s}) = -s \alpha.$$

It is also useful to revert this procedure
with a function turning a remainder into an expected $p$-digit suffix,
$$ \expect(p, r) \coloneqq -r b^p \pmod{m}.$$
Even more useful it to find what's the required index for this suffix
by reducing this equation to
$$ \expect'(p, r) \coloneqq -r \by{b^p}{m} \pmod{\by{m}{b^p}}.$$
Notice that for an arbitrary remainder,
this expected value is not always reachable by an element of the corresponding orbit.
This happens, for example, if there is no state on layer $p$ with remainder $r$.


Consider a state $\layer p s$ and let its remainder be $r = \rem(\layer p s)$.
A transition from it by a letter $k$ advances to a state $\layer{p-1}{s'}$ if
$$r b^p + k b^{p-1} + s' \cdot \gcd(m, b^{p-1}) \equiv 0 \mod m.$$
We reduce this equation to obtain
$$\begin{aligned}
  s' &\equiv -(rb + k) \by{b^{p-1}}{m}  \mod \by{m}{b^{p-1}} \\
  s' &= \expect'(p-1, rb + k).
\end{aligned}
$$

Viewed as an integer, this $s'$ is a number between $0$ and $\by{m}{b^{p-1}} - 1$.
For it to index a state, we need $s' < \by{b^{p-1}}{m}$.
Notice that this amounts to saying that the necessary suffix
$s' \cdot \gcd(m, b^{p-1})$ has at most $p-1$ digits in base $b$.

In case the procedure above fails,
there is no $p$ digit suffix starting with $k$ that takes the machine to a remainder of zero.
What the machine does, then, is to try longer suffixes representing the same remainder.
This is a recursive procedure where it just copies the transitions from $\layer{p^+}{s^+}$
where $s^+ = \expect'(p^+, \rem(\layer p s))$
until looking at layer $N$.
There, the meaning of a state switches from "expected" to "remainder"
and the information passed changes to acting as state
$$\ca{\layer{N}{\Big[\rem(\layer{p}{s}) \mod \by{m}{b^N}}\Big]}.$$
If even this layer cannot advance, it simply continues as a divisibility machine modulo $\by{m}{b^N}$.

We can put the discussion above together into a definition for the transition.
$$
\begin{align*}
  \delta_4(\layer{p}{s}) &= \begin{cases}
  \cb{\layer{p^-}{s'}}, & s' < \by{b^{p-1}}{m}, \\
  \ca{\layer{N}{\left(r' \bmod \by{m}{b^N}\right)}}, & p = N, \\
  \delta(\ca{\layer N (r \bmod \by{m}{b^N})}, k), & p = N - 1, \\
  \delta(\cb{\layer{p^+}{\expect'(p^+, r)}}, k), & \text{otherwise},
  \end{cases}
  \\
      &\begin{array}{rrcl}
        \text{ where }  & & & \\
          & r  &=& \rem(\layer p s) \\
          & r' &=&  rb + k \pmod m \\
          & s' &=& \expect'(p^-, r')
      \end{array}
\end{align*}
$$

That is the DFA recognizing divisibility while checking for $N$ digits.
According to Alexeev's formula, the minimal automaton is the choice of $N$ requiring the least states
among all such DFA.
As previously commented, this subsumes constructions I, II, and III
by choosing the right amount of layers.


### Example: Divisibility by 75 in Decimal

The minimal automaton for base $b = 10 = 2\cdot5$ division by $75= 3\cdot5^2$ has $N = 2$,
for a total of 3 layers.
We calculate
$$ \begin{array}{lcr}
\by{b^0}{m} = 1 & \by{b^1}{m} = \frac{10}{5} = 2 & \by{m}{b^2} = \frac{75}{25} = 3.
\end{array}
$$

This results in the machine below,
where we use green dashed arrows to represent the recursive lookup.
Notice that it guarantees a suffix divisible by $25$ while keeping up with divisibility by $3$.

```tikz {tikzlibrary="automata" usepackage="amssymb"}
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]},
   every edge quotes/.style = {font = {\scriptsize\tt}},
   every state/.style={node font = \footnotesize, minimum size = 1cm},
  ]
  \node[state, sdiv] (2-0) at (0, 0) {$\layer 2 0$};
  \node[state, sdiv] (2-1) at (-2, 2) {$\layer 2 1$};
  \node[state, sdiv] (2-2) at (-2, -2) {$\layer 2 2$};

  \node[state, spow] (1-0) at (2, 2) {$\layer 1 0$};
  \node[state, spow] (1-1) at (2, -2) {$\layer 1 1$};

  \node[state, spow, initial, accepting, initial where=above, initial text = ] (0-0) at (5, 0) {$\layer 0 0$};

  \path[->]
    (2-0) edge["{0}"] (1-0)
    (2-0) edge["{7}"'] (1-1)
    (2-0) edge["{[369]}", loop right] ()
    (2-0) edge["{[14]}"', sloped] (2-1)
    (2-0) edge["{[258]}", sloped] (2-2)
  ;

  \path[->]
    (2-1) edge["{5}"] (1-0)
    (2-1) edge["{[0369]}", loop left] ()
    (2-1) edge["{[147]}"', sloped, bend right] (2-2)
    (2-1) edge["{[28]}"', sloped, bend left] (2-0)
  ;

  \path[->]
    (2-2) edge["{2}"'] (1-1)
    (2-2) edge["{[0369]}", loop left] ()
    (2-2) edge["{[147]}", sloped, bend right] (2-0)
    (2-2) edge["{[258]}"', sloped] (2-1)
  ;

  \path[->]
    (1-0) edge["{0}"] (0-0)
    (1-0) edge["{7}"] (1-1)
    (1-0) edge[green, dashed, bend right] (2-0)
   % (1-0) edge["{[369]}", sloped, bend right] (2-0)
   % (1-0) edge["{[14]}",  sloped, bend right] (2-1)
   % (1-0) edge["{[258]}" near start, sloped, bend left] (2-2)
  ;

  \path[->]
    (1-1) edge["{5}"'] (0-0)
    % (1-1) edge["{[0369]}", out=-90, in=180] (2-1)
    % (1-1) edge["{[147]}", bend left] (2-2)
    % (1-1) edge["{[28]}"] (2-0)
  ;
  \draw[->, green, dashed] (1-1) .. controls (-5, -4) and (-6,  -1) ..   (2-1);

  \path[->]
    (0-0) edge["{0}", loop right] ()
    (0-0) edge[green, dashed, bend right] (1-0)

    %(0-0) edge["{7}"] (1-1)
    %(0-0) edge["{[14]}"] (2-1)
    %(0-0) edge["{[258]}"] (2-2)
    %(0-0) edge["{[369]}"] (2-0)
  ;

}
```

E quindi uscimmo a riveder le stelle
====================================

Very well, my dear reader, this post got much longer than expected.
Nevertheless, it shows that one finds interesting patterns
even in the simplest of maths: number and their digits.

Now I bid you farewell and hope you have enjoyed
this trip over automata and numbers.

See ya next time!
