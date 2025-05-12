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

\def\divides{\mid}
\def\ceil#1{\lceil #1 \rceil}
\def\Ceil#1{\left\lceil #1 \right\rceil}

\def\ca#1{\colorbox{lightgreen}{$#1$}}
\def\cb#1{\colorbox{thistle}{$#1$}}
\def\cc#1{\colorbox{lightblue}{$#1$}} % lightcoral


First Construction: Transitions as Divisibility
===============================================

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


Second Construction: Orbit-based Transitions
============================================

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

The Automata from the Orbits
----------------------------

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


Third Construction: States for the Powers
=========================================

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
{ [shorten >=1pt, node distance=2cm, on grid, auto, >={Stealth[round]}]
  \node[state]                                     (q_1) []  {1};
  \node[state]                                     (q_2) [below = of q_1]  {10};
  \node[state]                                     (q_3) [right = of q_2]  {100};
  \node[state, initial, accepting, initial where = above, initial text= ] (q_0) [right = of q_3]  {0};

  \path[->]
    (q_0) edge[loop right] node               {\texttt{0}}     ()
    (q_1) edge[]           node [right]       {\texttt{0}}     (q_2)
    (q_2) edge[]           node [above]       {\texttt{0}}     (q_3)
    (q_3) edge[]           node [above]       {\texttt{0}}     (q_0)
    (q_0) edge[bend right] node [above right] {\texttt{[1-9]}} (q_1)
    (q_1) edge[loop left]  node []            {\texttt{[1-9]}} ()
    (q_2) edge[bend left]  node []            {\texttt{[1-9]}} (q_1)
    (q_3) edge[bend right] node [right]       {\texttt{[1-9]}} (q_1)
  ;
}
```

This idea generalizes for any case where $m = b^k$.
Then $L(b, b^k) = 0^\star + \A^\star 0^k$
--- the language whose words are either all zero or end with at least $k$ zeros.
One can recognize this by simply keeping a record of the zeros,
which takes $1 + k = 1 + \log_b m$ states,
an exponential decrease compared to the previous constructions!
By using ${k, \ldots, 0}$ as state space,
the transition keeps track of how many zeros are left,
$$ \begin{aligned}
\delta(0, 0) &= 0 \\
\delta(s, 0) &= s - 1 \\
\delta(s, a) &= k
\end{aligned}
$$

Despite the exponential gain,
this process only works for dividing powers of the base --- a very specific case.
Nonetheless, it is possible to mix it with our previous constructions
in a way that extends to other divisors.
The process is kinda convoluted though, so let's do it in steps
by first generalizing to the languages $L(b, xb^k)$ and $L(a^c, a^d)$.

Recognizing $L(b, x b^k)$ with $x$ and $b$ coprime
--------------------------------------------------

Suppose that $m = x b^k$ with $\gcd(b, x) = 1$.
There is a smart way to determine divisibility by $m$ requiring only $x + k$ states.
The idea is that coprime factors can be checked separately.
First, notice that thanks to coprimality $m \divides n$
if and only if $x \divides n$ and $b^k \divides n$.
Furthermore, we know from our previous discussion
that trailing zeros never "lose" divisibility.
Thus, a characterization follows.

:::Lemma
Let $b$ and $m$ be as above.
A number written in base $b$
is divisible by $m$ if and only if it is either only zeros
or a number divisible by $x$ followed by $k$ zeros.
In other words, $L(b, m) = 0^\star + L(b, x)0^k$.
:::

Now, we can build an automaton that first checks for divisibility by $x$
and then proceeds to count trailing zeros.
Let's again use colors to represent those distinct kinds of states.
$$\S = \ca{\Z_x} \sqcup \cb{\Z_k}.$$

The starting and accepting state is $\cb{0}$.
For the transitions,
the green states act as the usual divisibility automaton automaton
except that reading a zero on $\ca{0}$ takes you to the purple states.
$$ \begin{aligned}
\delta(\ca{0}, 0) &= \cb{k-1} \\
\delta(\ca{s}, k) &= \ca{bs + k \pmod x}
\end{aligned}
$$

Meanwhile, the purple state $\cb{s}$ represents a number divisible by $x$
but which still have to read at least $s$ to be divisible by $b^k$.
Think of it as augmenting $\ca{0}$ with a counter.
Reading a zero takes you one step further while reading anything else
takes you back to the green states.

$$ \begin{aligned}
\delta(\cb{0}, 0) &= \cb{0} \\
\delta(\cb{s}, 0) &= \cb{s-1} \\
\delta(\cb{s}, k) &= \delta(\ca{0}, k)
\end{aligned}
$$

:::Missing
Illustrate this machine
:::


### Example: Decimal Division by 300

:::Missing
300 base 10
:::

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

- $i > q$: Those terms are all divisible by $a^d$ because $ci \ge c(q+1) = cq + c > cq + r = d$.
And can, thus, be _ignored) without remorse.

- $i < q$: Notice that $a^{cq + r} \divides n \implies b^q | n$.
In other words, the $q$ last digits of $n$ on base-$b$ _must be zero_.

- $i = q$: The remaining term is $n_q a^{cq}$. The only constraint for $a^d$ to divide it
is that $a^r \divides n_q$.
This is possible because $r < c$ and $0 \le n_q < a^c$.

Put this all together, we get a relatively simple formula.

:::Lemma
Let $b = a^c$ and $m = a^d$ and write $d = cq + r$.
A word $\omega$ represents a base-$b$ number divisible by $m$
if it is either all zeros or has $q$ trailing zeros and the digit before that is divisible by $a^r$.
In other words, $L(a^c, a^d) = 0^\star + \A^\star\omega_00^q$
with $\omega_0 \equiv 0 \mod a^r$.
:::

This lets us construct an automaton with $2 + q$ state.
However, notice that using Euclidean division helped with the formulation
but this lemma has a caveat:
whenever $r = 0$, i.e. $c$ divides $d$,
the condition on the last digit vanishes and we end up with a spurious state representing it.
A smart trick solves this problem:
instead of division,
consider the ceiling $Q = \ceil{\frac d c}$ and write
$d = c Q - r$, with $0 \le r < c$.

:::Lemma
Let $b = a^c$ and $m = a^d$, $Q = \ceil{\frac d c}$, and write $d = cQ - r$.
A word $\omega$ represents a base-$b$ number divisible by $m$
if it is either all zeros or has $Q-1$ trailing zeros and the digit before that is divisible by $a^{c-r}$.
In other words, $L(a^c, a^d) = 0^\star + \A^\star\omega_00^{Q-1}$
with $\omega_0 \equiv 0 \mod a^{c-r}$.
:::

This way, the condition on $\omega_0$ becomes just another zero if the exponents align.

To construct the automaton, let $Q = \ceil{\frac d c}$, write $d = cQ - r$,
and take as states
$$\S = \{ \ca 0 \} \sqcup \cb{\Z_{\ceil{\frac d c}}}.$$
The starting and accepting state is, as usual, $\cb{0}$.
The state $\ca 0$ represents the ignored digits and
the machine leaves it as soon as it encounters something divisible by $a^r$,
$$
\delta(\ca{0}, k) = \begin{cases}
  \cb{Q-1}, & a^r \divides k  \\
  \ca{0}, & \text{otherwise}
\end{cases}
$$

After that, it turns into the already familiar zero counting machine.
$$ \begin{aligned}
\delta(\cb{0}, 0) &= \cb{0} \\
\delta(\cb{s}, 0) &= \cb{s - 1} \\
\delta(\cb{s}, a) &= \ca{1}
\end{aligned}
$$

:::Missing
Illustrate this machine
:::


### Example: Octal Division by 32

:::Missing
300 base 10
:::


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
\delta(\cb{s}, k) &= \ca{1}
\end{aligned}
$$

The green states store the remainder by $x$ with a detail:
we use $\ca{0}$ for divisibility only by $x$ and $\cb{Q-1}$
for divisibility by $x$ and $a^{c-r}$.
$$
\delta(\ca{s}, k) = \begin{cases}
  \cb{Q-1}, & a^r \divides k \text{ and } x \divides (bs + k) \\
  \ca{bs + k \pmod x} , & \text{otherwise}
\end{cases}
$$

Despite all the indirections, I hope this DFA makes sense to you!
Also, notice that when $b$ and $m$ are coprime, there are no purple states
and it becomes the original automaton $A_1$.


Are these machines minimal?
===========================

So, what is the size of the minimal automaton for the general case $L(b, m)$?
The answer turns out to be rather complicated!
@alexeev_2004 proved that the minimum necessary amount of states is
$$ \min_{N \ge 0}\left[ \frac{m}{\gcd(m, b^N)} + \sum_{i=0}^{N - 1} \frac{b^i}{\gcd(b^i, m)} \right].$$
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







---------------------------------------------------------------------------------------

<!-- For the general construction, -->
<!-- the state space is divided into $N+1$ blocks. -->
<!-- The first corresponds to the orbit $\orbit(b^N, m)$ and acts as a remainder for divisibility. -->
<!-- The other blocks have $b^i / \gcd(m, b^i)$ elements and correspond to the powers "trimmed by  -->




------------------------------------------------------------------------------------------------------------------------------------------

https://content.wolfram.com/sites/19/2010/02/Sutner.pdf

