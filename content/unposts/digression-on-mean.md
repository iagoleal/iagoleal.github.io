---
title: Shower Thoughts about Averages
keywords: [math, statistics]
date: 2021-12-27
---

\def\inlSum{\textstyle\sum}

\def\E#1{\mathbb{E}[#1]}
\def\inner#1{\left\langle#1\right\rangle}
\def\norm#1{\left\lVert#1\right\rVert}
\def\dist(#1,#2){\mathrm{dist}({#1},{#2})}

\def\R{\mathbb{R}}
\def\Id{\mathbb{I}}

I have a confession to make: I never really quite grasped
how to think about expected values.
Of course, I use it a lot (really really a lot) and I know pretty well how useful it is,
but I never had a good intuition for it.
In fact, I always thought that its importance was purely operational:
the average is easy to calculate, has plenty of applications
and there is a lot of powerful theorems related to it.

::: Theorem
The closest constant to a random variable is its expected value.
:::


::: Theorem
The mean of a random variable $X$ is the constant that is closest to it
on the euclidean distance.
:::


## The Geometry of Random Variables

Let's begin by laying the groundwork.
Suppose that we're considering an event with a finite amount (let's say $N$)
of possible outcomes $\Omega = \{\omega_1,\ldots,\omega_N\}$,
each occurring with a probability $p_i$.
You can think of it as throwing a dice, pulling a slot machine's lever
or any other situation where uncertain stuff may happen.

In this framework, we can think of a _random variable_
as a function that for each possible outcome assigns a real number[^rv-def],
$X \colon \Omega \to \R$.
As an example, a possible random variable is _how much_ cash
each outcome of a slot machine gives you.
Now comes the **most important part**:
these are just real-valued functions,
thus summing them or scaling by a number
always amounts to another random variable:

$$
  \begin{aligned}
    (X + Y)(\omega) &= X(\omega) + Y(\omega) \\
    (kX)(\omega)    &= k \cdot X(\omega)
  \end{aligned}
$$

This means that our random variables form a _Vector Space_!
For us, the beauty of this is that we can use the tools of
good ol' Euclidean Geometry to understand how they work.
And I don't know for you, but I always feel more at home
when working in the land of Linear Algebra.


[^rv-def]: If you know some measure theory,
you know that this definition is oversimplified.
It is missing the $\sigma$-algebra, measurability and other complicated stuff.
But, well, the focus of this post is on intuition
thus I'm ok with sacrificing generality and some rigour for the sake of simplicity.
Nevertheless, we can always think that there is an implicit discrete
$\sigma$-algebra everywhere.

A nice consequence of having only a finite amount of possible outcomes
is that the space of random variables is actually finite-dimensional!
The _indicator functions_ of the outcomes form a basis to this space:

$$
\Id_i(\omega_j) = \begin{cases}
  1,& i = j \\
  0,& \text{otherwise}.
\end{cases}
$$

The coefficients of a random variable in this basis are also pretty straightforward.
They are just the values returned by it for each outcome.
That is, if $X \colon \Omega \to \R$ returns the value $x_i$ when we sample an outcome $w_i$,
then it is decomposable as

$$
X = \sum_{j = 1}^N x_j \Id_j.
$$

Because we are interested in a geometrical definition for the mean,
let's consider a special example:
random variables that always return the same value
no matter what outcome we sample.
These are called _constant random variables_,
and as you can image from the introduction,
are of uttermost importance for what we are doing in this post.
So, if $X \colon \Omega \to \R$ always returns $c$,
it can be decomposed as

$$
C = \sum_{j = 1}^N c \Id_j
  = \underbrace{c}_{\text{constant}} \cdot \underbrace{\left(\sum_{j=1}^N \Id_j\right)}_{\text{direction}}.
$$

From this we see that all constants lie on the same line (one-dimensional space),
given by the diagonal of all indicators.
Remember that we want a method to find the closest constant to a random variable.
Intuitively, we can think of this procedure as projecting
the random variable into the line of constants.
So, let's proceed by considering how to project vectors into subspaces.


### Where is the probability?

Possibly you noticed that until now, we never used the probabilities.
The random variables represent values attached to some non-determinism outcome
but we haven't used any notion of which outcomes are more probable
nor how they relate to each other.

Another thing you may have noticed is that the previous section
was all function algebra, without any real geometry happening.
There were no angles, distances nor anything like that.
Very well, this is the time to fix things up
by killing two birds with one stone.
Our solution will use the _probabilities_ of the outcomes
to define an _inner product_ on the space of random variables.

Now it's time for modeling! But how can we embed the probabilistic structure
into a inner product?
To have an inner product that somehow reflects the probabilities,
we will ask that it satisfies some coherence conditions.

We want the inner product of two random variables $X$ and $Y$ to only depend
on the probability $p_i$ if both of them are non-zero for the outcome $\omega_i$.
This restriction represents that the information for one possible outcome is only important for it.
Some high-level consequences of this restriction are:

- Random variables with disjoint supports,
(That is, every time one of them is zero, the other is non-zero)
are **orthogonal**.

- The norm of a random variable **only depends
  on the outcomes for whom it is non-zero**.


Now more concretely:
How does this shape the inner product?
It is completely determined by how it acts on a basis,
so let's check these properties for the _indicators_.
First, the norm of $\Id_j$ can only on the probability $p_j$.
Also, since they are supported on different outcomes,
this definition forces the $\Id_J$ to form an orthogonal basis!

$$
\inner{\Id_i, \Id_j} = \begin{cases}
  f(p_i), & i = j \\
  0,& \text{otherwise}.
\end{cases}
$$

Where $f \colon \R \to \R$ is a yet undetermined function.
To totally define this inner product, we must enforce another property:
we require deterministic objects to be unaware of probabilistic structure.
Since the only way for a random variable to be deterministic is being constant,
this translates to: _if $C$ is a constant random variable,
then its norm $\norm{C}_2$ doesn't depend on any $p_i$._
Moreover, for the sake of consistency, let's also require
 norm of a constant random variable to be precisely the value that it always returns.
In math symbols: If for all $\omega$, $C(\omega) = c$ then $\norm{C}_2 = c$.
Now let's use this new property to determine the inner product values.

$$
\begin{aligned}
\norm{C}_2 &= \sqrt{\inner{C,C}} = \sqrt{\inner{c \inlSum_i \Id_i, c \inlSum_j \Id_j}} = c \sqrt{\inlSum_{i,j}\inner{\Id_i, \Id_j}} \\
           &= c \sqrt{\inlSum_j f(p_j)}
\end{aligned}
$$

Because the $p_j$ are the coefficients of a probability distribution,
they must sum to one.
Thus, a good choice for $f$ that satisfies our desired properties is
to set equal to the identity function.
This way we get

$$
\norm{C}_2 = c \sqrt{\inlSum_j f(p_j)} = c \sqrt{\inlSum_j p_j} = c \cdot 1 = c
$$

Now we finally have an inner product that coherently represents
the probabilistic structure of our random variables!
On the indicators basis, it is defined as

$$
\inner{\Id_i, \Id_j} = \begin{cases}
  p_i, & i = j \\
  0,& \text{otherwise}.
\end{cases}
$$

While for general $X$ and $Y$,
we can use linearity to discover that this amounts to

$$
\inner{X, Y} = \sum_{j} p_j x_j y_j.
$$


## What about the mean?

Recall that what an inner product essentially
defines are notions of distances and angles between vectors.
Particularly, the distance is given by $\dist(X, Y) = \sqrt{\norm{X-Y}_2}$.
Through it, we finally have the necessary tools
to rigorously describe the **average** using a variational principle,
as we talked about on the introduction.


::: Definition
Let $X$ be a random variable. We call its _average_ $\E{X}$ the value
of the constant random variable that is closest to it.

$$
\begin{array}{rl}
    \E{X} = \argmin\limits_{c} & \dist(X, C) \\
    \textrm{s.t.}  & C(\omega) = c, \forall \omega
\end{array}
$$
:::

Let's distill this definition a bit.
For us "closest" means "vector that minimizing the distance".
Remembering that all constant random variables lie on the same line,
we get from Geometry/Linear Algebra that $\E{X}$ is
the result of the _orthogonal projection_ of $X$ on $\sum_j \Id_j$.
(which is an unitary vector)
Thus, if we also remember that squaring a non-negative function does not affect
the point that minimizes it (because it is convex and monotonous),
our previous definition can be more concretely written
as this _least squares problem_:

$$
  \begin{array}{rl}
  \E{X} = \argmin\limits_{c} & \frac{1}{2}\norm{X-C}_2^2 \\
          \textrm{s.t.}  & C = c \sum_j \Id_j.
  \end{array}
$$

Least Squares notoriously have a closed form solution
but let's derive it on this particular case.
First we turn it into an unconstrained problem by substituting $C$
in the objective function; which makes it look like $\frac{1}{2}\sum_j p_i (x_i - c)^2$.
Since this is convex, all we need to find the minimizer
is applying the good old method of differentiating **with respect to c** and equating to zero.

$$
\begin{aligned}
  0 &= \frac{d}{dc} \left(\frac{1}{2}\inlSum_i p_i (x_i - c)^2\right) \\
    &= \frac{1}{2}\inlSum_i p_i \cdot 2 (x_i - c) \\
    &= \inlSum_i p_i x_i  - \inlSum_i p_i c \\
    &= \left(\inlSum_i p_i x_i\right) - c
\end{aligned}
$$

By moving the scalar $c$ to the equation's left-hand side,
we at last found a formula for the expected value.
And, of course, it meets our expectation![^pun-E]

$$
\boxed{\E{X} = \sum_{i=1}^N p_i x_i}
$$

[^pun-E]: ba dum tss.


### Let's gather more old friends

- σ
- Correlation

## But Iago, I dislike least squares!

Talk about median and mode

## Afterthoughts
