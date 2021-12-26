---
title: Shower Thoughts about Averages
keywords: [math, statistics]
date: 2021-12-27
---

\def\E#1{\mathbb{E}[#1]}
\def\inner#1{\left\langle#1\rangle\right}
\def\norm#1{\left\lVert#1\right\rVert}

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
as a function that for each possible outcome assigns a real number[^RV-def],
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

- Metric
  - orthogonal basis
  - Constants are independent of prob
  - Weights depend on $p_i$
    - $\norm{\delta_i}^2 = p_i$ (should justify this squared)


$$
\left\langle \delta_i , \delta_j \right\rangle = \begin{cases}
  p_i,& i = j \\
  0,& \text{otherwise}
\end{cases}
$$


## What about the mean?

$$
\begin{array}{rl}
    \min\limits_{x} & \norm{X - c}_2 \\
    \textrm{s.t.}  & c \text{ is a constant function}
\end{array}
$$

Least squares


### Another old friend: standard deviation

## But Iago, I dislike least squares!

Talk about median and mode

## Afterthoughts
