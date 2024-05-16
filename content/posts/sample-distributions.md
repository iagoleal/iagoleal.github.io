---
title: Uniformly Generating Discrete Distributions
keywords: [math]
date: 2024-05-14
---

\def\norm#1{\left\lVert#1\right\rVert}
\def\Unif{\mathrm{Unif}}
\def\Exp{\mathrm{Exp}}

Oftentimes, I need to sample points from some finite set.
Most times, there is an obvious way to draw elements,
but even so often, it happens that I need
the _probability distribution itself_ to be random
(e.g. property testing for solvers depending on weighted averages.)
To avoid possible statistical misfortunes,
it is desirable to choose such a distribution uniformly among all candidates.

As it stands, there are some methods to do that.
Today we're going to explore my favorite one,
which only requires us to be able to independently sample uniformly from the $[0, 1]$ interval.
Besides, it is pretty fast to compute and has an elegant geometric flavour.
What more could we ask?

Furthermore, a lot of the lemmas we'll use are $L^1$ versions of well-known results for Gaussians.
Although their equivalence seems to be common knowledge among mathematicians,
I don't know about any reference that does things this way.
So, as a bonus, now I'll have where to link people to whenever I want to talk about Markov kernel-invariance.
Oh well, it seems I'm putting the cart before the horse in here.
Let's start out.

In case you've landed on this page in search for an algorithm
and don't care for the mathematical intricacies --- no matter how elegant they might be ---
here is the final theorem, which will get you covered.

:::Theorem
Let $U \sim \Unif[0, 1]^N$ be uniform on the $N$-cube and define random variables $E_i = -\log(U_i) \sim \Exp(1)$.
We can get a random vector that is uniformly distributed on all probabilities over $N$ elements as

$$ Z = \frac{1}{\sum_i E_i} E.$$
:::



Sampling Distributions
======================

$$ p(x) = \sum_{k=1}^N p_k \delta_k(x) $$.


Simplex
=======

Exponentials

:::Theorem
The Linear Transformations preserving the standard simplex are **stochastic matrices**.
:::

:::Theorem
A function $f : \R^n_+ \to \R$ is invariant by the action of stochastic matrices
if and only if it only depends on the sum of the input's components.
That is, there is a $g : \R_+ \to \R$ such that

$$ f(x) = g\left(\sum\nolimits_k x_k\right). $$
:::

:::Theorem
Consider $E$, a continuous non-negative random vector
whose distribution only depends on the $L^1$ norm, i.e.,

$$ p_E(x) = \phi(\norm{x}_1),$$

Then the random variable

$$ Z = \frac{E}{\sum_i E_i} \sim \Unif(\Delta^N).$$

That is, Z is uniformly distributed on the standard simplex.
:::

:::Theorem
If $Z$ is a random vector with continuous non-negative independent components
and its distribution only depends on the $L^1$ norm, i.e.,

$$ p_Z(x) = \phi(\norm{x}_1),$$

Then its components are i.i.d. exponentials,

$$ Z_i \sim \exp(\lambda).$$
:::

:::Proof
https://math.stackexchange.com/questions/105418/very-elementary-proof-of-maxwells-theorem/105470#105470
:::

# Quantum

$$ \frac{2}{p}\Gamma(1/p) e^{-\norm{x}_p^p} $$


# The General Case


Barthe, Franck, Olivier Guédon, Shahar Mendelson, and Assaf Naor. “A Probabilistic Approach to the Geometry of the ℓpn-Ball.” The Annals of Probability 33, no. 2 (March 1, 2005). https://doi.org/10.1214/009117904000000874.

https://metaphor.ethz.ch/x/2017/hs/401-3370-67L/sc/Joost1.pdf

http://blog.geomblog.org/2013/01/a-sampling-gem-sampling-from-ellp-balls.html
