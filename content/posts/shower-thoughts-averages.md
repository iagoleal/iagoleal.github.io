---
title: Shower Thoughts about Averages
keywords: [math, statistics]
date: 2021-12-29
---

\def\inlSum{\textstyle\sum}

\def\E#1{\mathbb{E}[#1]}
\def\Std#1{\sigma[#1]}

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
and there are a lot of powerful theorems related to it.

Nevertheless, this all changed earlier this year.
I was taking a shower[^shower-why], not even thinking about math
when I was suddenly struck by a bliss of clarity:
_the average minimizes the squared error_---I said to myself.
This seemly simple statement is present (normally as an exercise)
in practically any introductory probability book,
but I had never realized that it implies so much!
From this, we can construct the average not by a smart formula
but geometrically using a variational principle.

[^shower-why]: I don't know why, but my best ideas
always seem to come while I'm sleeping or in the shower.

Let's look at it from an information point of view:
suppose you are working with a random variable $X$
and you don't have where to store all its information.
In fact, for the sake of this example, you can only store
_a single number_ to represent it.
Which number should you choose?
You probably already know it is $\E{X}$... But why?
Well... Because it is in fact the best possible approximation.
This post will be a digression on this theme:

::: Theorem
The closest constant to a random variable is its expected value.
:::

Of course, there are some things lacking on the theorem above.
How do you measure the distance between random variables?
Moreover, constants are numbers and random variables are functions.
They have different types!
Then, what does it mean to them to be close together?
The usual probabilistic view doesn't emphasize this,
but we can interpret random variables geometrically as points in space
and, then, measuring their distance becomes as simple as taking
the length of the line segment connecting them.
Indeed, if we know the random variables,
we can even measure this length through
nothing more, nothing less than the Pythagorean Theorem!

The Geometry of Random Variables
================================

Let's begin by laying the groundwork.
Suppose that we're considering an event with a finite amount (let's say $N$)
of possible outcomes $\Omega = \{\omega_1,\ldots,\omega_N\}$,
each occurring with a probability $p_i$.
You can think of it as throwing a die, pulling a slot machine's lever
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
They are just the values it returns for each outcome.
That is, if $X \colon \Omega \to \R$ returns the value $x_i$ when we sample an outcome $w_i$,
then it is decomposable as

$$
X = \sum_{j = 1}^N x_j \Id_j.
$$

In two dimensions, we can also visualize this as a figure:

<object type="image/svg+xml"
        data="basis-decomposition.svg"
        width="100%">
</object>

<!-- ```{.tikz tikzlibrary="calc,scopes,fadings,shadows,bending,arrows.meta,shapes.misc"} -->
<!--   \def\vecolor{green!50} -->
<!--   % Help lines -->
<!--   \draw [help lines, step=1.2, path fading = east, fading angle = 45] (0,0) grid (4,4); -->
<!--   \coordinate (I1) at (4, 0); -->
<!--   \coordinate (I2) at (0, 4); -->

<!--   % Vector and components -->
<!--   \coordinate (X) at (2.9, 2.4); -->
<!--   \node at (X) [above right = 0.01cm] {$X$}; -->
<!--   \coordinate (comp1) at (X |- I1); -->
<!--   \coordinate (comp2) at (I2 |- X); -->
<!--   \draw[thin, gray] (X) -- (comp1) node[below, color=black] {$x_1$}; -->
<!--   \draw[thin, gray] (X) -- (comp2) node[left,  color=black] {$x_2$};; -->
<!--   \draw[thick, \vecolor, circular glow] (0, 0) -- (X); -->
<!--   \draw[thick, -stealth] (0, 0) -- (2, 0) node[below] {$\mathbb{I}_1$}; -->
<!--   \draw[thick, -stealth] (0, 0) -- (0, 3.5) node[left] {$\mathbb{I}_2$};; -->

<!--   \fill[\vecolor, draw=black] (X) circle (2pt); -->
<!--   \fill[\vecolor] (comp1) circle (1pt); -->
<!--   \fill[\vecolor] (comp2) circle (1pt); -->
<!-- ``` -->

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

Or, as a figure:

<object type="image/svg+xml"
        data="constants.svg"
        width="100%">
</object>

From this we see that all constants lie on the same line (one-dimensional space),
given by the diagonal of all indicators.
Remember that we want a method to find the closest constant to a random variable.
Intuitively, we can think of this procedure as projecting
the random variable into the line of constants.
So, let's proceed by considering how to project vectors into subspaces.


Where are all the probabilities?
--------------------------------

Possibly you noticed that until now, we never used the probabilities.
The random variables represent values attached to some non-deterministic outcome
however we haven't used any notion of which outcomes are more probable
nor how they relate to each other.

Another thing you may have noticed is that the previous section
was all function algebra, without any real geometry happening.
There were no angles, distances nor anything like that.
Very well, this is the time to fix things up
by killing two birds with one stone.
Our solution will use the _probabilities_ of the outcomes
to define an _inner product_ on the space of random variables.

Now it's modeling time!
How can we embed the probabilistic structure into an inner product?
To have an inner product that somehow reflects the probabilities,
we will ask that it satisfies some coherence conditions.

We want the inner product of two random variables $X$ and $Y$ to only depend
on the probability $p_i$ if both of them are non-zero for the outcome $\omega_i$.
This restriction represents that the information
for one possible outcome is only important for this outcome.
Some high-level consequences of this restriction are:

- Random variables with disjoint supports
(That is, every time one of them is zero, the other is non-zero)
are **orthogonal**.

- The norm of a random variable only depends on the outcomes for whom it is **non-zero**.


Now more concretely:
how does this shape the inner product?
It is completely determined by how it acts on a basis,
so let's check these properties for the _indicators_.
First, the norm of $\Id_j$ can only depend on the probability $p_j$.
Also, since they are supported on different outcomes,
this definition forces the $\Id_j$ to form an orthogonal basis!

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
the norm of a constant random variable to be precisely the value that it always returns.
In math symbols: If for all $\omega$, $C(\omega) = c$ then $\norm{C}_2 = c$.
Now let's use this new property to determine the inner product values.

$$
\begin{aligned}
\norm{C}_2 &= \sqrt{\inner{C,C}} \\
           &= \sqrt{\inner{c \inlSum_i \Id_i, c \inlSum_j \Id_j}} \\
           &= c \sqrt{\inlSum_{i,j}\inner{\Id_i, \Id_j}} \\
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


What about the mean?
====================

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
(which is a unitary vector)
What we really want to calculate can be summarized as a picture:

<object type="image/svg+xml"
        data="projection.svg"
        width="100%">
</object>

In order to represent this as a closed formula,
we must remember that squaring a non-negative function does not change
the point that minimizes it (because it is convex and monotone).
Our previous definition can then be more concretely written
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
we finally found a formula for the expected value.
And, of course, it meets our expectation![^pun-E]

$$
\boxed{\E{X} = \sum_{i=1}^N p_i x_i}
$$

[^pun-E]: ba dum tss.


Let's gather more old friends
-----------------------------

We just defined the average of $X$ through the closest constant to it.
A question that naturally arises is how well this represents $X$.
We can answer this by looking to the projection's _error_,
that is, the distance between $X$ and $\E{X}$.
(In what follows, we sometimes overload the notation $\E{X}$
to also mean the constant random variable with norm $\E{X}$.
What we are talking about should be clear from context)

$$
\begin{aligned}
  \text{error} &= X - \E{X}, \\
  \Std{X}      &= \norm{\text{error}}_2 = \dist(X, \E{X}) \\
               &= \sqrt{\sum_{i=1}^N p_i (x_i - \E{X})^2}.
\end{aligned}
$$

You probably already know this formula by the name of **standard deviation**!
Although in probability textbooks it appears as the concise but opaque formula
$\Std{X} = \sqrt{\E{(X - \E{X})^2}}$, here it naturally appears as
the size of the least squares' error from approximating $X$ by a constant.

<object type="image/svg+xml"
        data="variance.svg"
        width="100%">
</object>

In real world probability, one of the most widely used kinds of random variables
are certainly the _Gaussian Random Variables_.
Among their fundamental properties is that their distribution is uniquely
defined by its mean and standard deviation[^std-sigma].
This part is only my opinion and no mathematics,
but I like to think that what makes them so special
is that they can be recovered solely by knowing their approximation
by a constant and how far off this approximation is.

[^std-sigma]: Or variance if you prefer it squared.


Until now, we have only been looking for distances and orthogonal projections.
Nevertheless, the inner product also tells us about the angle between two vectors!
From Euclidean Geometry, we know that the angle $\theta$ between two vectors
satisfies

$$
\inner{A,B} = \norm{A}_2\norm{B}_2 \cos\theta.
$$

An interesting use of this angle is in calculating the **correlation**
between random variables.
To find it, we first calculate the errors of approximating $X$ and $Y$
by their means. Then, the correlation is defined exactly as
the cosine of the _angle_ between these errors.
That is, if we let $\theta$ equal the angle between
$X - \E{X}$ and $Y - \E{Y}$, then

$$
\begin{aligned}
  \mathrm{corr}[X, Y] &= \cos(\theta) \\
                      &= \frac{\inner{X - \E{X}, Y - \E{Y}}}{\Std{X}\Std{Y}}
\end{aligned}
$$

What does the correlation mean, you may ask?
Well, in usual statistical parlance,
the farther the correlation is from zero, the closer the random variables are
from being linear functions from one another, i.e. $Y = aX + b$.

Do we really need those squares?
================================

Up until this point,
we only considered distances the "classical" or Euclidean sense.
But sometimes it is not the most appropriate way to measure how far things are.
For example, if you are driving in a city, there is no way to cross diagonally between blocks.
The best way to describe the distance between two points is through
the size of the horizontal and vertical increments
one has to traverse from one point to another.
Thus let's talk a bit about these more "exotic" distances.

Coming from our example above,
we define the $L^1$-norm[^1norm] of $X$
analogously to the Euclidean norm as

$$
\norm{X}_1 = \sum_{i = 1}^N p_i |x_i|.
$$

[^1norm]: Also known by the much more stylish names of Manhattan or taxicab's norm.


This is just like the Euclidean norm we were talking about
but exchanging the squares by absolute values.
Instead of measuring the length of the line segment between points,
it measures the length we must traverse if we could only walk through a grid
parallel to the indicator functions $\Id_j$.
This distance is widely used on the literature for its robustness and sparsity-inducing properties.
Also, while the Euclidean distance is rotation invariant,
the $L^1$-norm clearly has some preference towards the outcomes' indicators.
So, our variational principle using this distance is

$$
  \begin{array}{rl}
  \min\limits_{c} & \sum_{i=1}^N p_i |x_i - c|
  \end{array}
$$

This optimization problem can be rewritten as a _linear program_
as has a (possibly non-unique) solution that is also used all around Statistics:
the **median**. This is a constant $\mu$ with the nice property
$\mathrm{P}(X \le \mu) \ge \frac{1}{2}$ and $\mathrm{P}(X \ge \mu) \ge \frac{1}{2}$.

For any measure of distance between random variables between points,
there is some associated constant that minimizes it and, in general,
is already an important object in Statistics.
As an example, try to discover which is the closest constant to $X$
by using the $\infty$-norm: $\norm{X-Y}_\infty = \max_\omega P(\omega)|X(\omega) - Y(\omega)|$.
In fact, we don't even have to use a norm!
Any meaningful notion of distance can to be used to project (non-linearly)
on the space of constants.

As a last example before we finish,
let's consider a distance that does not come from a norm.
Consider $\dist(X, Y) = \mathrm{P}(X \ne Y)$.
This is an interesting distance and widely used in signal processing
because when $Y = 0$, it measures the sparsity of $X$ as a vector.
Can you guess what constant minimizes it?
Try to prove that it is the _mode_[^mode-def] of $X$!

[^mode-def]: The _mode_ is the value returned with the greatest probability.

Acknowledgements
================

When I started thinking of these ideas, I talked about them to Hugo Nobrega and João Paixão.
This post only came to life because of how excited they got with it.
Even so that they convinced me it could be interesting to write about it.

I also want to thank Daniel Hashimoto and Ivani Ivanova for
reading the first version of this post and helping me with their "bug reports".
