---
title: FFT as a Hylomorphism
keywords: [fft, haskell, recursion-schemes, functional-programming]
date: 2020-09-20
description:
  All divide-and-conquer algorithms can be separated
  into a construction part and a merge part.
  This post illustrates this in Haskell for the FFT.
---

\def\C{\mathbb{C}}

After writing the [previous post](/posts/recursion-schemes/),
I kept wondering what would be a good,
more "real life" example of recursion schemes in action.
Some days ago,
I noticed that one of my favorite algorithms can be written as a hylomorphism.

The [Fast Fourier Transform](https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm)
(FFT for short) uses a divide-and-conquer strategy
to allow us to calculate the discrete Fourier transform in $O(N \log N)$
(where $N$ is the length of the input).
It has a myriad of applications,
specially in signal processing and data analysis.

But today I am not explaining it.
Oh no, that would require a much longer post.
Today we will only use the FFT as an example of
how to write a fully fetched algorithm using neither loops nor explicit recursion.
Since it is more complicated than the previous post's examples,
this time we will need a real programming language,
thus everything will be in Haskell.
I did this implementation as an exercise in recursion schemes,
so I'm aiming more towards clarity than efficiency in here.
Therefore, we will represent vectors using Haskell's standard type for linked lists.
But if you want to adapt it to some other type such as `Vector` or `Array`,
I think the conversion should be pretty straightforward.

The Discrete Fourier Transform
==============================

One way to look at the discrete Fourier transform
is as a exchange between orthonormal bases on $\C^N$.
Given a vector $v \in \C^n$  and a basis $e_t$,
we can write $v$ as[^coefs-zero]
$$v = \sum_{t=0}^{N-1} x_t e_t,$$
where $(x_0,\ldots, x_{N-1})$ are the coefficients of $v$ on the basis $e_t$.
If, for example, $v$ represents a discrete temporal series,
its coefficients would represent the amplitude at each sampled instant.
In a lot of cases, however,
we are also interested in the amplitudes for the frequencies of this vector.
To calculate those, we exchange to another basis $f_k$ (related to $e_t$)
called the _Fourier basis_ and write $v$ as
$$ v = \sum_{k=0}^{N-1} y_k f_k$$
where the new coefficients $y_k$ are defined as
$$ y_k = \sum_{t=0}^{N-1} x_t \cdot e^{-{2\pi i \over N}t k}.$$

[^coefs-zero]: In this context, life gets much easier if our vectors are zero-indexed.

We have our first formula, time to implement it in Haskell!
Let's begin by importing the libraries we will need.

> import Data.Complex
> import Data.List (foldr, unfoldr)

The functions `foldr` and `unfoldr` are, respectively,
Haskell's built-in catamorphism and anamorphism for lists.

The formula for the $y$'s receives as list of coefficients
and returns another list of coefficients

> dft :: [Complex Double] -> [Complex Double]

The view we will take in here is that the input represents a parameter
and we will build a new list in terms of it.
This lends us to the following anamorphism:

> dft xs = unfoldr coalg 0
>  where
>   dim = fromIntegral $ length xs
>   chi k t = cis (-2 * pi * k * t / dim)
>   coalg k = if k < dim
>              then let cfs = fmap (chi k) [0 .. dim - 1]
>                       yk  = sum (zipWith (*) cfs xs)
>                   in  Just (yk, k + 1)
>              else Nothing

If you've never seem the function `cis` before,
it is Haskell's imaginary exponential $a \mapsto e^{i a}$.
The function `dft` builds the `y`'s one step at a time.
There are $N$ coefficients $y_k$ and each of them requires $O(N)$ calculations,
thus the complexity is $O(N^2)$.
Although this is not monstrous, it can be a bottleneck for real-time computations.
Fortunately we can do better than this.

The Fast Fourier Transform
==========================

The function `dft` implements the Fourier transform exactly as it is defined.
However, if you take a look at the coefficients used,
you will see that there's a certain pattern to them.
A way to exploit this pattern is encoded in the Danielson-Lanczos Lemma.

::: Lemma
In even dimensions,
the Fourier transform satisfies
$$y_k = y^{(e)}_k + e^{-{2\pi i \over N} k} \cdot y^{(o)}_k,$$
where $y^{(e)}$ is the Fourier transform of its even-indexed coefficients
and $y^{(o)}$ is the Fourier transform of its odd-indexed coefficients.
:::

::: Proof
$$ \begin{aligned}
y_k &= \sum_{t=0}^{N-1} x_t \cdot e^{-{2\pi i \over N} t k} \\
    &= \sum_{t=0}^{N/2-1} x_{2t} \cdot e^{-{2\pi i \over (N/2)} t k}
     + e^{-{2\pi i \over N} k} \sum_{t=0}^{N/2-1} x_{2t + 1} \cdot e^{-{2\pi i \over (N/2)} t k} \\
    &= y^{(e)}_k + e^{-{2\pi i \over N} k} \cdot y^{(o)}_k.
\end{aligned}$$
:::

Well, that's a lot of symbols...
The important part for us is that we can break the DFT into two smaller problems
and then merge them back together with a $O(N)$ procedure.
A divide-and-conquer algorithm!

Let's model this as a hylomorphism.
For simplicity (and to avoid the boilerplate of fixed points)
we will use the direct definition of a hylo.

> hylo f g = f . fmap (hylo f g) . g

The FFT will take an input list,
properly divide it into even and odds indices and then
conquer the solution by merging the subproblems into the output list.

> fft :: [Complex Double] -> [Complex Double]
> fft = hylo conquer divide

Alright, time to implement the actual algorithm.
Let's begin with the `divide` step.
Our algorithm must take a list of complex numbers and
rewrite it as a binary tree whose leafs are sublists of odd dimension
and nodes represent splitting an even-dimensional list.
The data structure that represents this call tree is

> data CallTree a x = Leaf [a] | Branch x x
>
> instance Functor (CallTree a) where
>   fmap _ (Leaf xs     ) = Leaf xs
>   fmap f (Branch xs ys) = Branch (f xs) (f ys)

The bunk of the `divide` method consists of splitting a list into even and odd components.
We can do this in $O(n)$ steps using a fold from a list to a pair of lists.

> split :: [a] -> ([a], [a])
> split = foldr f ([], []) where f a (v, w) = (a : w, v)

Finally, `divide` represents one step of constructing the call tree.
If the list's length is even,
we split it into smaller lists and store them in a `Branch`
to later apply the Danielson-Lanczos Lemma.
In case it is odd,
there are no optimizations we can do,
thus we just store the list's DFT in a `Leaf`.

> divide v = if even (length v)
>             then uncurry Branch (split v)
>             else Leaf (dft v)

This constructs the call tree.
Now it's time to deal with the `conquer` step.
First we notice that thanks to the periodicity of the complex exponential
(and the famous Euler formula),
$$ e^{{-2\pi i \over N } (k + {N \over 2})} = -e^{{-2\pi i \over N } k }.$$
From this, we can reconstruct the FFT from the smaller subproblems as
$$\begin{aligned}
y_k               &= y^{(e)}_k + e^{-{2\pi i \over N} k} \cdot y^{(o)}_k, \\
y_{k+{N \over 2}} &= y^{(e)}_k - e^{-{2\pi i \over N} k} \cdot y^{(o)}_k.
\end{aligned}$$

In Haskell,
we can apply both the reconstruction formulas and then concatenate the results.

> conquer (Leaf v)       = v
> conquer (Branch ye yo) = zipWith3 f [0..] ye yo
>                          ++ zipWith3 g [0..] ye yo
>  where
>   dim = fromIntegral (2 * length ye)
>   f k e o = e + cis (-2 * pi * k / dim) * o
>   g k e o = e - cis (-2 * pi * k / dim) * o

Conclusion
==========

The main advantage of writing code like this is that it is extremely modularized.
We have many small almost-independent snippets of code
that are combined through the magic of a `hylo` into an extremely powerful algorithm.
As bonus,
if you want to test this code by yourself,
you can also invert the fft to recover the original coefficients in $O(N \log N)$
through

> ifft x = fmap ((/dim) . conjugate) . fft . fmap conjugate $ x
>       where dim = fromIntegral (length x)

Another interesting fact I've noticed is that,
when working with `Double`s,
the `fft` has much less rounding errors than the `dft`.
This probably occurs because we make less floating-point operations
and not because of the hylomorphism.
But I thought it worth noticing anyway.

Well, it's all for today. Good morphisms to everyone!
