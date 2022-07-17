---
title: The Lazy Way to Solve Differential Equations
keywords: [haskell, functional-programming]
date: 2022-07-17
---

Back at college I took some classes on solving differential equation.
My favorite were certainly those from the Physics department,
because they taught us all kinds of formulas, methods and series
to actually compute the solutions instead obsessing over regularity and convergence issues[^mathematicians].
A lot of the techniques, specially those for equations with analytic coefficients,
sometimes felt a bit mechanical.
In fact, it almost always goes like this:
Get your equation, Taylor expand everything, collect terms by indices
and then solve the recurrence relations to find Taylor series for the solution.

[^mathematicians]: I hope my fellow mathematicians don't excommunicate me for such a comment. D:

Well, some days ago [João Paixão](https://www.joaopaixao.com/)
sent me a link to a paper from @calcStreams1998 called "Calculus in Coinductive Form".
In it the authors show that if we look at Taylor series as streams of real numbers,
then solving these differential equations become _as easy as writing them_!

Of course, I got really excited with the idea and had to turn it into code.
After all, that is the epitome of declarative programming!
The paper is really readable, and I urge you to take a look at it.
There are many mathematical details that I will not touch
and even a whole discussion on how these same techniques apply to Laplace transforms.
As an appetizer of what we are going to do,
consider the initial value problem

$$
  y'' = -x^2 y + 2 y' + 4 \\
  y(0) = 0,\; y'(0) = 1.
$$

By the end of this post we gonna be able to solve this differential equation
simply by writing the equivalent Haskell definition:

    y = 0 :> 1 :> (-x^2) * y + 2 * diff y + 4


Calculus with Infinite Lists
----------------------------

> {-# LANGUAGE DeriveFunctor, DeriveFoldable, NoMonomorphismRestriction #-}
> import Data.Foldable (toList)

First, a disclaimer:
I will not deal with convergence issues in this post.
For us everything will be beautiful and perfect and analytic.
Of course, since ignoring these things makes a chill run down my spine,
let's just agree that every time I say "smooth" I actually mean
"analytic in a big enough neighbourhood around whatever points we are considering".
Nice, now it's calculus time.

Our basic tool is the all too famous Fundamental Theorem of Calculus.
Consider a smooth function $f$;
An application of the FTC tells us that any smooth $f$
is uniquely determined by its value at a point $a$ and its derivative:

$$ f(a + x) = f(a) + \int_a^x f'(t) dt.$$

Ignoring all meaning behind the integral, derivative, evaluation etc.
we can view this as a recipe: smooth functions are equivalent
to pairs containing a number and another smooth function.
I don't know about you but this sounds a lot like a recursive datatype to me!

> data Stream a = a :> Stream a
>   deriving (Functor, Foldable)
>
> infixr 2 :>

The `Stream a` datatype represents a list with an infinite amount of elements.
To see this all you have to do is unfold the definition.
In our context this means that a smooth function may be represent by
the infinite list of its derivatives at a point:

    f = f a :> f'
      = f a :> f' a :> f''
      = f a :> f' a :> f'' a :> f'''
      = ...

Since the constructor `(:>)` is our way to represent the TFC,
the above amounts to saying that we can represent a (sufficiently regular) function
by its Taylor series.
That is, by applying the TFC recursively to the derivatives of $f$, we get

$$ f(a + x) = \sum_{k=0}^\infty f^{(k)}(a) \frac{x^k}{k!}. $$

As expected, the info that actually depends on $f$ are only its derivatives at $a$.
In math, we represent this as a power series but, by linear independence,
this is completely equivalent to the stream of its coefficients in the basis $\{x^k/k!\}$.

With this we're done.
Well... We're not actually done,
there is still a lot of cool stuff I want to show you.
Nevertheless, the definition above is already enough
to replicate the power series method of solving ODEs.
Don't believe me? Let's solve some familiar equations then.

The exponential function is the unique solution to
$y' = y$, $y(0) = 1$,
which becomes the following recursion in Haskell:

> ex = 1 :> ex

Let's check the starting coefficients of `ex` on ghci
to confirm that they match the derivative of $\exp$ at zero:
are exactly the derivatives of the $\exp$:

```ghci
ghci> take 10 (toList ex)
[1,1,1,1,1,1,1,1,1,1]
```

The way to define sine and cosine as the solutions to a system of ODEs
in Haskell becomes a mutually recursive definition:

> sine   = 0 :> cosine
> cosine = 1 :> fmap negate sine

As expected, these streams follow the same alternating
pattern of $0, 1, 0, -1,\ldots$ as the Taylor coefficients.

```ghci
ghci> take 10 (toList sine)
[0,1,0,-1,0,1,0,-1,0,1]
ghci> take 10 (toList cosine)
[1,0,-1,0,1,0,-1,0,1,0]
```

Even though we know how to calculate the Taylor coefficients
they're only means to an end.
The main reason one wants to solve differential equations
is to calculate _functions_, not series.
Let's then hack a poor man's function approximation
for these Taylor expansions.
For simplicity, I will use a fixed amount of 100 coefficients.
This works for this demonstration but in any real program,
it is better to call upon some analysis to find out the right amount
of terms for your desired error estimate.
Let's then create a higher order function that converts streams of real numbers
into real valued functions.

> -- | Turn a Stream f into a functional approximation
> --   of its Taylor series around a point a.
> -- That is, eval a f ≈ f(a + x)
> eval :: Fractional a => a -> Stream a -> a -> a
> eval a f x = foldr1 (\ fa f' -> fa + (x - a) * f') (take 100 taylor)
>  where
>   taylor      = zipWith (/) (toList f) factorials
>   factorials  = let fats = 1 : zipWith (*) fats [1..]
>                 in fmap fromIntegral fats

With our evaluator in hand, it's time to test
our previous streams into some well-known values:

```ghci
ghci> eval 0 ex 0
1.0
ghci> eval 0 ex 1
2.718281828459045
ghci> fmap (eval 0 sine)   [0, pi/2, pi, 2*pi]
[0.0,1.0,0.0,0.0]
ghci> fmap (eval 0 cosine) [0, pi/2, pi, 2*pi]
[1.0,0.0,-1.0,1.0]
```

Quite nice, huh?
Just a few lines of code and we already have the power to solve and approximate
some classical differential equations!
All thanks to Haskell's laziness and the TFC.
Our solver is done, but the code still lacks a cleaner interface
to manipulate streams and represent differential equations.
Let's define some functions to mitigate that.

From the previous discussion,
we can get the derivative of a stream simply by
dropping the first term.

> -- | Taylor series representation of the derivative.
> diff :: Stream a -> Stream a
> diff (_ :> f') = f'

It is possible to embed any constant as a stream with derivative zero.
Also, let's define a stream `x` representing the identity function[^x-name]
in order to make our equations look a bit nicer.

> -- | Taylor series for the constant zero.
> zero :: Num a => Stream a
> zero = 0 :> zero
>
> -- | Taylor series for the identity function `f x = x`.
> x :: Num a => Stream a
> x = 0 :> 1 :> zero

[^x-name]: This is a terrible name to use in an actual top-level definition.

Finally, our fellow mathematicians and physicists that perhaps may
use this code will certainly want to do arithmetical manipulations on the series.
We can achieve that with the traditional `Num`, `Fractional` and `Floating` instances.
As usual with these Calculus posts,
these instances correspond to the well-known formulas for derivatives.
Let's start with the arithmetic classes.

> instance Num a => Num (Stream a) where
>  -- Good ol' linearity
>  (+)  (fa :> f')  (ga :> g') = fa + ga :> f' + g'
>  (-)  (fa :> f')  (ga :> g') = fa - ga :> f' - g'
>  negate = fmap negate
>  -- Leibniz rule applied to streams
>  (*) f@(fa :> f') g@(ga :> g') = fa * ga :> f' * g + f * g'
>  fromInteger n = fromInteger n :> zero
>  abs    = error "Absolute value is not a smooth function"
>  signum = error "No well-defined sign for a series"
>
> instance Fractional a => Fractional (Stream a) where
>  -- The division rule from Calculus. We assume g(0) ≠ 0
>  (/) f@(fa :> f') g@(ga :> g') = fa / ga :> (f' * g - f * g') / g^2
>  fromRational n = fromRational n :> zero


For the `Floating` instance,
we will use the chain rule and the fact that we know the derivatives
for all methods in the class.
I recommend taking a look at the implementation
we did in a [previous post for Dual numbers](/posts/calculus-symbolic-ad).
They are strikingly similar, which is no coincidence of course.
The main idea is that applying an analytic $g$ to the stream of $f$
if the same as calculating the derivates for $g \circ f$.
Thus, all our `Floating` methods will look like this:

    g f = g (f a) :> g' f * f'

This is Haskell, so we can turn this idea into a higher order function
taking both `g` and its derivative:

> analytic g g' f@(fa :> f') = g fa :> g' f * f'

> instance Floating a => Floating (Stream a) where
>  pi = pi :> zero
>  exp   = analytic exp   exp
>  log   = analytic log   recip
>  sin   = analytic sin   cos
>  cos   = analytic cos   (negate . sin)
>  asin  = analytic asin  (\x -> 1 / sqrt (1 - x^2))
>  acos  = analytic acos  (\x -> -1 / sqrt (1 - x^2))
>  atan  = analytic atan  (\x -> 1 / (1 + x^2))
>  sinh  = analytic sinh  cosh
>  cosh  = analytic cosh  sinh
>  asinh = analytic asinh (\x -> 1 / sqrt (x^2 + 1))
>  acosh = analytic acosh (\x -> 1 / sqrt (x^2 - 1))
>  atanh = analytic atanh (\x -> 1 / (1 - x^2))

With all those instances,
we can give power series the same first-class numeric treatment
that they receive in mathematics.
For example, do you want to approximate some complicated integral?
Just use the Stream `x` that we previously defined:

```ghci
ghci> erf = 0 :> exp (-x^2)
ghci> take 10 (toList erf)
[0.0,1.0,-0.0,-2.0,0.0,12.0,0.0,-120.0,0.0,1680.0]
```

Also, we've only dealt with linear equations until now
but as long as everything is analytic,
these methods readily extend to non-linear equations.

```ghci
ghci> y = 0 :> 1 :> x^2 * cos (diff y) - x * sin y
ghci> take 10 (toList y)
[0.0,1.0,0.0,0.0,-0.9193953882637205,0.0,4.0,20.069867797120825,-6
.0,-265.9036412154172]
```

Finally, we should discuss a couple caveats of this method.
Solving an ODE through a Taylor series can be slow...
That is why, in practice, this would only be used for the most well-behaved equations.
There is also the issue of convergence that we decided to ignore during this post.
Not all `Floating a => a -> a` functions are analytic everywhere
and when this hypothesis doesn't hold,
the method will just silently fail and return garbage
such as `infinity` or `NaN` for the coefficients.
Nevertheless, this "automatic" solution is pretty much equivalent to what one
would do to solve this kind of equation by hand,
including these same issues.
In fact, I would even risk saying that the Haskell internals
are much more optimized than one could hope to be when solving by hand.


This is indeed an automatic method
----------------------------------

To sum everything up, I want to note a cool fact
that I've only realized after writing the entire post:
there is a direct relationship between this method of solving ODEs
and forward-mode automatic differentiation!

When working with [dual numbers](/posts/calculus-symbolic-ad),
we define its numeric instances to obey $\varepsilon^2 = 0$,
implying that for any analytic function it satisfies

$$ f(a + \varepsilon) = f(a) + f'(a)\varepsilon. $$

This is equivalent to Taylor expanding $f$ around $a$
and truncating the series after the first order terms.
However, nothing really forces us to stop there!
Since the derivative of an analytic function is also analytic,
we can again Taylor expand it to get the second derivative and so on.
By recursively repeating this procedure we get the entire Taylor expansion.
So, if instead of using a term $\varepsilon$ that vanishes at second order,
we apply $f$ to $a + x$, we get all derivatives of $f$ at $a$.

This is the same we have been doing all along with Streams;
the only difference being that we write `a :> 1` to represent $a + x$.
So, similarly with dual numbers,
we can define a procedure to calculate _all derivatives_ of a polymorphic $f$ at the point $a$
by applying $f$ to a suitable Stream.

> -- | A Stream with all derivatives of f at a.
> diffs f a = f (a :> 1)


Acknowledgements
----------------

This post gained life thanks to the enthusiasm of João Paixão and Lucas Rufino.
João sent me the paper and we three had some fun chats about its significance,
including becoming perplexed together about how little code we actually needed to implement this.

References {#refs}
------------------

The [Numeric.AD.Mode.Tower](https://hackage.haskell.org/package/ad-4.5.2/docs/Numeric-AD-Mode-Tower.html)
module of the [ad package](https://hackage.haskell.org/package/ad).

---
nocite: |
  @calcStreams1998, @rutten2005
---
