---
title: Let's Program a Calculus Student II
subtitle: Automatic Differentiation
keywords: [haskell, computer-algebra, functional-programming, automatic-differentiation]
date: 2022-04-20
---

https://hackage.Haskell.org/package/ad

https://arxiv.org/pdf/1804.00746.pdf

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor  #-}
module Calculus.AutoDiff where
import Calculus.Expression
```

## On polymorphism, evaluation and reflection

Recall our evaluation function from the previous post.
Its type signature was

```haskell
eval :: Floating a => Expr a -> a -> a
```

The way we interpreted it was that if we supplied an expression `e`
and a value `c` of type `a`,
it would collapse the expression substituting all instances of the variable `X`
by `c` and return the resulting value.
But thanks to currying we may also view `eval` as taking an expression `e`
and returning a Haskell function `eval e :: a -> a`.
Thus our code is capable of transforming expressions into functions.

At this point, one may ask if if we can do the opposite.
So, can we take an ordinary Haskell function and find
the symbolic expression that it represents?
The answer, quite surprisingly to me, is: _yes, provided that it is polymorphic_.

If you take a function such as `g :: Double -> Double`
that only works for a single type[^monomorphic],
all hope is lost.
Any information regarding "the shape" of the operation
performed by the function will have already disappeared at runtime
and perhaps even been optimized away by the compiler (as it should be).
Nevertheless, polymorphic functions that work for any Floating type,
such as `f :: Floating a => a -> a`,
are flexible enough to still carry information about its syntax tree
even at runtime.
One reason for this is that we defined a `Floating` instance for `Expr a`,
allowing the function `f` to be specialized to the type `Expr a -> Expr a`.
Thus we can convert between polymorphic functions and expressions.

```haskell
uneval :: (forall a. Floating a => a -> a) -> (forall b. Floating b => Expr b)
```

[^monomorphic]: Also known by the fancy name of `monomorphic function`.
These are functions without any free type parameter. That is,
no lowercase type variable appears in the type signature.

Notice the explicit `forall`: `uneval` only accepts polymorphic arguments.
After finding the right type signature, the inverse to `eval` is then really simple to write.
The arithmetic operations on a `Expr a` just build a syntax tree,
thus we can construct an expression from a polymorphic function by substituting
its argument by the constructor `X`.

```haskell
uneval f = f X
```

Let's test it on ghci to see that it works:

```ghci
ghci> uneval (\x -> x^2 + 1)
X :*: X :+: Const 1.0
it :: Floating b => Expr b
ghci> uneval (\x -> exp (-x) * sin x)
Apply Exp (Const (-1.0) :*: X) :*: Apply Sin X
it :: Floating b => Expr b
```

The `uneval` function allows us to compute a syntax tree for
a polymorphic function during a program's runtime.
We can then manipulate this expression and turn the result back into
a function through `eval`.
Or, if we know how to do some interesting operation with functions,
we can do the opposite process and apply it to our expression!
This will be our focus on the next section.

## Automatic Differentiation

In math, derivatives are concisely defined via a limiting process:
$$ f'(x) = \lim_{\varepsilon \to 0}\frac{f(x + \varepsilon) - f(x)}{\varepsilon}. $$

But when working with derivatives in a computer program,
we can't necessarily take limits of an arbitrary function.
Thus, how to deal with derivatives?

One approach is _numerical differentiation_,
where we approximate the limit by using a really small $\varepsilon$:

```haskell
numDiff' eps f x = (f (x + eps) - f x) / eps

numDiff = numDiff' 1e-10
```

This is prone to numerical stability issues
and doesn't compute the real derivative but only an approximation to it.

Another approach is what we followed in the previous post: _symbolic differentiation_.
This is the same way that one is used to compute derivatives by hand:
you take the algebraic operations that you learned in the calculus class
and implement them as transformations on an expression type representing a syntax tree.
One difficult of this, as you may have noticed, is that symbolic calculations
require lots of rewriting to get the derivative in a proper form.
They also require that you work directly with expressions and not with functions.
This, despite being mitigated by our `eval` and `uneval` operators,
can be pretty inefficient when your code is naturally composed of functions.
Besides that,
if we wanted to change our `Expr` type, for example,
to use a more efficient operation under the hood, or adding a `:^:` constructor for power operations,
or adding new transcendental functions,
we would have to modify both our `eval` and `diff` functions to consider this.123

A third option that solves all the previous issues is
[Automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation).
This uses the fact that any `Floating a => a -> a` is in fact a composition
of arithmetic operations and some simple transcendental functions such as `exp`, `cos`, `sin`, etc.
Since we know how to differentiate those,
we can augment our function evaluation to calculate at the same time
both the function value and the exact value of the derivative at any given point.
As we will see, we will even be able to recover symbolic differentiation
as a subcase of automatic differentiation.[^symb-ad]

[^symb-ad]: An idea I first heard of while reading the documentation of the [ad](https://hackage.haskell.org/package/ad) package.

### Dual Numbers

Here we will do the simplest case of automatic differentiation,
namely _forward-mode AD_ using [dual numbers](https://en.wikipedia.org/wiki/Dual_number).
This is only for illustrative purposes.
If you are planning in to use automatic differentiation in a program,
I like recommend taking a look at the [ad](https://hackage.haskell.org/package/ad) package.

In mathematics, a _dual number_ is an expression $a + b\varepsilon$
with the additional property that $\varepsilon^2 = 0$.
One can think of it as augmenting the real numbers with an infinitesimal factor.
As another intuition: this definition is very similar to the complex numbers,
with the difference that instead of $i^2 = -1$, we have $\varepsilon^2 = 0$.[^dual-complex]

The nicety of the dual numbers is that they can automatically calculate the derivative of any analytic function.
To view how this works, let's look at the Taylor Series of a `f` expanded around a point `a`.

$$ f(a + b\varepsilon) = \sum_{n=0}^\infty \frac{1}{n!}f^{(n)} (b\varepsilon)^n = f(a) + bf'(a)\varepsilon. $$

Therefore, applying $f$ to a number with an infinitesimal part
amounts to taking its first order expansion.

[^dual-complex]: If you're into Algebra,
you can view the complex numbers as the polynomial quotient
$\mathbb{R}[X] / \langle X^2 + 1 \rangle$
while the dual numbers are $\mathbb{R}[X] / \langle X^2 \rangle$.

Ok, back to Haskell.
As usual, we translate this definition into Haskell as a parameterized data type carrying two values.

```haskell
data Dual a = Dual a a
  deriving (Show, Eq)
```

Later, it will also be useful to have functions extracting the real and infinitesimal parts
of a dual number.

```haskell
realpart (Dual a _) = a
epsPart  (Dual _ b) = b
```

Alright, just like with expressions we will want to make `Dual a` into a number.
The sum and product of two dual numbers are respectively linear and bilinear
because, well... Because we wouldn't be calling it "sum" and "product" it they weren't.
In math it reads as

$$ \begin{aligned}
  (a + b\varepsilon) +     (c + d\varepsilon) &= (a + c) + (b + d)\varepsilon, \\
  (a + b\varepsilon) \cdot (c + d\varepsilon) &= ac + (bc + ad)\varepsilon + \cancel{bd\varepsilon^2}.
\end{aligned}$$

If you found those as having a strong resemblance to the sum and product rules
for derivatives, is because they do!
These are our building blocks for differentiation.

```haskell
instance Num a => Num (Dual a) where
 -- Linearity
 (Dual a b) + (Dual c d) = Dual (a + c) (b + d)
 (Dual a b) - (Dual c d) = Dual (a - c) (b - d)
 -- Bilinearity and cancel ε^2
 (Dual a b) * (Dual c d) = Dual (a * c) (b*c + a*d)
 -- Embed integers as only the real part
 fromInteger n     = Dual (fromInteger n) 0
 -- These below are not differentiable functions...
 -- But their first order expansion equals this except at zero.
 abs    (Dual a b) = Dual (abs a)    (b * signum a)
 signum (Dual a b) = Dual (signum a) 0
```

For division, we use the same trick as with complex numbers
and multiply by the denominators conjugate.

$$
  \frac{a + b\varepsilon}{c + d\varepsilon}
  = \frac{a + b\varepsilon}{c + d\varepsilon} \cdot \frac{c - d\varepsilon}{c - d\varepsilon}
  = \frac{ac + (bc - ad)\varepsilon}{c^2}
  = \frac{a}{c} + \frac{bc - ad}{c^2}\varepsilon
$$


```haskell
instance (Fractional a) => Fractional (Dual a) where
 (Dual a b) / (Dual c d) = Dual (a / c) ((b*c - a*d) / c^2)
 fromRational r          = Dual (fromRational r) 0
```

Finally, to extend the transcendental functions
to the dual numbers, we use the first order expansion described above.
We begin by writing a helper function that represents this expansion.

```haskell
-- First order expansion of a function f with derivative f'.
fstOrd :: Num a => (a -> a) -> (a -> a) -> Dual a -> Dual a
fstOrd f f' (Dual a b) = Dual (f a) (b * f' a)
```

And the floating instance is essentially our calculus cheatsheet again.

```haskell
instance Floating a => Floating (Dual a) where
 -- Embed as a real part
 pi = Dual pi 0
 -- First order approximation of the function and its derivative
 exp   = fstOrd exp exp
 log   = fstOrd log recip
 sin   = fstOrd sin cos
 cos   = fstOrd cos (negate . sin)
 asin  = fstOrd asin (\x -> 1 / sqrt (1 - x^2))
 acos  = fstOrd acos (\x -> -1 / sqrt (1 - x^2))
 atan  = fstOrd atan (\x -> 1 / (1 + x^2))
 sinh  = fstOrd sinh cosh
 cosh  = fstOrd cosh sinh
 asinh = fstOrd asinh (\x -> 1 / sqrt (x^2 + 1))
 acosh = fstOrd acosh (\x -> 1 / sqrt (x^2 - 1))
 atanh = fstOrd atanh (\x -> 1 / (1 - x^2))
```

### Derivatives

```haskell
autoDiff f c = epsPart (f (c :+@ 1))
```

## Diff in terms of eval

```haskell
diff_ f = autoDiff (eval f) X
```

polymorphic version


```haskell
-- Symbolically differentiate any expression
diff :: Floating a => Expr a -> Expr a
diff f = let g = fmap fromNum f
         in autoDiff (eval g) X
 where
  fromNum x = Const x :+@ 0

deriving instance Functor Expr
```
