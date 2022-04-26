---
title: Let's Program a Calculus Student II
subtitle: From Automatic to Symbolic Differentiation
keywords: [haskell, computer-algebra, functional-programming, automatic-differentiation]
date: 2022-04-25
---

On the [previous post](/posts/calculus-symbolic),
we wrote a data type representing a formula
that could appear in a Calculus class
and discussed how to find its derivative.
The approach that we chose was rather algebraic,
we took each of the formulas for a derivative
and taught the program to recursively apply them.

Today we will redefine these symbolic derivatives
using a different approach: _automatic differentiation_.
This new way to calculate derivatives will only depend
on the evaluation function for expressions,
Thus decoupling it from whatever representation
we choose for our expressions and, of course,
it is always good to learn different ways to build something!

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor  #-}
module Calculus.AutoDiff where
import Calculus.Expression
```

## On polymorphism, evaluation and reflection

Recall our evaluation function from the previous post.
Its signature was

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
 exp   = fstOrd exp   exp
 log   = fstOrd log   recip
 sin   = fstOrd sin   cos
 cos   = fstOrd cos   (negate . sin)
 asin  = fstOrd asin  (\x -> 1 / sqrt (1 - x^2))
 acos  = fstOrd acos  (\x -> -1 / sqrt (1 - x^2))
 atan  = fstOrd atan  (\x -> 1 / (1 + x^2))
 sinh  = fstOrd sinh  cosh
 cosh  = fstOrd cosh  sinh
 asinh = fstOrd asinh (\x -> 1 / sqrt (x^2 + 1))
 acosh = fstOrd acosh (\x -> 1 / sqrt (x^2 - 1))
 atanh = fstOrd atanh (\x -> 1 / (1 - x^2))
```

### Derivatives of functions

Now that we have setup all the dual number tooling,
it is time to calculate some derivatives.
From the first order expansion $f(a + b\varepsilon) = f(a) + bf'(a)\varepsilon$,
we see that by applying a function to $a + \varepsilon$,
that is, setting $b = 1$,
we calculate $f$ and its derivative at $a$.
Let's test this in ghci:

```ghci
ghci> f x = x^2 + 1
f :: Num a => a -> a
ghci> f (Dual 3 1)
Dual 10 6
it :: Num a => Dual a
```

Just as we expected!
We can thus write a differentiation function
by doing this procedure and taking only the $\varepsilon$ component.

```haskell
autoDiff f c = epsPart (f (Dual c 1))
```

Some cautionary words: remember from the previous discussion
that to have access to the structure of a function,
we need it to be polymorphic.
In special, our `autoDiff` has type
`Num a => (Dual a -> Dual b) -> (a -> b)`.
It gets a function on dual numbers and spits out a function on numbers.
But, for our use case it is fine because we can specialize this signature to

```haskell
autoDiff :: (forall a . Floating a => a -> a) -> (forall a . Floating a => a -> a)
```

### Derivatives of expressions

Recall we can use `eval` to turn an expression into a function
and, reciprocally, we can apply a polymorphic function to the constructor `X`
to turn it into an expression.
But what happens if we take `eval f` and compute its derivative at the point `X`?
We get the __symbolic derivative__ of `f` of course!

```haskell
diff_ f = autoDiff (eval f) X
```

Some tests in the REPL to see that it works:

```ghci
ghci> diff_ (sin (X^2))
(Const 1.0 :*: X :+: X :*: Const 1.0) :*: Apply Cos (X :*: X)
it :: Floating a => Expr a
```

This function has a flaw nevertheless.
It depends too much of polymorphism.
While our symbolic differentiator from the previous post
worked for an expression `f :: Expr Double`, for example,
this new function depends on being able to convert
`f` to a polymorphic function, which it can't do in this case.
This gets clear by looking at the type signature of `diff_`:

```haskell
diff_ :: Floating a => Expr (Dual (Expr a)) -> Expr a
```

But not all hope is lost!
Our differentiator works, we only need to discover how to
turn an `Expr a` into an `Expr (Dual (Expr a))` and we can get the proper type.

Let's think... Is there a canonical way of embedding a value as an expression?
Of course there is! The `Const` constructor does exactly that.
Similarly, we can view a "normal" number as a dual number with zero infinitesimal part.
Thus, if we change each coefficient in an expression by the rule
`\ c -> Dual (Const c) 0`, we get an expression of the type we need
without changing any meaning.

To help us change the coefficients, let's give a `Functor` instance to `Expr`.
We could write it by hand but let's use some GHC magic to automatically
derive it for us.

```haskell
deriving instance Functor Expr
```

Finally, our differentiation function is equal to `diff_`,
except that it first converts all coefficients of the input to the proper type.

```haskell
-- Symbolically differentiate expressions
diff :: Floating a => Expr a -> Expr a
diff f = let g = fmap from f
         in autoDiff (eval g) X
 where
  from x = Dual (Const x) 0
```

Just apply it to a monomorphic expression and voilà!

```ghci
ghci> diff (sin (X^2) :: Expr Double)
(Const 1.0 :*: X :+: X :*: Const 1.0) :*: Apply Cos (X :*: X)
it :: Expr Double
```

## References

* [ad package on Hackage](https://hackage.Haskell.org/package/ad)
* [The Simple Essence of Automatic Differentiation](https://arxiv.org/pdf/1804.00746.pdf) by Conal Elliott.
