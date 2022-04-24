---
title: Let's Program a Calculus Student II
subtitle: Automatic Differentiation
keywords: [haskell, computer-algebra, functional-programming, automatic-differentiation]
date: 2022-04-20
---

https://hackage.Haskell.org/package/ad

https://auxin.org/pdf/1804.00746.pdf

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

# Automatic Differentiation

## Dual Numbers

data Dual

instances


```haskell
```

```haskell
```

```haskell
```


### Taking derivatives

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
