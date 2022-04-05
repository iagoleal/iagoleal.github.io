---
title: Let's program a Calculus I student
keywords: [haskell, functional-programming]
date: 2022-03-31
---

Last week I did a little Haskell show-off to two friends.
Besides the classical infinite list of primes one-liner
and mandatory factorial and Fibonacci functions,
I also wanted something more complex.
Specifically, since they work with [Graphical Linear Algebra](https://graphicallinearalgebra.net/),
I wanted to show them how nice it is to write DSLs in Haskell.
It feels almost too natural.
You write your types as if they are grammars,
your functions as if they rewriting rules
and _bang_, by the magic of recursion everything works.

I offered them what I consider the perfect exhibition for this: _Let's make a solver for a Calculus exam!_

Calculus is a subject that in their College years, everybody learns to respect (or fear).
Thus, at first sight this may seem too monumental of a task for a mere exposition.
But what if I told you that if we restrict ourselves to derivatives,
it takes about a hundred lines of code?
A lot of people are not used to think of Calculus this way,
but computing derivatives is actually a pretty straightforward algorithm.

One thing that one of those friends,
who is a Professor in a Department of Computer Science,
said really resonated with me:
"People would struggle much less with math if they learnt in school how to write syntax trees."[^joao-comment]

[^joao-comment]: The phrase wasn't exactly that. It had a better effect.
But it has been almost a week and I have the memory of a gold fish.
The intention is preserved thought.

### Discussion about syntax trees

## Please be rational

Before delving into the depths of first-year undergraduate math,
let's take a step back and start with something simpler: _rational functions_.

```haskell
module Numeric.Fraction where
```

A rational function is formed of sums, products, and divisions
of numbers and a indeterminate symbol, traditionally denoted by $x$.
An example is something like:

$$ \frac{32x^4 + \frac{5}{4}x^3 - x + 21}{\frac{5x^{87} - 1}{23x} + 41 x^{76}}.$$

Let's construct the rational functions over some field of numbers `a` then.
It should have $x$, numbers (called _constants_), and arithmetic operations between them.

```haskell
data Fraction a = X
                | Const a
                | (Fraction a) :+: (Fraction a)
                | (Fraction a) :*: (Fraction a)
                | (Fraction a) :/: (Fraction a)
  deriving (Show, Eq)
```

I choose to give it the name `Fraction` because rational functions
are represented by fractions of polynomials.
We make it a parameterized typed because `a` could be any numeric field,
just like in math we use the notations $\mathbb{Q}(x)$, $\mathbb{C}(x)$, $\mathbb{Z_{17}}(x)$
to denote the rational functions over different fields.

Since we are using operator constructors,
let's give them the same associativity and fixity as the built-in operators.

```haskell
infixl 6 :+: -- Left associative
infixl 7 :*: -- Left associative with higher precedence that :+:
infixl 7 :/: -- Left associative with same precedence as :*:
```

For now our constructors are only formal, they just create syntax trees:

```haskell
ghci> Const 2 :+: Const 2 :+: X
(Const 2 :+: Const 2) :+: X
it :: Num a => Fraction a
```

We can teach it how to simplify these equations
but since the focus here is on derivatives,
we will postpone this to a [further section](#Simplifier).
Let's say that right now our student will just solve the problems
and return the exam answers in long form without simplifying anything.

The next thing is thus teach it how to evaluate an expression at a value.
The nice part is that in terms of implementation,
that's equivalent to writing an interpreter from the Fractions to the base field.

```haskell
eval :: Fractional a => Fraction a -> a -> a
eval X         c = c
eval (Const a) _ = a
eval (f :+: g) c = eval f c + eval g c
eval (f :*: g) c = eval f c * eval g c
eval (f :/: g) c = eval f c / eval g c
```

This is it.
Our evaluator traverses the expression tree by turning each `X` leaf into the value `c`,
keeping constants as themselves,
and collapsing the nodes according to the operation they represent.
As an example:


```haskell
ghci> p = X :*: X :+: (Const 2 :*: X) :+: Const 1
p :: Num a => Fraction a
ghci> eval p 2
9.0
it :: Fractional a => a
```

### What is a number after all?

One nicety about languages like Haskell
is that they are not only good for writing DSls,
they are also good for writing _embedded DSLs_.
That is, something like our symbolic Fractions
can look like just another ordinary part of the language.

It won't be nice to just write `X^2 + 2*X + 1`
instead of the expression we evaluated above?

Well, we first need to teach or program how to use
the built-in numeric constants and arithmetic operations.
We achieve this through the typeclasses `Num` and `Fractional`.
Wish are kinda the Haskell equivalents of saying
our type forms a [Ring](https://en.wikipedia.org/wiki/Ring_(mathematics))
and [Field](https://en.wikipedia.org/wiki/Field_(mathematics)).

```haskell
instance Num a => Num (Fraction a) where
 -- For the operations we just use the constructors
 (+) = (:+:)
 (*) = (:*:)
 -- This serves to embed integer constants in our Ring.
 -- Good for us that we already have a constructor for that.
 fromInteger n = Const (fromInteger n)
 -- This one is how to do `p -> -p`.
 -- We didn't define subtraction, so let's just multiply by -1.
 negate p      = Const (-1) :*: p
 -- These ones are kinda the problem of `Num`...
 -- For this exposition, let's just pretend they don't exist.
 abs    = id
 signum = id
```

This makes our type into a Ring and we can now use constants, `+` and `*` with it.
The code to make it into a Field is equally straightforward.

```haskell
instance Fractional a => Fractional (Fraction a) where
 (/) = (:/:)
 fromRational r = Const (fromRational r)
```

Let's see how it goes

```haskell
ghci> (X^2 + 2*X + 1) / (X^3 - 0.6)
(((X :*: X) :+: (Const 2.0 :*: X)) :+: Const 1.0) :/: (((X :*: X) :*: X) :+: (Const (-1.0) :*: Const 0.5))
it :: Fractional a => Fraction a
```

What we wrote is definitely much cleaner than the internal representation.
But there is still one more nicety:
Doing this also gave us the ability to compose expressions!
Recall the type of our evaluator function:

```haskell
eval :: Fractional field => Fraction field -> field -> field
```

But we just implemented a `Fractional (Fraction a)` instance!
Thus, as long as we keep our Fractions polymorphic,
we can evaluate an expression at another expression.

```haskell
ghci> eval (X^2 + 3) (X + 1)
((X :+: Const 1.0) :*: (X :+: Const 1.0)) :+: Const 3.0
it :: Fractional a => Fraction a
```

### Enough arithmetic, it's Calculus time

Alright, alright. Time to finally teach some calculus.
Remember all the lectures, all the homework...
Well, in the end what we need to differentiate a rational function
are only five simple equations: 3 tree recursive rules and 2 base cases.

```haskell
diff :: Fractional a => Fraction a -> Fraction a
diff X           = 1
diff (Const _)   = 0
diff (f :+: g)   = diff f + diff g
diff (f :*: g)   = diff f * g + f * diff g
diff (f :/: g)   = (diff f * g - f * diff g) / g^2
```

Well, that's it.
Now that we've tackled the rational functions,
let's meet some old friends from Calculus again.

## A little more complexity

```haskell
module Numeric.Expression where
```

## What about the chain rule?

### Introduce functions constructor

```haskell
data Func = Cos | Sin | Log | Exp | Asin | Acos | Atan
  deriving Show
```

```haskell
data Expr a = X
                | Const a
                | (Expr a) :+: (Expr a)
                | (Expr a) :*: (Expr a)
                | (Expr a) :/: (Expr a)
                | Apply Func (Expr a)
```

### Floating type class

```haskell
instance Floating a => Floating (Expr a) where
 pi      = Const pi
 exp     = Apply Exp
 log     = Apply Log
 sin     = Apply Sin
 cos     = Apply Cos
 asin    = Apply Asin
 acos    = Apply Acos
 atan    = Apply Atan
 sinh x  = (exp x - exp (-x)) / 2
 cosh x  = (exp x + exp (-x)) / 2
 asinh x = log (X + sqrt ( X^2 - 1))
 acosh x = log (X + sqrt ( X^2 + 1))
 atanh x = (log (1 + x) - log (1 - x)) / 2
```


### cheatsheets

```haskell
cheatsheet :: Floating a => Func -> Expr a
cheatsheet Sin  = cos X
cheatsheet Cos  = negate (sin X)
cheatsheet Exp  = exp X
cheatsheet Log  = 1 / X
cheatsheet Asin = 1 / sqrt (1 - X^2)
cheatsheet Acos = -1 / sqrt (1 - X^2)
cheatsheet Atan = 1 / (1 + X^2)
```


```haskell
eval :: Floating a => Expr a -> a -> a
eval X         c = c
eval (Const a) _ = a
eval (f :+: g) c = eval f c + eval g c
eval (f :*: g) c = eval f c * eval g c
eval (Apply f e) c = let g = calculator f
                     in g (eval e c)
```


```haskell
diff :: Floating a => Expr a -> Expr a
diff X           = 1
diff (Const _)   = 0
diff (f :+: g)   = diff f + diff g
diff (f :*: g)   = f * diff g + diff f * g
diff (Apply f e) = let df = cheatsheet f
                   in eval df e * diff e
```

## Simplifier

## Taylor series
