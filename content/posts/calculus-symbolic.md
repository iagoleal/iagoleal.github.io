---
title: Let's Program a Calculus Student
keywords: [haskell, computer-algebra, functional-programming]
date: 2022-04-05
---

Last week I did a little Haskell show-off for two friends.
Besides the classical infinite list of primes one-liner
and mandatory factorial and Fibonacci functions,
I also wanted something more complex.
Specifically, since they work with [Graphical Linear Algebra](https://graphicallinearalgebra.net/),
I wanted to show them how nice it is to write DSLs in Haskell.
It feels almost too natural.
You write your types as if they are grammars,
your functions as if they are rewriting rules
and _bang_, by the magic of recursion everything works.

I offered them what I consider the perfect exhibition for this: _Let's make a solver for a Calculus exam!_

Calculus is a subject that in their College years, everybody learns to respect (or fear).
Thus, at first sight this may seem too monumental of a task for a mere exposition.
But what if I told you that if we restrict ourselves to derivatives,
it takes about a hundred lines of code?
A lot of people are not used to thinking of Calculus this way,
but computing derivatives is actually a pretty straightforward algorithm.

One thing that one of those friends,
who is a Professor in the Department of Computer Science,
said really resonated with me:
"People would struggle much less with math if they learned in school how to write syntax trees."[^joao-comment]

I really liked this phrase and would add even more:
learning about syntax trees (and their siblings s-expressions) and recursion
eased my way not only with math but with learning grammar as well.
How I wish that math and languages classes from school
worked with concepts that are as uniform as they could.
Well, enough rambling. Time to do some programming!

[^joao-comment]: The phrase wasn't exactly that. It had a better effect.
But it has been almost a week and I have the memory of a goldfish.
The intention is preserved thought.

## Please be rational

Before delving into the depths of first-year undergraduate math,
let's take a step back and start with something simpler: _rational functions_.

```haskell
module Calculus.Fraction where
```

A rational function is formed of sums, products, and divisions
of numbers and a indeterminate symbol, traditionally denoted by $x$.
An example is something like

$$ \frac{32x^4 + \frac{5}{4}x^3 - x + 21}{\frac{5x^{87} - 1}{23x} + 41 x^{76}}.$$

Let's construct the rational functions over some field of numbers `a`.
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
We make it a parameterized type because `a` could be any numeric field,
just like in math we use the notations $\mathbb{Q}(x)$, $\mathbb{C}(x)$, $\mathbb{Z_{17}}(x)$
to denote the rational functions over different fields.

Since we are using operator constructors,
let's give them the same associativity and fixity as the built-in operators.

```haskell
infixl 6 :+:      -- Left associative
infixl 7 :*:, :/: -- Left associative with higher precedence than :+:
```

For now our constructors are only formal, they just create syntax trees:

```ghci
ghci> Const 2 :+: Const 2 :+: X
(Const 2 :+: Const 2) :+: X
it :: Num a => Fraction a
```

We can teach it how to simplify these equations
but since the focus here is on derivatives,
we will postpone this to a [further section](#Simplifier).
Let's say that right now our student will just solve the problems
and return the exam answers in long-form without simplifying anything.

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

```ghci
ghci> p = X :*: X :+: (Const 2 :*: X) :+: Const 1
p :: Num a => Fraction a
ghci> eval p 2
9.0
it :: Fractional a => a
```

### What is a number after all?

One nicety about languages like Haskell
is that they are not only good for writing DSls,
but they are also good for writing _embedded DSLs_.
That is, something like our symbolic Fractions
can look like just another ordinary part of the language.

It won't be nice to just write `X^2 + 2*X + 1`
instead of the expression we evaluated above?

Well, we first need to teach or program how to use
the built-in numeric constants and arithmetic operations.
We achieve this through the typeclasses `Num` and `Fractional`.
This is kind of Haskell's way of saying
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

```ghci
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

```ghci
ghci> eval (X^2 + 3) (X + 1)
((X :+: Const 1.0) :*: (X :+: Const 1.0)) :+: Const 3.0
it :: Fractional a => Fraction a
```

### Enough arithmetic, it's Calculus time

Alright, alright. Time to finally teach some calculus.
Remember all the lectures, all the homework...
Well, in the end, what we need to differentiate a rational function
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

## Elementary Functions

In calculus, besides rational functions,
we also have sines, cosines, exponentials, logs
and anything that can be formed combining those
via composition or arithmetic operations.
For example:

$$ \frac{1}{23}\log\left(\sin(3x^3) + \frac{e^{45x} - 21}{x^{0.49}\mathrm{asin}(-\frac{\pi}{x})}\right) + \cos(x^2)$$


This sort of object is called an [Elementary function](https://en.wikipedia.org/wiki/Elementary_function)
in the math literature but here we will call it simply an _expression_.

```haskell
module Calculus.Expression where
```

Let's create a type for our expressions then.
It is pretty similar to the `Fraction` type from before
with the addition that we can also apply some transcendental functions.

```haskell
data Expr a = X
            | Const a
            | (Expr a) :+: (Expr a)
            | (Expr a) :*: (Expr a)
            | (Expr a) :/: (Expr a)
            | Apply Func (Expr a)
   deriving (Show, Eq)

data Func = Cos | Sin | Log | Exp | Asin | Acos | Atan
  deriving Show
```

The `Func` type is a simple enumeration of the most common functions
that one may find running wild on a Calculus textbook.
There are also other possibilities but they are generally
composed from those basic building blocks.

Since the new constructor plays no role in arithmetic,
We can define instances `Num (Expr a)` and `Fractional (Expr a)`
that are identical to those we made before.
But having these new functions also allows us to add a `Floating` instance to `Expr a`,
which is sort of Haskell's way of expressing things that act like real/complex numbers.

```haskell
instance Floating a => Floating (Expr a) where
 -- Who doesn't like pi, right?
 pi      = Const pi
 -- Those are easy, we just need to use our constructors
 exp     = Apply Exp
 log     = Apply Log
 sin     = Apply Sin
 cos     = Apply Cos
 asin    = Apply Asin
 acos    = Apply Acos
 atan    = Apply Atan
 -- We can write hyperbolic functions through exponentials
 sinh x  = (exp x - exp (-x)) / 2
 cosh x  = (exp x + exp (-x)) / 2
 asinh x = log (x + sqrt (x^2 - 1))
 acosh x = log (x + sqrt (x^2 + 1))
 atanh x = (log (1 + x) - log (1 - x)) / 2
```

### Time to update our methods

We already have our type and its instances.
Now it is time to also consider derivatives
of the transcendental part of the expressions.
The evaluator is almost equal except for a new pattern:

```haskell
eval :: Floating a => Expr a -> a -> a
eval X         c = c
eval (Const a) _ = a
eval (f :+: g) c = eval f c + eval g c
eval (f :*: g) c = eval f c * eval g c
eval (f :/: g) c = eval f c / eval g c
eval (Apply f e) c = let g = calculator f
                     in g (eval e c)
```

We also had to define a `calculator` helper
that translates between our `Func` type and the actual functions.
This is essentially the inverse of the floating instance
we defined above but I couldn't think of a way to do that
with less boilerplate without using some kind of metaprogramming.[^suggestions]

[^suggestions]: If you have any suggestions to make this code mode elegant,
feel free to contact me and we can edit it. :)

```haskell
calculator :: Floating a => Func -> (a -> a)
calculator Cos  = cos
calculator Sin  = sin
calculator Log  = log
calculator Exp  = exp
calculator Asin = asin
calculator Acos = acos
calculator Atan = atan
```

The derivative is pretty similar,
with the difference that we implement the chain rule
instead of for the `Apply` constructor.

Let's start by writing a cheatsheet of derivatives.
This is the kind of thing you're probably not allowed to carry to a Calculus exam,
but let's say that our program has it stored in its head (provided this makes any sense).
Our cheatsheet will get a `Func` and turn it into the expression
of its derivative.

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

Finally, the differentiator is exactly the same as before
except for a new pattern that looks for the derivative on the cheatsheet
and evaluates the chain rule using it.

```haskell
diff :: Floating a => Expr a -> Expr a
diff X           = 1
diff (Const _)   = 0
diff (f :+: g)   = diff f + diff g
diff (f :*: g)   = diff f * g + f * diff g
diff (f :/: g)   = (diff f * g - f * diff g) / g^2
diff (Apply f e) = let f' = cheatsheet f
                   in eval f' e * diff e
```

This way we finish our Calculus student program.
It can write any elementary function as normal Haskell code,
evaluate them, and symbolically differentiate them.
So what do you think?

## What's next?

Although we finished our differentiator,
there are a couple of topics that I think are worth discussing
because they are simple enough to achieve
and will make our program a lot more polished or fun to play with.

### Simplifier

Definitely the least elegant part of our program is the expression simplifier.
It is as straightforward as the rest,
consisting of recursively applying rewriting rules to an expression,
but there are a lot of corner cases and possible rules to apply.
Besides that, sometimes which equivalent expression is the simple one
can be up to debate.

We first write the full simplifier.
It takes an expression and apply rewriting rules to it
until the process converges, i.e. the rewriting does nothing.

```haskell
simplify :: (Eq a, Floating a) => Expr a -> Expr a
simplify expr = loop expr X
 where loop cur prev
        | cur == prev = cur
        | otherwise   = loop (rewrite cur) cur
```

This is a typical tail-recursive loop.
The only detail is that we must provide the first expression.
Anything works really...
I choose to use `X` because it is a non-simplifiable expression.

From this function, we can define a new version of `diff`
that simplifies its output after computing it.

```haskell
diffS = simplify . diff
```

The bulk of the method is formed by a bunch of identities.
You can think of them as the many math rules that a student
should remember in order to simplify an expression while solving a problem.
Since there is really no right answer[^anedocte-trig]
when we are comparing equals with equals,
any implementation will invariably be rather ad hoc.
One thing to remember though is that the rules should eventually converge.
If you use identities that may cancel each other,
the simplification may never terminate.

[^anedocte-trig]: Countless times I only finished a proof
because I used an identity on the non-intuitive side.
For example writing something like
$$f(x) = f(x) \cdot 1 = f(x) \cdot (\sin(x)^2 + \cos(x)^2).$$
Those are always some great _A-ha!_ moments.

```haskell
-- Constants
rewrite (Const a :+: Const b)   = Const (a + b)
rewrite (Const a :*: Const b)   = Const (a * b)
rewrite (Const a :/: Const b)   = Const (a / b)
rewrite (Apply func (Const a))  = Const (calculator func a)
-- Associativity
rewrite (f :+: (g :+: h)) = (rewrite f :+: rewrite g) :+: rewrite h
rewrite (f :*: (g :*: h)) = (rewrite f :*: rewrite g) :*: rewrite h
-- Identity for sum
rewrite (f :+: Const 0)  = rewrite f
rewrite (Const 0 :+: f)  = rewrite f
rewrite (f :+: Const a)  = Const a :+: rewrite f
-- Identity for product
rewrite (f :*: Const 1)  = rewrite f
rewrite (Const 1 :*: f)  = rewrite f
rewrite (f :*: Const 0)  = Const 0
rewrite (Const 0 :*: f)  = Const 0
rewrite (f :*: Const a)  = Const a :*: rewrite f
-- Identity for division
rewrite (Const 0 :/: f)  = Const 0
rewrite (f :/: Const 1)  = rewrite f
-- Inverses
rewrite (f :/: h)
 | f == h = Const 1
rewrite ((f :*: g) :/: h)
 | f == h = rewrite g
 | g == h = rewrite f
rewrite (f :+: (Const (-1) :*: g))
 | f == g = Const 1
-- Function inverses
rewrite (Apply Exp  (Apply Log  f)) = rewrite f
rewrite (Apply Log  (Apply Exp  f)) = rewrite f
rewrite (Apply Sin  (Apply Asin f)) = rewrite f
rewrite (Apply Asin (Apply Sin  f)) = rewrite f
rewrite (Apply Cos  (Apply Acos f)) = rewrite f
rewrite (Apply Acos (Apply Cos  f)) = rewrite f
rewrite (Apply Atan ((Apply Sin  f) :/: (Apply Cos g)))
  | f == g = rewrite f
-- Recurse on constructors
rewrite (f :+: g)   = rewrite f :+: rewrite g
rewrite (f :*: g)   = rewrite f :*: rewrite g
rewrite (f :/: g)   = rewrite f :/: rewrite g
rewrite (Apply f e) = Apply f (rewrite e)
-- Otherwise stop recursion and just return itself
rewrite f = f
```

I find it interesting to look at the size of this `rewrite` function
and think of the representation choices we made along this post.
There are many equivalent ways to write the same thing,
forcing us to keep track of all those equivalence relations.

### Taylor series

One of the niceties of working with a lazy language
is how easy it is to work with infinite data structures.
In our context, we can take advantage of that
to write the _Taylor Series_ of an expression.

The Taylor series of $f$ at a point $c$ is defined as the infinite sum

$$ f(x) = \sum_{n = 0}^\infty \frac{f^{(n)}(c)}{n!} (x-c)^n.$$

Let's first write a function that turns an expression and a point
into an infinite list of monomials.
We do that by generating a list of derivatives and factorials,
which we assemble for each natural number.

```haskell
taylor :: (Eq a, Floating a) => Expr a -> a -> [Expr a]
taylor f c = simplify <$> zipWith3 assemble [0..] derivatives factorials
 where
  assemble n f' nfat = Const (eval f' c / nfat) * (X - Const c)^n
  -- Infinite list of derivatives [f, f', f'', f'''...]
  derivatives = iterate diff f
  -- Infinite list of factorials [0!, 1!, 2!, 3!, 4!...]
  factorials  = fmap fromInteger factorials'
  factorials' = 1 : zipWith (*) factorials' [1..]
```

We can also write the partial sums which only have $N$ terms
of the Taylor expansion.
These have the computational advantage of actually being evaluable.

```haskell
approxTaylor :: (Eq a, Floating a) => Expr a -> a -> Int -> Expr a
approxTaylor f c n = (simplify . sum .take n) (taylor f c)
```

At last, a test to convince ourselves that it works.

```ghci
ghci> g = approxTaylor (exp X) 0
g :: (Eq a, Floating a) => Int -> Expr a
ghci> g 10
((((((((Const 1.0 :+: X) :+: ((Const 0.5 :*: X) :*: X)) :+: (((Const 0.16666666666666666 :*: X) :*: X) :*: X)) :+: ((((Const 4.1666666666666664e-2 :*: X) :*: X
) :*: X) :*: X)) :+: (((((Const 8.333333333333333e-3 :*: X) :*: X) :*: X) :*: X) :*: X)) :+: ((((((Const 1.388888888888889e-3 :*: X) :*: X) :*: X) :*: X) :*: X
) :*: X)) :+: (((((((Const 1.984126984126984e-4 :*: X) :*: X) :*: X) :*: X) :*: X) :*: X) :*: X)) :+: ((((((((Const 2.48015873015873e-5 :*: X) :*: X) :*: X) :*
: X) :*: X) :*: X) :*: X) :*: X)) :+: (((((((((Const 2.7557319223985893e-6 :*: X) :*: X) :*: X) :*: X) :*: X) :*: X) :*: X) :*: X) :*: X)
it :: (Eq a, Floating a) => Expr a
ghci> eval (g 10) 1
2.7182815255731922
it :: (Floating a, Eq a) => a
```

## Acknowledgements

This post only exists thanks to the great chats I had with João Paixão and Lucas Rufino.
Besides listening to me talking endlessly about symbolic expressions and recursion,
they also asked a lot of good questions and provided the insights
that helped shape what became this post.
