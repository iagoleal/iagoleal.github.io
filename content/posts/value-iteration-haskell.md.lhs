---
title: Playing with Value Iteration in Haskell
keywords: [math, dynamic-programming, Haskell]
date: 2024-02-29
description:
  From monads to stochastic control and from fixed points to value iteration.
  An exploration about using abstract concepts to construct concretes algorithms.
suppress-bibliography: true
---

In [a previous post](/posts/automata-monads),
we explored how to use monads to express different kinds of finite automata
as instances of dynamics with context.
These monadic transitions, though, appear in disguise in many other areas of mathematics.

Today we are going in a similar exploration of _Decision Processes_,
a close cousin to Finite Automata from the not-so-close field of Optimal Control.
These are multistage decision problems with the same kind of dynamics as finite automata.
Furthermore, we can formulate Value Iteration
--- one of the usual to optimize them ---
compactly using the Haskell machinery.

Value Iteration is among my favorite algorithms
and I have used it to solve a lot of real-world problems.
Therefore, let's use Haskell to play with it and see what we can get.

> {-# LANGUAGE GHC2021                              #-}
> {-# LANGUAGE PackageImports,      RecordWildCards #-}
> {-# LANGUAGE AllowAmbiguousTypes, TypeFamilies    #-}
> import              Data.Foldable   (foldlM)
> import              Data.Kind       (Type)
> import              Data.Semigroup  (Arg(Arg))
> import "containers" Data.Map.Strict qualified as M
> import "vector"     Data.Vector     qualified as V

Decision Processes
==================

For us, a _decision process_ consists of a controllable dynamical system
where, at each time step, we choose to take some action.
This transitions the system into some uncertain future state,
represented as a monadic context over the states.
Each such action also incurs a cost --- modeled as a real number ---
and, to keep up with inflation,
we introduce a discount factor $\gamma \in [0, 1)$
representing how much future costs are worth in today's value.

> -- | Finite automaton with states `s`, actions `a`, and monadic context `m`.
> --   The type parameters `s` and `a` are assumed to represent finite sets.
> data Process m s a = Process
>   { next     :: s -> a -> m s    -- ^ Change state with a context.
>   , cost     :: s -> a -> Double -- ^ Cost of choosing an action.
>   , discount :: Double           -- ^ How much future costs are worth in today's value.
>   }

The standard example of this structure are _Markov Decision Process_,
where `m` is a probability distribution monad,
representing stochastic transitions.
We can, nevertheless, use any appropriate context in place of it
and all the theory will keep working.
Have fun trying to fit your favorite iterated problem into this framework!

For computational reasons, we will focus on processes where both the states and actions are finite.
Which, in our context, means that
they are comparable, ordered and that we can enumerate all their elements.
To ease our life later on,
let's also assume this enumeration to be strictly sorted,
which is always feasible for finite sets.

> class (Eq a, Ord a) => Finite a where
>   elems :: [a]   -- WARNING: required to be sorted

:::Aside

The Game of War
---------------

In order to illustrate the concept,
let's introduce a small decision problem written in the language above.
As I want to make an impact,
it will involve the goriest and deadliest among all decisions: whether to wage war.[^war]

[^war]: Please do not take these examples as a basis for any serious business.
Also, please don't start any war.

Suppose you're a happy king running some faraway a country.
Unfortunately, for historical reasons, your country has terrible relations with the neighbour,
and you keep bouncing between war declarations and peace treaties.
Considering this context, at the first of each year
you summon the country's counsel of sages to decide if this will be a year of fighting or truce.
This defines the available actions for both countries.

> data Choice = Peace | War
>   deriving (Eq, Ord, Show)

In order to make a choice,
the counsel recalls the state of affairs for the past year,
comprised by the choices of both you and the enemy.

> type Affairs = (Choice, Choice)
> --                /\      /\                --
> --------------   you    enemy   --------------

We can enumerate both of those,
since they have a finite number of elements.

> instance Finite Choice where
>   elems = [Peace, War]
>
> instance (Finite a, Finite b) => Finite (a, b) where
>  elems = [(a, b) | a <- elems, b <- elems]

The transition of states from one year to the next
ignores the previous state of affairs and construct a new one
from both parties choice of action.

> nextGame :: Affairs -> Choice -> Choice -> Affairs
> nextGame _ a b = (a, b)

From the above,
we see that the monadic context is a function type `m s = Choice -> s`
representing the actions available to the adversary.

Now, the most delicate part: the costs of war.
Since this is just for illustrative purposes,
let's just fabricate some numbers to represent it.
The current cost will depend on the cumulative cost from past year's
plus a possible action cost for raising a military in order to wage war.

> costGame :: Affairs -> Choice -> Double
> costGame s a = costPast s + costDecision a
>  where
>   costPast (Peace, Peace) =   0  -- No fighting, no costs
>   costPast (Peace, War)   =   2  -- Being attacked unarmed destroys your country
>   costPast (War,   Peace) =  -2  -- Negative cost = reward from looting the other country
>   costPast (War,   War)   =   1  -- Keeping war has a cost to everyone
>   costDecision Peace =    0  -- You don't need to do anything
>   costDecision War   =    1  -- Raising a military, training etc.

From this data we can define a decision process
representing the game of war which will accompany us throughout the post.
We choose a discount of `0.75` to indicate that the kingdom cares more about today
than with the future.

> gameOfWar :: Process ((->) Choice) Affairs Choice
> gameOfWar = Process { next = nextGame, cost = costGame, discount = 0.75}

:::


A Policy to Follow
------------------

In formal languages,
the equivalent to the action set is an alphabet `a`
and the way to control the dynamics is to read a list of tokens in `[a]`.

> run :: Monad m => Process m s a -> [a] -> s -> m s
> run Process{..} = flip (foldlM next)

For decision process, on the other hand,
we usually want to implement a _policy_
which decides the action to take based on the current state.
To keep up with all the uncertainty theme, we will allow the policies to return monads of actions.
Deterministic policies can always be represented as non-deterministic ones via `pure` / `return`.

> type Policy m s a = s -> m a

In case you find it strange to allow monadic actions,
let's think about some "concrete" cases where it might make sense.
When working with Markov Decision Processes, for example,
it is customary to have probability distributions over actions,
in order to better model navigating in an uncertain world.
Another example are stochastic games,
where the monad represents actions inaccessible to you,
but that your adversaries can execute (`m = Reader Enemy`).
Then, it is useful to model policies taking into account what the enemy.

With a policy at hand,
one generally wants to use it to simulate the system's dynamics.
You can, for example, use it to to get an action-independent transition function.

> follow :: Monad m => Process m s a -> (s -> m a) -> (s -> m s)
> follow p policy x = policy x >>= next p x

Did you see how it transforms a choice of actions into a choice of states?
Well, now that we know how to transition states,
we can run this transition indefinitely into a simulation.
The idea is to start at some initial state and keep asking the policy
what to do, generating an infinite walk over the state space.

> simulate :: Monad m => Process m s a -> (s -> m a) -> s -> [m s]
> simulate p policy s = iterateM (follow p policy) (pure s)
>  where
>   iterateM f x = x : iterateM f (f =<< x)

:::Aside

For our example game,
a policy is a function `Affairs -> Choice -> Choice`
indicating that you take into account both the past
and your enemy's choice in order to choose what to do.
Some examples of policies consist of:
- Always keep peace or wage war, no matter what is happening.
- Ignore the past and copy whatever the enemy does right now.
- Try to keep peace but retaliate whenever someone declares war,
now or in the past.

> polPeace _ _ = Peace
> polWar   _ _ = War
> polCopy  _ a = a
> polRetaliate _        War = War
> polRetaliate (_, War) _   = War
> polRetaliate _        _   = Peace

We can them follow along a policy for `n` steps
by simulating the game and mapping the enemy's actions to a list of possible futures.

> simulGame pol initial n = take n (map possibilities steps)
>  where
>   steps = simulate gameOfWar pol initial
>   possibilities f = map f elems

As an example, let's go on ghci and follow the retaliating policy for a couple steps.
We will start at a state where our country declared war while the adversary wants to remain in peace.
As you can see, after the initial step, this policy only traverses states where both sides agree.

```ghci
ghci> :{
ghci| let states = simulGame polRetaliate (War, Peace) 5
ghci| in for_ (zip [0..] states) $ \ (i, s) ->
ghci|   putStrLn $ show i ++ ": " ++ show s
ghci| :}
0: [(War,Peace),(War,Peace)]
1: [(Peace,Peace),(War,War)]
2: [(Peace,Peace),(War,War)]
3: [(Peace,Peace),(War,War)]
4: [(Peace,Peace),(War,War)]
```

:::

Every policy has a cost
-----------------------

Up until now,
we've only considered the dynamics generated by a `Process`.
But what about its cost?
The function `cost` only tells us the cost of a single decision,
which is only part of the story.
What we need to know is the total cost of following a policy.

Since we consider nondeterministic transitions,
we need a way to evaluate real-valued functions on monadic states.
This is, in general, problem dependent and will be represented
as a further property our monad must satisfy.

> class Monad m => Observable m r where
>   collapse :: (s -> r) -> m s -> r

The above is similar to the `Context` class
[from the previous post on finite automata](/posts/automata-monads),
just parameterized in `r` instead of `Bool` (We will only use `r ~ Double` today).
Perhaps there is something more profound in all of this,
but I personally don't know.
I have also just noticed we can view `flip collapse`
as an arrow from `m` to the continuation monad `(s -> r) -> r`.
Again, I have no idea if there is something deeper going on,
but I am curious.
If you have any insights, please send them!

Well, back to calculating costs.
With an eye on future sections,
the next thing we will define is the _total cost_
of a process given a future estimate of costs `v :: s -> r`.
Keep in mind that this is the most important function we will encounter on this post.
It takes an estimate for the costs of future states, a state, and an action
and turn it into a deterministic cost.

> totalCost :: (Observable m r, r ~ Double)
>           => Process m s a -> (s -> r) -> s -> a -> r
> totalCost Process{..} v s a = cost s a + discount * collapse v s'
>  where s' = next s a

From the total cost,
we can get the cost of following a policy
by substituting the action for the policy's choice.
Since it is an infinite process,
let's just follow the policy for `n` steps.
The term we leave behind has order `O(discount ^ n)`,
so you can calculate an appropriate `n` for your tolerance.

> policyCost :: (Observable m r, r ~ Double) => Int -> Process m s a -> (s -> m a) -> s -> r
> policyCost 0 _ _ _ = 0
> policyCost n p policy s =
>   let v = policyCost (n-1) p policy
>   in collapse id $ fmap (totalCost p v s) (policy s)

The procedure above will give you the right answer,
although it can be considerably slow for certain kinds of non-determinism.

:::Aside

What should be the `Observable` instance for the game monad `(->) Choice`?
The common idea in game theory is to assume the worst:
prepare for a future where the enemy takes the action incurring the greatest cost to us.

> instance Ord r => Observable ((->) Choice) r where
>  collapse v f = maximize (v . f)

We define the `maximization` function some paragraphs below.

By taking one of our previously defined policies,
we can see how much we expected following it will cost the reign.

```ghci
ghci> policyCost 10 gameOfWar polRetaliate (Peace, Peace)
6.549491882324219
```

:::


Best policies and value functions
=================================

In real-world applications,
we generally don't want to just simulate a system.
We rather want to optimize it to find the best policy possible.
And with best, I mean the one with the least total cost.

Since our state and actions spaces are finite,
one way would be to just enumerate all possible policies
and search for the best among them.
Well, even for a deterministic system there are exponentially many (`|a|^|s|`)
policies, and with a monadic context it could grow even more.
If you have the entire age of the universe at your disposal,
you can try the method above.
But I'm generally in a hurry and, thus, need a better method.

Among the main insights in [dynamic programming](/posts/dynamic-programming)
is that we can find the optimal policy by looking at the system's value function.
Consider $v^\star$, the total cost of starting at any initial state and following an optimal policy thereafter.
With some algebraic manipulations on `totalCost`,
you can deduce that the optimum
satisfies a recursive relation called the _Bellman equation_:

$$
  \begin{array}{rl}
   v^\star(s) =
    \min\limits_{a} & \mathrm{cost}(s, a) + \gamma \rho(v^\star, s') \\
    \textrm{s.t.}  & s' = \mathrm{next}(s, a).
  \end{array}
$$

In order to solve the Bellman equation,
let's introduce optimization of functions into our toolkit.
Using that our types are `Finite`,
we can optimize over them via sheer brute force.

> minimize :: (Finite a, Ord r) => (a -> r) -> r
> minimize f = minimum (fmap f elems)
>
> maximize :: (Finite a, Ord r) => (a -> r) -> r
> maximize f = maximum (fmap f elems)

After finding the optimal value function,
we can extract a deterministic policy from it
by solving the optimization problem for each state separately.
We can use the ultra practical `Arg` type from `Data.Semigroup`
to minimize while keeping track of the optimal point.

> extract :: (Observable m r, Finite a, r ~ Double)
>         => Process m s a -> (s -> r) -> (s -> m a)
> extract p v s = let (Arg vs a) = minimum [ Arg (totalCost p v s a) a | a <- elems ]
>                 in pure a

Value Iteration
---------------

We will solve the Bellman equation using a method called **Value Iteration**.
It works by viewing the recursive relation as the fixed point of a functional operator
and using standards tools (which we will shortly define) to solve it.
Let's thus write a function converting a decision process to such operator.

> bellman :: (Observable m r, Finite a, r ~ Double)
>         => Process m s a -> (s -> r) -> (s -> r)
> bellman p v s = minimize (totalCost p v s)

For seasoned Haskellers,
fixed points mean a function that is as useful as it is simple:

> fix :: (t -> t) -> t
> fix f = let x = f x in x

Unfortunately for us,
jumping too fast into it may mean falling prey
to the pitfall of partial functions.
Because, even though `fix . bellman` type checks correctly,
it results in an infinite loop.
The function `fix` only really shines when dealing with some kind of lazy structure,
or when the recursion has a base case.
How can we write a solver for the Bellman equation then?

Thankfully,
the folks working in Analysis have already solved our problem
with the more than famous[^banach-famous]
[Banach Fixed Point Theorem](https://en.wikipedia.org/wiki/Banach_fixed-point_theorem).
It states that in a metric space (somewhere we can measure distances)
and under mildly assumptions,
iterating a function places us arbitrarily close to its fixed point.

Let's, thus, define a class for metric space
and an appropriate instance for finite functions using the uniform norm.

[^banach-famous]: Famous among us mathematicians, of course.

> class Metric x where
>   dist :: x -> x -> Double
>
> instance (Finite s) => Metric (s -> Double) where
>  dist f g = maximize $ \s -> abs (f s - g s)

Time for the fixed point iteration method!
To calculate the solution, we start at any $x_0$
and produce a converging sequence by turning the fixed point equation into an update rule.

$$ x_{n + 1} = f(x_n). $$

Similarly to `fix`,
this will build a chain of compositions $f \circ f \circ f \circ f ...$.
The main difference is that Banach's theorem
lets us stop as soon as the results become close enough.

> fixBanach :: Metric x => Double -> x -> (x -> x) -> x
> fixBanach tol v0 f =
>   let v = f v0
>   in case compare (dist v v0) tol of
>     LT -> v
>     _  -> fixBanach tol v f

In dynamic programming,
value iteration finds the optimal value function
by using the method above on `bellman`.
By the fixed point theorem,
it converges to the optimal whenever
the discount factor is less than 1.

> valueIteration :: (Observable m r, Finite s, Finite a, r ~ Double)
>                => Double -> Process m s a -> (s -> m a, s -> r)
> valueIteration tol p =
>   let ovf = fixBanach tol (const 0) (bellman p)
>   in (extract p ovf, ovf)

Ok folks, we've solved our problem. Good night for everyone.
Although, think about it... the method above feels painfully slow.

:::Aside
Try to run `valueIteration 1e-6 gameofWar` and see what happens.
Go there, I am waiting --- and, in fact, I will keep waiting,
because after running the algorithm for hours on my notebook,
there as still no sign of convergence.
:::

Where is the tabulation?
------------------------

Dynamic programming is known for its use of tabulating solutions
for small parts of a problem in order to speed up larger ones.
The method above, nevertheless, only iterates functions.

The main problem is that `bellman p v`
builds a closure that must evaluate `v`
whenever it is evaluated.
Since the fixed point is formed by compositions of `bellman p`,
it has to go through a chain of minimizations every time we want to evaluate it.
Now imagine how slow it is to go through all elements when calculating
the function's distance!

A [classical technique for memoization](/posts/representable-memoize)
involves substituting a function by a representable functor
because of its faster indexing.
Let's introduce the class of `Representable` functors.

> class Functor f => Representable f where
>  type Key f :: Type
>  tabulate   :: (Key f -> a) -> f a
>  index      :: f a          -> (Key f -> a)
> -- Class laws:
> -- index    . tabulate = id
> -- tabulate . index    = id

What the class means is that `f`
is isomorphic to a function type with a fixed domain `Key f`.
Hence, we can freely switch between them
whenever one is more appropriate than the other.

The trick to memoization in Haskell is to instead of evaluating a function,
we create a new function that indexes a data structure tabulating it.

> rep :: forall f r s. (Representable f, s ~ Key f)
>     => ((s -> r) -> (s -> r)) -> (s -> r) -> (s -> r)
> rep g = index @f . tabulate . g

Notice that, by the class laws, `rep g` is semantically equivalent to `g`.
Nevertheless, they have very different runtime behaviours.
With this method, we can rewrite `valueIteration` to use a representation instead of the function itself.

> valueIterationRep :: forall f m s a r
>                   .  (Representable f, s ~ Key f
>                   ,   Observable m r, Finite s, Finite a, r ~ Double)
>                   => Double -> Process m s a -> (s -> m a, s -> r)
> valueIterationRep tol p =
>   let ovf = fixBanach tol (const 0) op
>   in (extract p ovf, ovf)
>  where op = rep @f (bellman p)    -- Only change

What I like the most about the implementation above
is that the data structure used to tabulate
is completely orthogonal to the algorithm per se.
We can choose it based on the problem at hand
without having to change any of the algorithm's internals.

:::Aside
With a proper representation, value iteration finally converges for the game of war.
Since there are only 4 possible states, we can represent  `Affairs -> r` by a tuple.

> data GameRep r = GameRep r r r r
>   deriving (Show, Functor)

The `Representable` instance is pure and simple bookkeeping.

> instance Representable GameRep where
>  type Key GameRep = Affairs
>  tabulate f = GameRep (f (Peace, Peace))
>                       (f (Peace, War))
>                       (f (War,   Peace))
>                       (f (War,   War))
>  index (GameRep a _ _ _) (Peace, Peace) = a
>  index (GameRep _ b _ _) (Peace, War)   = b
>  index (GameRep _ _ c _) (War,   Peace) = c
>  index (GameRep _ _ _ d) (War,   War)   = d

By running `valueIterationRep @GameRep 1e-6 gameofWar`,
you can see that the optimal solution for the game of war
is to pursue peace no matter what! Now that's inspiring! [^ignore-me]

[^ignore-me]: Inspiring until you realize that it becomes war at all costs
by doing a small tweak in the cost function.
I guess I was luck with my choice of weights... Well, it costs nothing to dream.

:::

Can't we just use vectors?
--------------------------

Despite its elegance (at least for me),
the solution above unfortunately
throws the burden of choosing an appropriate representation
to whoever is running the algorithm.
We gotta admit that this last bit isn't very courteous from our part.
For the sake of good measure,
let's at least provide some default container our user can plug in the algorithm
and still get more tabulation than directly using functions.

If we were implementing value iteration in Fortran instead of Haskell,
our default data structure would be arrays instead of functions,
and that would be a great choice!
Fortunately for us, it is always possible to represent functions over finite domains as vectors.
Let's construct a wrapper type for this representation.

> newtype AsVec (s :: Type) r = AsVec (V.Vector r)
>   deriving (Show, Functor)

The representable instance is basically bookkeeping.
Since `elems` orders the states, it implicitly defines a (partial) mapping
`Int -> s`, so we can tabulate by mapping over it [^yoneda].
In the other direction, we can also extract a mapping `s -> Int`
in order to index the vector with elements of `s`.

> instance Finite s => Representable (AsVec s) where
>  type Key (AsVec s) = s
>  tabulate f         = AsVec $ V.fromList (map f elems)
>  index (AsVec xs) s = xs V.! (melems M.! s)

[^yoneda]: For you category theorists out there,
the value `AsVec (V.fromList elems)` represents
the isomorphism between Hom and our functor.

In order to do the indexing,
we needed a way to convert from states to integers.
A one-size-fits-all method is to keep track of this relation
is to construct a `Map s Int` from the ordered list `elems` of states.

> melems :: Finite s => M.Map s Int
> melems = M.fromAscList (zip elems [0..])

Notice, though, that the solution above is not optimal
because of its `O(log |s|)` cost for accessing each index.
When you know your type well-enough,
it is generally possible to come up with a constant time `index` instance.
One example would be if `s` had a `Ix` instance.
For the purposes of this post, the above is good enough,
but just know that you could do better when specializing for a known type.

With the instance above,
we can construct a version of value iteration
which, no matter the decision process, represents the value function with vectors.

> valueIterationVec :: forall m s a r
>                   .  (Observable m r, Finite a, Finite s, r ~ Double)
>                   => Double -> Process m s a -> (s -> m a, s -> r)
> valueIterationVec = valueIterationRep @(AsVec s)

Although vectors are a natural choice for their constant indexing,
the definition above doesn't use it fully because it has to index `melems`.
Since we already have most of the structure done,
another reasonable default would be to use a `Map` directly.

> newtype AsMap (s :: Type) r = AsMap (M.Map s r)
>   deriving (Show, Functor)
>
> instance Finite s => Representable (AsMap s) where
>  type Key (AsMap s) = s
>  tabulate f = AsMap $ M.fromList [(s, f s) | s <- elems]
>  index (AsMap xs) s = xs M.! s

Adieu
=====

We thus conclude today's exploration.
Although the code above for Value Iteration isn't the fastest in the west,
it has its merits on separating the algorithms concerns.
From it, you could start improving the parts separately by adding stuff such as
parallelism, unboxed storage, or using `ST` to reduce allocations.
Nevertheless, the spirit of the algorithm is already entirely in here!
