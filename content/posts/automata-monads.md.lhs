---
title: Putting Automata into Context
keywords: [haskell, automata]
date: 2023-10-13
---

\def\powset{\mathcal{P}}

Recently, I have been brushing up my knowledge of Formal Languages
and stumbled again into the many different faces of finite automata.
Most materials present DFAs, NFAs and company
as distinct beasts, each with their own properties and theorems.
Nevertheless, I couldn't get out of my mind that all definitions seemed _too similar_.
After hitting my head into a couple walls, I finally noticed a link!
Each kind of finite automaton has a dynamics that runs in a certain context,
or equivalently, that is able to apply certain effects while it executes.

The cooler part is that the way you write these contexts
is exactly the same as you would do in a Haskell program: Monads.
Even more, one models things such as nondeterminism or partiality
using the exact same monads as in real life code.

Depending on how used you are to these things,
this last statement can be either obvious or shocking.
Well, I can't say it for you.
But for me, it was a rather interesting finding.

> {-# LANGUAGE RecordWildCards #-}
> import Data.Foldable          (foldlM)
> import Control.Monad.Identity (Identity, runIdentity)

One Definition to Rule Them All
===============================

Since this post is about abstraction,
let's start with a general definition.
We will closely follow the [standard definition for a finite automaton](https://en.wikipedia.org/wiki/Finite-state_machine#Mathematical_model)
with one twist: the transition function is parameterized for a certain context.

> -- | Finite automaton with state `s`, alphabet `a` and a monadic context `m`.
> --   The type parameters `s` and `a` are assumed to represent finite sets.
> data Automaton m s a = Automaton
>   { initial    :: s               -- ^ Initial State
>   , transition :: s -> a -> m s   -- ^ Change state with a context.
>   , accepting  :: s -> Bool       -- ^ Accepting subset as a predicate.
>   }

The type above is a simple model of computation
written as a controllable dynamical system.
Given a string of characters in the alphabet,
it starts in the `initial` state and,
for each character follows the appropriate `transition`
into the next state.
The context `m` is a monad that represents what effects our automaton is able to perform.
This way we can run the automaton by consuming the input characters until we arrive in the final state (in context).

> run :: Monad m => Automaton m s a -> [a] -> m s
> run Automaton{..} = foldlM transition initial

The function `foldlM` is just Haskell's way of repeatedly iterating
a transition from the initial state.
Notice that in the above we are using `RecordWildCards`
to put the `Automaton`'s in scope.

By switching the Monad `m`, we recover different families of automata.
For example, **Deterministic Finite Automata** use the `Identity` Monad
because their transition is an ordinary function.

> -- | Deterministic Finite Automaton.
> type DFA  = Automaton Identity

On the other hand, for **Nondeterministic Finite Automata**,
we can use the Haskell idiom of modeling power sets using Lists.
This way, the transition has type `s -> a -> [s]` and represents a relation between states.

> -- | Non-deterministic Finite Automaton.
> type NFA  = Automaton []


Automata Theory is all about languages.
Thus, given an automaton, one is generally interested in using it to match or recognize input strings.
To do that, we will need our remaining field `accepting`.
If after running the automaton, it ends in an accepting state, we say that it recognizes the input.


> recognize :: (Monad m, Context m) => Automaton m s a -> [a] -> Bool
> recognize aut@Automaton{..} = possible accepting . run aut

In order to complete the above we must define the `possible` function.
By inspecting its type,

    possible :: (s -> Bool) -> (m s -> Bool)

We see that it lifts a predicate about a state into a predicate about a monadic state.
In the case of all our examples,
it will be equivalent to being possible that the final state is an accepting one.
Hence its name.

Since the `possible` function depends on the chosen Monad,
we model it using a typeclass.

> class Context m where
>  possible :: (a -> Bool) -> (m a -> Bool)


Many Flavours of Automata
=========================

Now that we are armed with the code for recognizing words,
it is time to explore different automata.

Let's begin with our old friends `DFA` and `NFA`
and then proceed to their more exotic cousins.

Deterministic
-------------

These are the simplest automata of them all
because, after running them, we have a single unambigous state.
To test whether it is accepting, all we have to do is checking it.

> instance Context Identity where
>  possible pred = pred . runIdentity

Nondeterministic
----------------

These automata arrive at a whole list of states.
We consider them to accept a word if any of the final states is accepting.

> instance Context [] where
>  possible pred = any pred

Altough we aren't using parallel programming in here,
you can think of these automata as following many sequences of states in parallel
and, in the end, it recognizes the word if any sequence arrived at an accepting state.

Partial Transitions
-------------------

An advantage of parameterizing the return context is that we are able to plug other Monads
to get their effects on the automaton transition.
For example, in a DFA any state is required to have a valid transition
for any character in the alphabet.
This can lead to some peculiar modeling where an automaton
must have a lot of characters transitioning to some error state.
Wouldn't it be better to allow the state machine to fail
whenever it encounters a character that is invalid to the current state?

Well, if you are in any way used to Haskell,
you should know what Monad models a computation that maybe happens.

> -- | Partial Deterministic Finite Automaton.
> type PDFA = Automaton Maybe
>
> instance Context Maybe where
>  possible pred Nothing  = False
>  possible pred (Just s) = pred s


Probabilistic Finite Automata
-----------------------------

Let's get a bit less classical with [Probabilistic Finite Automata](https://en.wikipedia.org/wiki/Probabilistic_automaton).
These are similar to non-deterministic automata but
also quantify the system's chance of taking a certain transition.

Since Haskell does not have a built-in probabilistic programming,
we have to first define a Monad that represents probability distributions.
In order to not lose ourselves into a tangent,
we will write the simplest probability monad possible
without much worry about performance or flexibility.
But know that there are production grade probability programming libraries in Haskell,
such as [monad-bayes](https://hackage.haskell.org/package/monad-bayes).

We represent a probability distribution as a list of outcomes paired to their respective probabilities.

> data Dist p a = Dist [(a, p)]
>   deriving (Functor, Show)
>
> listProb (Dist x) = x

With an eye on some further applications,
we are also parameterizing it on the probability's type.
Nevertheless, for now we are interested on Real probabilities.

> -- Probabilities over R
> -- The coefficients are assumed to be in [0, 1] and to sum to 1.
> type Prob = Dist Double

It has Applicative and Monad instances representing,
respectively, the interaction of independent and dependent random variables.

> instance Num p => Applicative (Dist p) where
>  pure x = Dist [(x, 1)]
>  Dist fs <*> Dist xs = Dist [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]
>
> instance Num p => Monad (Dist p) where
>  Dist xs >>= g = Dist [(y, p * q) | (x, p) <- xs, (y, q) <- listProb (g x)]

Finally, since we are assuming everything is finite,
we can model events as predicates.
The probability of an event happening is the sum of the probabilities
for all outcomes for which the event holds.

> prob :: (a -> Bool) -> Prob a -> Double
> prob pred = sum . fmap snd . filter (pred . fst) . listProb

Great! Despite being a bit crude,
the above should be enough for our stochastic ambitions in this post.
Also remember that we are glossing over a lot of details in here.
So it is completely reasonable to not understand everything above.

Our Probabilistic Finite Automaton is then an automaton whose contexts
are probability distributions.
That is, each transition is non-deterministic and uncertain:
you don't know for which state it transitions but know the chance for each one.

> -- | Partial Deterministic Finite Automaton.
> type PFA = Automaton Prob

Given a probability distribution on the final states,
we consider that it is possible to be accepting if there is a non-zero probability
to accept the input string.

> instance Context Prob where
>  possible pred m = prob pred m > 0


Going Quantum
-------------

- Quantum http://blog.sigfpe.com/2007/03/monads-vector-spaces-and-quantum.html
