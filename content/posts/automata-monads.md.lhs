---
title: Exploring Automata with Monads
keywords: [haskell, automata]
date: 2023-10-13
---

Recently, I have been brushing up my knowledge of Formal Languages
and stumbled into the many different faces of finite automata.
And what is a better way to review then to write some Haskell?

One thing about formal languages that always itches me out
is how similar some definitions are to one another.
Most materials present DFAs, NFAs and company
as distinct concepts with their own properties and theorems.
Well, if you read my posts, you probably already noticed
that how much I like generic algorithms.
Let's, therefore, explore the world of finite automata
with an eye into unifying them.

> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE DuplicateRecordFields #-}
> import Data.Foldable          (Foldable(foldl'), foldlM)
> import Control.Monad.Identity (Identity)

Deterministic Finite Automata
=============================

For a start, we will consider the simplest family of them all,
deterministic finite automata.
Their mathematical definition is rather dry and goes something like this.

:::Definition
A **deterministic finite automaton** is a 5-tuple $(S, A, \delta, s_0, T)$
where

- $S$ is a finite set of _states_;
- $A$ is a finite set of symbols called the _alphabet_;
- $s_0$ is the _initial state_;
- $T \subset S$ is a set of _accepting states_.
- $\delta : S \times A \to S$ is the _transition function_;
:::

Ok, although the above definition is standard, it is also dryer than the Atacama.
Besides, I nerver quite understood Computer Scientists' gusto for defining everything in terms of n-tuples.
Its advantage, however, is in how easy it is to translate into a programming language.
The finite sets become types, the subset relation becomes a predicate
and our 5-tuple becomes a product type.

> -- | Deterministic Finite Automaton with states `s` and alphabet `a`.
> -- The type parameters are assumed to represent finite sets.
> data DFA s a = DFA
>   { initial    :: s
>   , accepting  :: s -> Bool
>   , transition :: s -> a -> s
>   }

Despite this algebraic definition,
my intuition (and I suppose everyone else's) for automata is as a dynamical system.
It starts at the initial state and, as the user feeds it characters in the alphabet,
it accordngly transitions from state to state until the input ends.
This represents a program that one can run in order to achieve some final state.

> runDFA :: DFA s a -> [a] -> s
> runDFA DFA{..} = foldl' transition initial

When the dynamics ends, we can test if it arrived at one of the accepting states.
In the affirmative case, we say that this automaton recognizes the input word.
A large chunck of automata theory consists of idnetifying which
automata can recognize all words on which languages.

> recognizeDFA :: DFA s a -> [a] -> Bool
> recognizeDFA aut@DFA{..} = accepting . runDFA aut

Each automaton represents a program that one can run
by feeding it a word in its alphabet,
and the automaton will tran

The transition function $\delta$ defines a graph with nodes in $S$
and edges labeled by $A$.

Many Flavours of Automata
=========================

> -- Finite automaton with state s, alphabet a
> -- and a monadic context m.
> data Automaton m s a = Automaton
>   { initial    :: s
>   , accepting  :: s -> Bool
>   , transition :: s -> a -> m s
>   }

> run :: Monad m => Automaton m s a -> [a] -> m s
> run Automaton{..} = foldlM transition initial

> recognize :: (Monad m, Accepting m) => Automaton m s a -> [a] -> Bool
> recognize aut@Automaton{..} = exists accepting . run aut

> class Accepting m where
>  exists :: (a -> Bool) -> (m a -> Bool)

> type FDA  = Automaton Identity
> type PDFA = Automaton Maybe
> type NFA  = Automaton []

