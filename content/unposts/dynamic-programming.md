---
title: A Tale of Dynamic Programming
keywords: [dynamic-programming, markov-decision-processes, reinforcement-learning]
date: 2022-05-30
---

\def\States{\mathcal{S}}
\def\Actions{\mathcal{A}}

What if I told you that some of the most used algorithms to
find the shortest path in a graph,
calculate gradients while training a neural network,
and parse context-free grammars
are essentially implementations of the same idea?
Namely: _dynamic programming_.

I gotta say that I was taught dynamic programming
in many different contexts but it took me a while
to finally get "click" that it they were actually the same thing.
When learning algorithms and data structures,
it was a memoization-based technique were you speed up your algorithm
by first solving the easy parts and saving them for later use.
At work, I mostly deal with solving a lot of linear programs
for long-term scheduling problems.[^sddp]
The main algorithm we use, called _Stochastic Dual Dynamic Programming_,
at first didn't seem so much like the programming technique algorithms class.
Finally, one of the main methods for model-based reinforcement learning
is again called dynamic programming,
and at first it also didn't seem so much like the others instances.

So, what is happening here?
Did everybody choose to call their algorithms dynamic programming
just because it is a cool name?[^dp-name]
Well, in fact there are some principles that apply to all of those instances,
from planning a rocket's trajectory to TeX's word-wrapping.
And the [list goes on and on](https://en.wikipedia.org/wiki/Dynamic_programming#Algorithms_that_use_dynamic_programming).


In this post,
I intend to write about the framework
that encompasses all those problems and how DP fits into it.
The motivation for this came some weeks ago after teaching a colleague
about how can we start with the programming technique and arrive at SDDP.
This post is in fact a more organized/legible version of my notes and diagrams from that day.
By the way, I also recommend reading
Richard Sutton's [The Quest for a Common Model of the Intelligent Decision Maker](https://arxiv.org/pdf/2202.13252.pdf).
It is a great paper about unifying decision frameworks across different fields,
and certainly also inspired this post.

[^sddp]: To be more precise, we work with hydrothermal dispatch problems,
where one must decide between many sources of energy (hydro, thermal, renewable)
to supply a certain power demand taking into account the uncertainties of the future.
For example: hydro is cheap and clean but you risk running out of water
if you use all of it and next month turns out particularly dry.
Finding the best energy dispatch is once again solved via dynamic programming.

[^dp-name]: Even Richard Bellman admittedly [chose because of that](https://en.wikipedia.org/wiki/Dynamic_programming#History).

## On Decision Making and State Machines
Before delving into dynamic programming, we first have to establish a few concepts.
After all, it's always best to know which problems you intend to solve
before learning a method to solve them, right?


Let's begin by talking about _controllable state machines_
or _automata_ if you're into Greek words.
Consider a system that can be in one of many _states_ $s \in \States$.
At each state, you can choose among a set of _actions_ $a \in \Actions(s)$
that transition the system to a new state $s' = T(s, a)$,
where $T$ is called the system's _transition function_.
Usually, such objects are represented by graphs such as the one below.


An important notice: If you come from Computer Science,
you are probably used to finite state machines.
But in our context here, the states may be any set.
Some algorithms that we will see only work if the state space is finite
but others may even require a continuous space!
An example is SDDP, which uses linear programming duality
and thus requires the state space to be a polyhedron in $R^n$.

### Decision Processes

Unfortunately nothing in life comes for free...
And taking an action $a$ in state $s$ has a certain cost $c(s, a)$.
Our simplest decision problem then takes the following format:

> What is the best action to take at state $s$?
That is, which one produces the lowest cost?

Or in math language:

$$
\begin{array}{rl}
  \min\limits_{a} & c(s, a) \\
  \textrm{s.t.}  & a \in \Actions(s).
\end{array}
$$

This is an optimization problem and can be solved by standard techniques,
depending on the nature of the cost function and action set.
If $\Actions(s)$ is finite, one uses combinatorial optimization.
Are they all linear? Linear programming to the rescue.
Convex? Just use convex programming.
I think you get it.

Nevertheless, there is still something missing on the description above.
We used the initial state $s$ and the cost function $c$
to decide which is the best action to take.
But what about the transition function?
I wouldn't introduce it if there was no good use...

In the real world, it is not so common to just want to take an action.
After all, what we do now affects the future.
When we choose our action $a$,
our system will arrive at a new state $T(s, a)$.
Thus we can ask again the same question:
what is the best action to take at this new state?
What we call a _decision process_ is a sequence of actions
whose total cost is the minimum possible over time.
Starting at state $s$,
our decision is a solution to the following optimization problem:

$$
\begin{array}{rl}
  \min\limits_{a} & \sum\limits_{t=1}^\infty \gamma^{t-1}c(s_t, a_t) \\
  \textrm{s.t.}   & s_{t+1} = T(s_t, a_t), \\
                  & s_1     = s, \\
                  & a_t \in \Actions(s_t).
\end{array}
$$

The $\gamma \in [0, 1]$ is a constant called the _discount factor_.
Its use is twofold.
Mathematically, it is useful because
if we have $\gamma < 1$ and the costs are bounded,
we can guarantee that the series converge.
That is, suppose that

$$\forall s \in \States, a \in \Actions(s),\, c(s, a) \le M.$$

This bounds the total cost by a geometric series that cannot blow up,

$$
\sum\limits_{t=1}^\infty \gamma^{t-1}c(s_t, a_t) \le \sum\limits_{t=1}^\infty \gamma^{t-1} M \le \frac{M}{1 - \gamma},
$$

thus guaranteeing that the decision problem is well-posed.

Besides that, there is also a more applied interpretation of $γ$
that is also interesting:
It says that spending in the future in cheaper than expending right now.
So, in an economic context, the discount $γ$ is the same as inflation.
Similarly, if we are programming a robot that gains rewards
(equivalent to negative costs) at each time step,
the discount factor amounts to impatience:
it is better to gain the reward sooner than later.


#### State over time

# Dynamic Programming

Finite, Interpolation, Computeiro

## Example: Shortest Path in a Graph

## Example: Backpropagation

# Stochastic Dynamic Programming

## Markov Decision Processes

# SDDP

## Approximations by cuts

## Stagewise independence


https://arxiv.org/pdf/2202.13252.pdf

https://www.mit.edu/~dimitrib/allerton_api_final.pdf
