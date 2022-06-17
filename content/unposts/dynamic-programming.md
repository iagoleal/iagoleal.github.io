---
title: A Tale of Dynamic Programming
keywords: [dynamic-programming, markov-decision-processes, reinforcement-learning]
date: 2022-05-30
---

\def\States{\mathcal{S}}
\def\Actions{\mathcal{A}}
\def\R{\mathbb{R}}
\def\E{\mathbb{E}}

What if I told you that some of the most used algorithms to
find the shortest path in a graph,
calculate gradients while training a neural network,
and parse context-free grammars
are essentially implementations of the same idea?
Namely, _dynamic programming_.

I gotta say that I was taught dynamic programming
in many different contexts but it took me a while
to finally get "click" that it they were actually the same thing.
When learning algorithms and data structures,
it was a memoization-based technique were you speed up your algorithm
by first solving the easier parts and saving them for later use.
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

I want to invite you to a journey through many realms of mathematics.
We will range from automata to optimal control,
passing through Markov chains, dynamical systems, linear programming
and even metric spaces.
Take your seat and enjoy the ride!


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

As a matter of motivation, let's begin by considering a vampire.
It is common knowledge[^vampire]
that vampires are always in one of four states:
their usual human form, transformed into a bat or a wolf or walking in the shadows.
In the human form, the vampire can choose to turn into one of the animals
or jump into a shadow to enter it.
As a bat, the vampire is able to keep flying or drink a person's blood,
an act that turns him back into human form immediately.
As a wolf he can similarly stay in the form or go back to human.
He is unable to remain much in the shadows,
The vampire can't remain much on the shadows and, for whatever reason,
he is only able to leave them as a wolf.
It seems really complicated to be a vampire, right?
Fortunately, the folks at the Comp Sci department
invented some nice diagrams to help our pointy-toothed friends.

```dot
digraph "Vampire State Machine" {
  rankdir=LR;
  size="8,5"

  qi [fillcolor = black shape = point];

  node [shape     = circle
        style     = "solid,filled"
        width     = 0.7
        color     = black
        fixedsize = shape
        fillcolor = "#B3FFB3"];

  H [label = "Human" shape = doublecircle];
  S [label = "Shadow"];
  B [label = "Bat"];
  W [label = "Wolf"];

  qi -> H;

  H -> B [label = "transform"];
  H -> W [label = "transform"];

  H -> S;
  S -> W;

  B -> B [label = "fly"];
  B -> H [label = "drink blood"];

  B -> H [label = "return"];
  W -> H [label = "return"];

  H -> H [label = "stay"];
  W -> W [label = "stay"];
}
```

[^vampire]: Definitely citation needed.


Our modeling of a vampire is an stance of something called
a _state machines_ or _automata_ if you're into Greek words.
There are 4 states in which the vampire can be and at each one
there is an available set of actions to take that may transition him to another state.
More abstractly,
an automaton is system that can be in one of many _states_ $s \in \States$
and at each state, you can choose among a set of _actions_ $a \in \Actions(s)$
that transition the system to a new state $s' = T(s, a)$,
where $T$ is called the system's _transition function_.

An important notice: If you come from Computer Science,
you are probably most used to _finite_ state machines.
Just know that in our context, the states may be any set.
Some of the algorithms that we will see today
only work for finite state spaces
but there are others that may even require a continuous space!
An example is SDDP, which uses linear programming duality
and thus requires the state space to be a convex subset of $R^n$.

### Decision Processes

Iterating the transition $T$ establishes a dynamics for our system
where we start at a initial state $s$ and by taking a sequence of actions
$a_1, a_2, \ldots$ we walk over the state space.

$$
\begin{aligned}
    s_1     &= s, \\
    s_{t+1} &= T(s_t, a_t).
\end{aligned}
$$

This new view makes our state machine somewhat equivalent
to a controllable dynamical system,
which is another really cool name in my opinion.

As an example, think of a game of Sudoku.
The states are the (partially numbered) boards
and the actions consist of putting a valid number
in a certain coordinate.
You start with some random board and repeatedly
place numbers until you reach a terminal state
where there are no available actions.

One can argue that since a state encapsulates
all you must know about your system in order to choose an action,
no matter if the previous history nor time step.
Indeed, if those things affect your choice,
then we can without loss of generality model our
problem as a larger system where the state also carries those information.
Thus controlling a dynamic system amounts to
a function $\pi : (s : \States) \to \Actions(s)$
which chooses a valid action for each state.
In the literature this called a _policy_,
in analogy to a government taking actions to run its state.

Unfortunately life is not known for its free lunchs
and in most systems, whenever we take action $a$ at state $s$,
there is a certain cost $c(s, a)$ to pay.
Depending on the context this can be, for example,
a real monetary cost (for economic problems),
some total distance (for planning)
or even a negative cost representing a reward.

Thus, by following a policy $\pi$,
we produce a sequence of cost $c(s_t, \pi(s_t))$
for each time step.
We could define the total cost for $\pi$
as the sum of those costs but there is an additional detail to notice.
If I gave you something and asked if you want to pay me today
or next year, which option would you prefer?
Sometimes there are factors such as inflation or interests
that make costs in the future not have the same actual value
as the costs we expend right now.
This prompts us to introduce a problem dependent _discount factor_ $\gamma \in [0, 1]$
such that the total cost for a policy $\pi$ is

$$
\begin{array}{rl}
 v^\pi(s) = & c(s_1, \pi(s_1)) + \gamma c(s_2, \pi(s_2)) + \gamma^2 c(s_3, \pi(s_3)) + \ldots \\
  \textrm{where}  & s_1     = s, \\
                  & s_{t+1} = T(s_t, a_t), \\
\end{array}
$$

The equation above defines the _value function_ $v^\pi : \States \to \R$
for a given policy $\pi$.
__Spoiler__: keep an eye on the $v^\pi$,
because later on this post we will find them to be useful tools
that are closely related to the memoization techniques
that people usually identify with dynamic programming.

Besides its practical interpretation,
the discount factor $\gamma$ also plays a significant role
from the mathematical point of view.
If $|\gamma| < 1$ and the costs are uniformly bounded
(which is the case for a finite action space, for example)
we can guarantee that the series defining $v^\pi$ converges
for any policy and initial state.
That is, suppose that exists $M > 0$ such that

$$\forall s \in \States, a \in \Actions(s),\, |c(s, a)| \le M.$$

This bounds the total cost by a geometric series that cannot blow up,

$$
\sum\limits_{t=1}^\infty \gamma^{t-1}|c(s_t, \pi(s_t))| \le \sum\limits_{t=1}^\infty \gamma^{t-1} M \le \frac{M}{1 - \gamma},
$$

thus guaranteeing that that the value function is well-defined.

### Optimal Decisions

Having multiple courses of action possible
prompts us to ask which is the best one possible.
When programming a robot to escape a labyrinth,
you want it to take the least amount of time.
When controlling a spaceship towards the moon,
it is important to guarantee that it will use the least amount of fuel.
When brawling at a bar, you want to knock out your foe
with the least injuries possible.
Thus, our problem can be naturally formulated as searching for _optimal policies_:

> Starting at state $s$, find a policy $\pi$ producing
the lowest total cost over time.

Or equivalently in math language:

$$
\begin{array}{rl}
\min\limits_\pi v^\pi(s) =
  \min\limits_{a_t} & \sum\limits_{t=1}^\infty \gamma^{t-1}c(s_t, a_t) \\
  \textrm{s.t.}     & s_1     = s, \\
                    & s_{t+1} = T(s_t, a_t), \\
                    & a_t \in \Actions(s_t).
\end{array}
$$

Right now, this may seem like a big and scary optimization problem
but in fact there is a lot of structure that we can exploit in order to solve it.
This will be the subject of the next section.
Before we continue,
let's go over a little tangent on how to formulate some classical problems
in this decision making framework.

#### Sir, I don't have all that time available...

It is a nice formalism but to be realistic, not all decisions go on forever.
I would even risk saying that most of them end after some time.
Regardless, the formalism we have developed is powerful enough
to also encompass finite-horizon decisions,
provided that we make some adjustments to our models.

Consider a process that may end at a certain point but we don't really know when.
An example would be trying to escape a labyrinth full of monsters.
If a monster catches you its game over and there are no more decisions to make.
We can model this by introducing in our model a dummy _terminal state_
(that we denote as $\blacksquare$)
where there is a single possible action: to do nothing with zero cost.

```dot
digraph "Episodic Horizon" {
  rankdir=LR;
  size="8,5"

  T [label = "" width=0.2 style=filled, color = black, fillcolor = black, shape = square];

  node [shape     = circle
        style     = "solid,filled"
        width     = 0.7
        color     = black
        fixedsize = shape
        fillcolor = "#B3FFB3"
        label     = ""];

  A -> B -> C -> T;
  A -> C -> A -> D -> T;
  A -> E -> F -> G -> T;
  F -> D;
  E -> {A F} [shape=curved];

  T:e -> T:e [constraint=false];
}
```

Another possibility are problems with a _fixed horizon_,
that is, problems taking a fixed amount $N$ of steps to end.
Since we must know in which stage we are in order to end the process,
we must consider the time step $t$ as part of the state.
Then, any state from the stage N will point towards the terminal state.
Thus, given a transition $T$ for the rest of the state,
our dynamics follows a transition defined as

$$
  \bar{T}((t, s), a) = \begin{cases}
    (t + 1, T(s, a)), & t < N \\
    \blacksquare, & t = N.
  \end{cases}
$$

When drawing diagrams for fixed horizon problems,
it is common practice to cluster the states by their stage.

```dot
digraph "State over Time" {
  rankdir=LR;
  size="8,5"

  T [label = "" width=0.2 style=filled, color = black, fillcolor = black, shape = square];

  node [shape     = circle
        style     = "solid,filled"
        width     = 0.7
        color     = black
        fixedsize = shape
        fillcolor = "#B3FFB3"
        label     = ""];

  subgraph cluster_1 {
    rank = same;
    label="t = 1";
    s;
  }

  subgraph cluster_2 {
    rank = same;
    label="t = 2";
    a2; b2;
  }

  subgraph cluster_3 {
    rank = same;
    label="t = 3";
    a3; b3;
  }

  subgraph cluster_N {
    rank = same;
    label="t = N";
    a4; b4;
  }

  subgraph cluster_ldots {
    rank = same;
    style = invis;
    k [fontsize=20 color = "#00000000" fillcolor= "#00000000" label = ". . ."];
  }

  s -> {a2 b2} -> {a3 b3} -> k -> {a4 b4} -> T;
}
```

#### Example: Shortest Path in a Graph

Suppose you are at your home town
and just received a message from friend
telling you that there are singing llamas in Cuzco, Peru, right now.
This makes you at the same time incredulous and curious,
so you just pick your favorite bike and get on the road towards Cuzco.
Unfortunately there are no direct bikeways connecting your home to Cuzco,
meaning that you will have to find a route going through other cities.
Also, there is a risk that the llamas will stop to sing at any time
and just go back to their usual behaviour of eating grass throughout the mountains.
This prompts you to decide to take the shortest path to Cuzco possible.

The above description is an instance of finding the shortest path in a graph.
In it, we represent each city by a graph node and a direct routes between two cities
as a weighted edge where the weight is the distance.
Going from home to Cuzco amounts to finding the path between those two nodes
with the smallest total distance.

The translation from this graph description
to a decision process description is quite straightforward.

* **States**: nodes in the graph.
* **Actions** at state $s$: edges going from $s$ to another node.
* **Transition**: The opposite node on the same edge.
That is, given an edge $s \to s'$, $T(s, s \to s') = s'$.
* **Costs**: $c(s, a)$ is the weight of edge $a$.

Finding the shortest path from $s$ to node $z$
is the same as setting the initial state to $s$ and making $z$
a terminal state of our dynamics.

## Dynamic Programming

Alright, its finally time to solve those decision problems.
The simplest idea could be to exhaustively search the space of all actions
trying to find the best solution.
Notice that even for finite states and horizon, this may be prohibitively expensive
since the possible candidates grow exponentially with the time steps.
Any practical method will to take into account how this class of problems
naturally breaks apart into separate stages.

Our approach will involve the famous _Bellman principle of optimality_,
which is the cornerstone of dynamic programming.
Taking Bellman's own words on his book [Dynamic Programming](https://press.princeton.edu/books/paperback/9780691146683/dynamic-programming),
it reads as:

> An optimal policy has the property that
whatever the initial state and initial decision are,
the remaining decisions must constitute an optimal policy
with regard to the state resulting from the first decision.


Alright, what does this mean?
What the principle of optimality is telling us
is that in order to calculate an optimal policy,
we should turn this iterative process of making actions and calculating costs
into a recursive procedure.
That is, taking an action puts us into a new state $s_2 = T(s_1, a_1)$
where we are again faced with the exact same problem of finding an optimal policy
but this time starting at $s_2$.
Let's see how we can exploit this idea.

Remember that we defined the value function $v^\pi$
as the total cost of following a policy $\pi$
when starting at a given state.
Let's define the _optimal value function_ $v^\star$
as the total cost of choosing the best course of action
while starting at a certain state $s$.

$$
\begin{array}{rl}
 v^\star(s) =
  \min\limits_{a_t} & \sum\limits_{t=1}^\infty \gamma^{t-1}c(s_t, a_t) \\
  \textrm{s.t.}     & s_1     = s, \\
                    & s_{t+1} = T(s_t, a_t), \\
                    & a_t \in \Actions(s_t).
\end{array}
$$

Notice in the optimization problem above
that the initial state is only ever used to choose the first action.
Later actions do not depend directly on it but only on its consequences.
This means that we can break the problem into two parts:
calculating a _immediate cost_ dependent on the initial state
and calculating a future cost dependent on all next states.

$$
\begin{array}{rl}
 v^\star(s) =
  \min\limits_{a,a_t} & c(s, a) + \left(
    \begin{array}{rl}
      \min\limits_{a_t} & \sum\limits_{t=2}^\infty \gamma^{t-1}c(s_t, a_t) \\
      \textrm{s.t.}     & s_2 = s', \\
                        & s_{t+1} = T(s_t, a_t), \\
                        & a_t \in \Actions(s_t)
    \end{array}
  \right) \\
  \textrm{s.t.} & s' = T(s, a), \\
                 & a \in \Actions(s).
\end{array}
$$

There's already some recursive structure unfolding in here!
What is still missing consists of noticing that since the sum in the future cost
starts at $t =2$, we can factor out $\gamma$.
By renaming $l = t-1$ we get

$$
\sum\limits_{t=2}^\infty \gamma^{t-1}c(s_t, a_t)
= \gamma \sum\limits_{t=2}^\infty \gamma^{t-2}c(s_t, a_t)
= \gamma \sum\limits_{l=1}^\infty \gamma^{l-1}c(s_l, a_l)
$$

By applying this in the expression for $v^\star$,

$$
\begin{array}{rl}
 v^\star(s) =
  \min\limits_{a} & c(s, a) + \gamma\left(
    \begin{array}{rl}
      \min\limits_{a_l} & \sum\limits_{l=1}^\infty \gamma^{l-1}c(s_l, a_l) \\
      \textrm{s.t.}     & s_1 = s', \\
                        & s_{l+1} = T(s_l, a_l), \\
                        & a_l \in \Actions(s_l)
    \end{array}
  \right) \\
  \textrm{s.t.}  & s' = T(s, a), \\
                 & a \in \Actions(s).
\end{array}
$$

Although this is a really big expression,
it should be straightforward to see that the expression
for the future cost is _exactly_ the optimal value $v\star(s')$
of starting the dynamics at $s' = T(s, a)$.
This way, the principle of optimality express itself mathematically
as a recursive equation that the value for an optimal policy must satisfy.


$$
\begin{array}{rl}
 v^\star(s) =
  \min\limits_{a} & c(s, a) + \gamma v^\star(s') \\
  \textrm{s.t.}  & s' = T(s, a), \\
                 & a \in \Actions(s).
\end{array}
$$

This is called the _Bellman equation_ and all of dynamic programming
consists of method for solving it.

### Existence, Uniqueness and Convergence

### Solving the Bellman Equation

For this section, let's assume that both
the state $\States$ and action $\Actions(s)$ spaces are finite.
I know that this is not the most general setting
but it encompasses a lot of interesting problems
and is a simpler place to start.
Later I will comment a bit on how one can extend the results in this section
to continuous spaces.

#### Value Iteration

I don't know about you but whenever I see a recursion such as Bellman's equation,
I immediately think of fixed point iteration.
By considering an operator

$$
\begin{array}{rl}
 (B v)(s) =
  \min\limits_{a} & c(s, a) + \gamma v(s') \\
  \textrm{s.t.}  & s' = T(s, a), \\
                 & a \in \Actions(s),
\end{array}
$$

we can turn the Bellman equation into an update rule.
All we have to do is to choose an arbitrary initial value function $v$
and iteratively evaluate

$$ v \gets Bv. $$

By the magic of the Banach Fixed Point theorem,
this will converge towards the optimal value function
no matter what the initial value function is.
Thus, we arrive at our first algorithm: _value iteration_.
It consists of setting a tolerance $\varepsilon$ and initial value function $v$
and iterating $v \gets Bv$ until the error is uniformly below $\varepsilon$.
Below we see a Julia implementation of value iteration.

```julia
function value_iteration(v ; tol)
  π  = Dict{States, Actions}()
  maxerr = Inf
  while maxerr > tol
    maxerr = 0
    for s in States
      v0 = v[s]
      v[s], π[s] = findmax(a -> c(s, a) + v[T(s,a)], Actions(s))
      maxerr = max(maxerr, abs(v[s] - v0))
    end
  end
  return v, π
end
```

The algorithm above is in fact just one variation of value iteration.
There are still many choices we can make to improve it that are problem-dependent.
We have chosen to update $v$ in-place,
already propagating the new value function while traversing the states.
We could have kept the old value function and only updated $v$ after
traversing all states.
Our approach has the advantage of using the improved information
as soon as it is available but updating in batch may be interesting
because we can broadcast the optimization across many processes in parallel.

Other important choice we have is the initial value function.
Choosing a good warm start can greatly improve the convergence.
As an example, it is a good idea to already fill $v(\blacksquare)$
with zero for a terminal state.
Finally, the order that we traverse the states matter.
There is a reason why dynamic programming is famous for solving problems backwards.
Whenever we know that a given state is easier to solve,
we should start traverse by it.

If we are writing a solver for a maze,
it makes a lot of sense to start at the exit (where the cost is zero)
and then recursively traverse its neighbours breadth-first.
Similarly, in a fixed horizon, the best approach
is to start in the states for the last stage
and keep going back in time.
When we have such structure in the state space,
traversing this way may even converge to the optimal policy
in a single iteration!

Well, we have a lot of options...
however as long as we keep visiting all states, any of those approaches
converges towards the optimal value function.

#### Policy Iteration

### What if the state space is infinite?

## Example: Shortest Path in a Graph

## Example: Backpropagation

https://coeieor.wpengine.com/wp-content/uploads/2019/03/ijcnn2k.pdf

## Stochastic Dynamic Programming

Until now, we've only dealt with deterministic processes.
Life, on the other side, is full of uncertainty and, as a good applied field,
dynamic programming was created from its inception to deal with stochastic settings.

We call a state machine where the transitions $T(s, a)$ and costs $c(s, a)$
are stochastic a _Markov Decision Process_.
This name comes from the fact that the new state only depends on the current state
and action, being independent of the process' history just like a Markov chain.
A usual intuition for this kind of processes is as the interaction between
an actor and an environment.
At each time step, the environment is at an state $s$
and the actor may choose among different actions $a \in \Actions(s)$
to interact with the environment.
This action affects the environment in some way that is out of reach to the actor
(this stochastic / non-deterministic),
changing its state to $s' = T(s, a)$ and incurring a cost of $c(s, a)$
to the actor, as illustrated in the diagram below.

```dot
digraph {
  rankdir=LR;
  compound=true;

  {rank = source; A [label = "Actor"]};

  subgraph cluster_env{
    rank = same;
    label="Environment";
    node [shape     = circle
          style     = "solid,filled"
          color     = black
          fixedsize = shape
          fillcolor = "#B3FFB3"];
    s2 [label = "s'"];
    s -> s2 [label = "T(s,a)"];
  }

  A -> s:nw [label = "a"       lhead=cluster_env];
  s:s -> A [label = "c(s, a)" ltail=cluster_env];
}
```

Allowing non-determinism opens the way for modeling
a whole lot more of cool situations.
For example, robots that play video games!
The states may be the internal state of the game or some partial observation of them
that is available to the player
and the actions are the buttons on the controller.
The transitions are internal to the game and the costs are related to some winning/losing factor.
Have you ever heard of Deep Mind's
[Playing Atari with Deep Reinforcement Learning](https://arxiv.org/pdf/1312.5602v1.pdf) paper?
In it, they use reinforcement learning to train a robot capable of playing Atari 2600 games
and all modeling is done via Markov decision processes in a way that is really similar
to the discussion in this paragraph. I really recommend checking it out.

With a stochastic environment, we can't necessarily predict the costs and transitions,
only estimate them. To accommodate that, we will have to change a few things
in our definition of policies and value functions.
Those will specialize to our old definitions whenever
the MDP is stochastic.

A deterministic policy was a choice of action for each state.
We define a _stochastic policy_ to be a probability distribution
of choosing an action given an state.
Since the state will usually be random,
we will get inspired by conditional probabilities and denote it by $\pi(a | s)$.

For the value function, we still want to assign a total cost to each state.
A solution is to consider all possible costs and take their average.
The value of a stochastic policy is thus

$$
 v^\pi(s) = \E^\pi\left[c(s, a) + \gamma v^\pi(T(s, a))\right | a = \pi(s)]
$$

where we consider the action $a$ to be randomly sampled with probability $\pi(a | s)$
and write $\E^\pi$ for the average value considering that we follow this probability.

Similarly, the Bellman equation for the optimal value function also considers
the mean cost:

$$
\begin{array}{rl}
 v^\star(s) =
  \min\limits_{a} & \E\left[c(s, a) + \gamma v^\star(s')\right] \\
  \textrm{s.t.}  & s' = T(s, a), \\
                 & a \in \Actions(s).
\end{array}
$$

In this post we will only work the expected value
but know that if you were feeling particularly risk-averse,
you could exchange it for any [coherent risk measure](https://en.wikipedia.org/wiki/Coherent_risk_measure)
and all results would still hold.

### Solving MDPs

If the (possibly random) states, actions and costs are finite,
the methods of _value iteration_ and _policy iteration_ also hold with minor adjustments.
One can prove the existence and convergence of the stochastic Bellman equation
with the same assumptions as before.



## References

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

https://arxiv.org/pdf/2202.13252.pdf

https://www.mit.edu/~dimitrib/allerton_api_final.pdf

https://martin-thoma.com/how-to-draw-a-finite-state-machine/
