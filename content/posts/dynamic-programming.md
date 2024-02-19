---
title: A Tale of Dynamic Programming
keywords: [dynamic-programming, markov-decision-processes, reinforcement-learning]
date: 2022-06-25
description:
  From parsing text to controling robots, dynamic programming is everywhere.
  Let's explore its workings, which problems it solves, and its algorithms.
suppress-bibliography: true
---

\def\States{\mathcal{S}}
\def\Actions{\mathcal{A}}
\def\R{\mathbb{R}}
\def\E{\mathbb{E}}
\def\Bellman{\mathcal{B}}
\def\CBall{\char"1f52e}
\def\dist{\mathrm{d}}
\def\powerset{\mathcal{P}}
\def\dummy{\blacklozenge}

What if I told you that some of the most used algorithms to
find the shortest path in a graph,
calculate gradients while training a neural network,
and parse context-free grammars
are essentially implementations of the same principle?
It is called _dynamic programming_ and is one of those instances
in mathematics where a simple principle unfolds
into profound conclusions ranging over many fields.
In fact, we can, already in this first paragraph,
summarize the idea using Richard Bellman's (Dynamic Programming's creator) own words:

> An optimal policy has the property that
whatever the initial state and initial decision are,
the remaining decisions must constitute an optimal policy
with regard to the state resulting from the first decision.

I have to admit that despite encountering dynamic programming in different contexts,
it took me a while to finally get the "click" that they were actually the same thing.
When learning algorithms and data structures,
it was a memoization-based technique where you could speed up some algorithms
by first solving the easier parts and storing the solution for later use.
Then, at work, I mostly deal with solving a lot of linear programs
for long-term scheduling problems.[^sddp]
The main algorithm we use, called _Stochastic Dual Dynamic Programming_,
at first didn't seem so much like the programming technique from the algorithms class.
Finally, one of the main methods for model-based reinforcement learning
is again called dynamic programming,
and it also didn't seem so much like the other instances.

So, what's happening here?
Did everybody choose to call their algorithms dynamic programming
just because it's a cool name?[^dp-name]
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
For example: hydro is cheap and clean, but you risk running out of water
if you use all of it and next month turns out particularly dry.
Finding the best energy dispatch is once again solved via dynamic programming.

[^dp-name]: Even Richard Bellman admittedly named it based [on how cool it sounds](https://en.wikipedia.org/wiki/Dynamic_programming#History).

On Decision-Making and State Machines
=====================================

Before delving into dynamic programming per se, we first have to establish a few concepts.
After all, it's always best to know which problems you intend to solve
before learning a method to solve them, right?

As a matter of motivation, let's start with something I am really fond of:
old school plataformer games.
In our hypothetical game which is definitely not about some Italian plumber,
the character stands idle doing nothing by default.
But with the press of a button in the controller,
the player may command the character to do a few things:
shoot, jump or walk.
And, of course, each of these actions activate the respective animation on the screen.
In the best Resident Evil style,
this game only allows a character to shoot while idle
and also forces you to first be idle after a jump before doing any other action.
Think of that as the time it takes to restore one's balance after falling.
This description may seem overly complicated on text,
but fortunately the nice folks in the Comp Sci department
already invented diagrams that show these transitions nicely.

<object data="state-machine.svg" type="image/svg+xml">
</object>

Our modeling above is an instance of something called
a _state machine_ or _automata_ if you're into Greek words.
There are 4 states in which the character might be and at each one
there is an available set of actions to take that transitions that state.
More abstractly,
an automaton is a system that can be in one of many _states_ $s \in \States$
and at each state, you can choose among a set of _actions_ $a \in \Actions(s)$.
Whenever you take an action, the system changes to a new state according to a _transition function_

$$T : (s : \States) \times \Actions(s) \to \States.$$

Unfortunately life is not known for its free lunches
and, in general, whenever one takes action $a$ at state $s$,
it is necessary to pay a certain _cost_,
properly modeled as another function

$$c : (s : \States) \times \Actions(s) \to \R.$$

Depending on the context this can be, for example,
a real monetary cost (in economic contexts),
some total distance or elapsed time (for planning)
or even a negative cost representing a reward.

The Dynamics of Decision-Making
-------------------------------

Iterating the transition $T$ establishes a dynamics for our system:
by starting at an initial state $s_0$ and taking a sequence of actions
$\{a_t\}$, we generate a trajectory over the state space.

$$
s_{t+1} = T(s_t, a_t).
$$

When viewed in this light,
our state machines are called _controllable dynamical systems_ or _decision processes_,
which are yet additional cool names for you to memorize.

<!-- TODO: Bad paragraph. What does this mean?  -->
As an example, think of a game of Sudoku.
The states are the (partially numbered) boards
and the actions consist of putting a valid number
in a certain coordinate.
You start with some random board and repeatedly
place numbers until you reach a terminal state
where there are no available actions.

One can argue that a state encapsulates
all you must know about your system in order to choose an action,
no matter the previous history nor time step.
Indeed, if any other thing affects your choice,
you can, without loss of generality,
model the process as a larger automaton where the state also carries the additional information.
Thus, controlling a dynamic system amounts to selecting a valid action for each state,
that is, a function

$$\pi : (s : \States) \to \Actions(s).$$

In the literature this is called a _policy_,
in analogy to a government taking actions to control the state of the nation.

Starting at state $s_0$ and following a policy $\pi$
produces a deterministic dynamical system without the need for choosing a control:

$$
s_{t+1} = T(s_t, \pi(s_t)).
$$

This dynamics, in counterpart, yields a cost $c(s_t, \pi(s_t))$ for each time step.
We could define the total cost for $\pi$ as the sum of those costs,
but there is an additional detail to notice.
Suppose that, for any reason, money is short and you had to take a loan in order to pay your bills.
In those struggling conditions,
would you prefer to pay the money back today or next year?

Sometimes there are factors such as inflation or interests
making future costs have a real value that is different from its nominal value.
This prompts us to introduce a problem dependent _discount factor_ $\gamma \in [0, 1]$
representing how much the cost depreciates over time.
The total cost of following a certain policy $\pi$
is the cumulative sum of all properly discounted costs we generate by following it.
We define the _value function_ $v^\pi : \States \to \R$ associated with $\pi$
as the total cost of starting at a given state:

$$
\begin{array}{rl}
 v^\pi(s) = & c(s_0, \pi(s_0)) + \gamma c(s_1, \pi(s_1)) + \gamma^2 c(s_2, \pi(s_2)) + \ldots \\
  \textrm{where}  & s_0     = s, \\
                  & s_{t+1} = T(s_t, \pi(s_t)), \\
\end{array}
$$

Besides from its practical interpretation,
the discount factor $\gamma$ also plays a significant role
from the analytical point of view.
If $|\gamma| < 1$ and the costs are uniformly bounded
(which is the case for a finite action space, for example)
we can guarantee that the series defining $v^\pi$ converges
for any choice of actions and initial state.
That is, suppose there exists $M > 0$ such that

$$\forall s \in \States, a \in \Actions(s),\, |c(s, a)| \le M.$$

This bounds the total cost by a geometric series that cannot blow up,

$$
\sum\limits_{t=0}^\infty \gamma^{t}|c(s_t, a_t)| \le \sum\limits_{t=0}^\infty \gamma^{t} M \le \frac{M}{1 - \gamma},
$$

thus guaranteeing that the value function is well-defined.

Optimal Decisions
-----------------

Having multiple possible courses of action
prompts us to ask which one is the best.
When programming a robot to escape a labyrinth,
you want it to take the least amount of time;
When controlling a spaceship towards the moon,
it is important to guarantee that it will use the least amount of fuel;
When brawling at a bar, you want to knock out your foe
while sustaining the least injuries possible.
Most of all, the best policy is the one with
the least cost taking _all time_ into account --- both the present and its future consequences.
For example, sometimes a policy that has a higher cost for the first state
is overall better because it puts us into a more favorable state.
Thus, our problem can be naturally formulated as searching for _optimal policies_:

> Starting at state $s$, find a policy $\pi$ producing the least total cost over time.

Or equivalently in math language:

$$
\begin{array}{rl}
\min\limits_\pi v^\pi(s) =
  \min\limits_{a_t} & \sum\limits_{t=0}^\infty \gamma^{t}c(s_t, a_t) \\
  \textrm{s.t.}     & s_0     = s, \\
                    & s_{t+1} = T(s_t, a_t), \\
                    & a_t \in \Actions(s_t).
\end{array}
$$

Right now, this may seem like a big and scary optimization problem
but in fact it contains a lot of structure we're able to exploit.
That is the subject of the next section.
But, before we continue,
let's go over a little tangent on how to formulate some classical problems
in this decision-making framework.

### Example: Shortest Path in a Graph

Suppose you are at your hometown
and just received a message from a friend
telling you that there are singing llamas in Cuzco, Peru, right now.
This makes you at the same time incredulous and curious,
so you just pick your favorite bike and get on the road towards Cuzco.
Unfortunately there are no direct bikeways connecting your home to Cuzco,
meaning that you will have to find a route going through other cities.
Also, there is a risk that the llamas will stop to sing at any time
and just go back to their usual behavior of eating grass throughout the mountains.
This prompts you to decide to take the shortest possible path to Cuzco.

The above description is an instance of finding the shortest path in a graph.
In it, we represent each city by a graph node and direct routes between two cities
as a weighted edge where the weight is the distance.
Going from home to Cuzco amounts to finding the path between those two nodes
with the smallest total distance.

The translation from this graph description
to a decision process is quite straightforward.

* **States**: nodes in the graph.
* **Actions** at state $s$: edges going from $s$ to another node.
* **Transition**: the opposite node on the same edge.
That is, given an edge $s \to s'$, $T(s, s \to s') = s'$.
* **Costs**: $c(s, a)$ is the weight of edge $a$ --- the time taken traveling through that edge.

Finding the shortest path from $s$ to $z$
is the same as setting the initial state to $s$ and making $z$
a terminal state of our dynamics.

Dynamic Programming
===================

Alright, it's finally time to start optimizing those decision problems.
The simplest idea would be to exhaustively search the space of all actions
trying to find the best solution.
Notice that even for finite states and horizon, this may be prohibitively expensive
since the possible candidates grow exponentially with the time steps.
Any practical method must take into account how this class of problems
naturally breaks apart into separate stages.

Our approach will involve the famous _Bellman principle of optimality_,
which is the cornerstone of dynamic programming.
Taking @{bellmanDPBook} [ch 3, p. 83]'s own words, it reads as:

> An optimal policy has the property that
> whatever the initial state and initial decision are,
> the remaining decisions must constitute an optimal policy
> with regard to the state resulting from the first decision.

Alright, what does this mean?
What the principle of optimality is telling us
is that in order to calculate an optimal policy,
we should turn this iterative process of making actions and calculating costs
into a recursive procedure.
That is, taking an action $a$ at the initial state $s$
puts us into a new state $s' = T(s, a)$
where we are again faced with the exact same problem of finding an optimal policy,
but this time starting at $s'$.
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
  \min\limits_{a_t} & \sum\limits_{t=0}^\infty \gamma^{t}c(s_t, a_t) \\
  \textrm{s.t.}     & s_0     = s, \\
                    & s_{t+1} = T(s_t, a_t), \\
                    & a_t \in \Actions(s_t).
\end{array}
$$

Notice in the optimization problem above
that the initial state is only ever used to choose the first action.
Later actions do not depend directly on it but only on its consequences.
This means that we can break the problem into two parts:
calculating an _immediate cost_ dependent only on the initial state
and calculating a _future cost_ dependent on all the ensuing states.

$$
\begin{array}{rl}
 v^\star(s) =
  \min\limits_{a,a_t} & {c(s, a)} + \left(
    \begin{array}{rl}
      \min\limits_{a_t} & \sum\limits_{t=1}^\infty \gamma^{t}c(s_t, a_t) \\
      \textrm{s.t.}     & s_1 = s', \\
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
starts at $t = 1$, we can factor out $\gamma$.
By renaming $l = t-1$ we get

$$
\sum\limits_{t=2}^\infty \gamma^{t-1}c(s_t, a_t)
= \gamma \sum\limits_{t=2}^\infty \gamma^{t-2}c(s_t, a_t)
= \gamma \sum\limits_{l=1}^\infty \gamma^{l-1}c(s_l, a_l),
$$

and applying this in the expression for $v^\star$,

$$
\begin{array}{rl}
 v^\star(s) =
  \min\limits_{a} & c(s, a) + \gamma\left(
    \begin{array}{rl}
      \min\limits_{a_l} & \sum\limits_{l=0}^\infty \gamma^{l}c(s_l, a_l) \\
      \textrm{s.t.}     & s_0 = s', \\
                        & s_{l+1} = T(s_l, a_l), \\
                        & a_l \in \Actions(s_l)
    \end{array}
  \right) \\
  \textrm{s.t.}  & s' = T(s, a), \\
                 & a \in \Actions(s).
\end{array}
$$

Although this is a huge expression,
it should be straightforward to see that the expression
for the future cost is _exactly_ the optimal value $v^\star(s')$
of starting the dynamics at $s' = T(s, a)$.
This way, the principle of optimality express itself mathematically
as a recursive equation that the value for an optimal policy must satisfy.

$$
\boxed{
  \begin{array}{rl}
   v^\star(s) =
    \min\limits_{a} & c(s, a) + \gamma v^\star(s') \\
    \textrm{s.t.}  & s' = T(s, a), \\
                   & a \in \Actions(s).
  \end{array}
}
$$

This is called the _Bellman equation_ and all dynamic programming
consists of methods for solving it.
Even more: we can think of the Bellman equation as a recursive specification
for the decision problems and of dynamic programming
as any problem-specific implementation that solves it.

Existence, Uniqueness and Fixed Points {#fixed-point}
-----------------------------------------------------

It is time to get deeper into analysis.
Whenever mathematicians see a recursive relation such as the Bellman equation,
they immediately start asking such things:
what guarantees do we have about $v^\star$?
Can I trust that it is unique? Does it even exist?
Surely we mathematicians may seem a bit too anxious with all these questions,
but they are for good reasons.
Besides the guarantee that everything works,
in this case proving the existence of a solution also teaches us how to construct it!
So pay attention, because in the next section
we're going to adapt the theorems in here into algorithms to solve the Bellman equation.

Recursion has a deep relationship with fixed points,
allowing us to use whichever is more practical to our goals.
To solve a problem with dynamic programming,
our first step will be to write the Bellman equation
as the fixed point of an operator
$\Bellman : (\States \to \R) \to (\States \to \R)$
called --- guess what --- the _Bellman Operator_.
It takes value functions to value functions and is defined as

$$
\begin{array}{rl}
 (\Bellman v)(s) =
  \min\limits_{a} & c(s, a) + \gamma v(s') \\
  \textrm{s.t.}  & s' = T(s, a), \\
                 & a \in \Actions(s).
\end{array}
$$

Now, the optimal value function $v^*$ is the fixed point
We thus reduce the question of existence and uniqueness of solutions
for the Bellman equation to finding fixed points of $\Bellman$:

$$ v^* = \Bellman v^*.$$

If you are not accustomed to fixed points,
the transition above from the Bellman equation to operator may seem strange.
Hence, let's go through a little story in order to develop some intuition.

Imagine that you are the king/queen of a fantastical kingdom.
You are an absolute monarch and whatever action you decide to take,
your subjects will follow.
Lately, the kingdom's reserves are running dry
and your counselors advised you to define a clear governing policy
in order to minimize the kingdom's spending.
Since besides a ruthless ruler you're also a great mathematician
and a fan of this blog,
at this point you already know what must be done to save your kingdom from bankruptcy:
solve the Bellman equation.

Because at this point of the post you still don't know how to solve it,
you decide to take advantage of your fantastical world and hire a wizard
to look into his crystal ball and act as an oracle telling you
how much each state of affairs will cost to the kingdom in the future.
After God knows how many rituals and incantations,
the wizard hands you a shining new value function $\CBall : \States \to \R$.

Since it is never wise to blindly follow the advices from a crystal ball,
you decide to use it only to predict the future,
while relying on your discernment for taking immediate decisions.
In other words, you rule your kingdom by solving the optimization problem

$$
\begin{array}{rl}
  (\Bellman \CBall)(s) = \min\limits_{a} & c(s, a) + \gamma \CBall(s') \\
  \textrm{s.t.}  & s' = T(s, a), \\
                 & a \in \Actions(s).
\end{array}
$$

Thus, the process of going from prediction to decision is precisely the Bellman operator.
The function $\Bellman v$ is the cost of choosing the best action taking $v$ as your future estimate.

### A Useful Theorem

Alright, we have transformed the problem
of finding an optimal decision into solving the Bellman equation,
and then transformed it again into finding a fixed point to the Bellman operator,

$$ \Bellman v^\star = v^\star. $$

If this seems as complicated --- although more abstract --- as where we started,
have no fears!
We just arrived at a point where we can invoke a powerful theorem from Analysis
to solve all our issues at once.
Behold the _Banach Fixed Point Theorem_!

::: {.Theorem data-title="Banach Fixed Point"}
In a complete metric space $(M,\dist)$,
any continuous $f : M \to M$ that decreases the distance between points:

$$ \dist(f(v), f(w)) \le \gamma \dist(v, w),\; \textrm{for } \gamma \in [0, 1),$$

Has a unique fixed point $v^\star$.

Furthermore, one can arrive at $v^\star$ from any initial value $v_0$
by iterating $f$:

$$ \lim_{n \to \infty} f^n(v_0) = v^\star,\; \forall v_0 \in M.$$

This procedure converges linearly with the error at each iteration bounded as

$$ \dist(v_n, v^*) \le \frac{\gamma^n}{1 - \gamma} \dist(v_0, f(v_0)).$$
:::

Proving this theorem is out of scope for this post[^banach].
However, we can think of it as saying that if a mapping shrinks all distances,
then eventually the image of all points will be squeezed into a single point.

<!-- TODO: FIGURE -->

[^banach]: It is out of scope more because it is a tangent to the topic
than because of any difficulty in the proof.
If you are interested in analysis, I really recommend you to try proving it.
The main idea is using the contraction property
to show that the distance between the iterations of $f$ must converge towards zero.

What we care the most about this theorem
is that it gives us a constructive recipe to find fixed points on metric spaces.
All we have to do is to interpret the fixed point equation as an update rule,

$$ v \gets f(v),$$

And iterate it until the distance converges to below a certain tolerance.
It is then straightforward to convert the description above into a computational procedure.

```julia
function fixed_point(f; v0, tol)
  v = f(v0)
  while distance(v, v0) > tol
    v0 = v
    v  = f(v)  # Update rule
  end
  return v
end
```

### A Metric Space of Value Functions

To apply the Banach fixed point theorem to the Bellman operator,
we must find a suitable function space where $\Bellman$ is a contraction.
A fitting choice are the bounded continuous functions over the states,
$C^0_b(\States, \R)$ with distance given by the uniform norm

$$ \dist(v, w) = \|v - w\|_\infty = \sup_{s \in \States} |v(s) - w(s)|.$$

In this (complete) metric space, $\Bellman$ turns out to be a contraction with factor $\gamma$
(yes, the discount factor).
Furthermore, for finite state spaces, _any function is continuous and bounded_,
meaning that the above encompasses all algorithms of interest in this post.

Since I don't want to depart too much from the post's main topic
nor dive into mathematical minutiae,
we are going to relegate the necessary proofs to an [appendix](#appendix).

::: {.Theorem data-title="Existence and Uniqueness of Solution"}
Any decision process with discount factor $\gamma < 1$
has a unique optimal value function $v^\star$ satisfying the Bellman equation

$$
  \begin{array}{rl}
   v^\star(s) =
    \min\limits_{a} & c(s, a) + \gamma v^\star(s') \\
    \textrm{s.t.}  & s' = T(s, a), \\
                   & a \in \Actions(s).
  \end{array}
$$

Furthermore, you can calculate the (not necessarily unique) optimal policy via

$$
  \begin{array}{rl}
   \pi^\star(s) =
    \argmin\limits_{a} & c(s, a) + \gamma v^\star(s') \\
    \textrm{s.t.}  & s' = T(s, a), \\
                   & a \in \Actions(s).
  \end{array}
$$
:::

::: Proof
It all follows from applying the Banach fixed point theorem to the Bellman operator.
:::

The above result is what I like to call a "bazooka theorem",
because besides guaranteeing the existence and uniqueness of an optimal value function
--- and consequently an optimal policy ---
it also teaches us how to calculate it for finite states,
as we will shortly see in the ensuing section.

Solving the Bellman Equation
============================

Dynamic Programming consists of solving the Bellman Equation,
and, as with all famous equations, there are many possible approaches.
Which one to choose will depend on the problem and hardware at hand.

From now on, let's assume that both
the state $\States$ and action $\Actions(s)$ spaces are _finite_.
This allows us to focus on exhaustive methods exploring the entire state space.
There are other methods, such as Reinforcement Learning or Dual Dynamic Programming,
which are able to generalize the ideas in here to infinite spaces.
But this is a story for another night...

Before jumping into the algorithms,
it is worth discussing a couple of technical decisions
we must make in order to implement them.

This first thing we must notice is
that although the most straightforward way to represent functions in a programming language
is through computational procedures[^procedure-function],
it would be quite inefficient in our case.
This happens because altering procedures for improvement is computationally expensive.
Thus, by efficiency reasons,
it is customary to represent the policy and value function not as functions
but using some other data structure.
Since our state space is finite,
there is a wide range of data structures capable of exactly representing such functions.
Common choices are arrays or hash maps,
but you can really use anything capable of storing coefficients.
Below we see some examples with these types for in-memory storage.

[^procedure-function]: They are even called _functions_ in most programming languages.

```julia
# Storage with vector / array
# WARNING: This requires some method idx : States -> Int for later indexing
function asarray(f :: Function)
  return [f(s) for s in States]
end

# Storage with hash map / dictionary
function asdictionary(f :: Function)
  return Dict(s => f(s) for s in States)
end
```

The memoization that people associate with dynamic programming
lies entirely in this "trick".
However, it is good to keep in mind that this is only a matter
of computational representation of functions
and is totally orthogonal to any algorithmic design.
In a language with first-class functions,
it is possible to do dynamic programming using only function composition.
It just so happens that it will not be as fast as one would like.

In the algorithms, we will write this choice of representation
as two opaque types `Values{States}` and `Policy{States, Actions}`,
assumed to deal with all boilerplate as needed.

Another important thing to mention is that
we will also only interact with a process via its _total cost_,
never touching its constituent parts separately[^other-total-cost].
Hence, let's already build a function that does the conversion for us.

```julia
# Turn a decision problem into its respective cost function.
function total_cost(p :: Process)
  return (v, s, a) -> p.cost(s, a) + p.γ * v[p.next(s, a)]
end
```

Notice how we used square brackets to represent that we are accessing a data structure instead of calling a function.

[^other-total-cost]: You may wonder if this means that dynamic programming
also works for decision processes whose costs satisfy other kinds of Bellman equations (non-additive costs, for example)
and the answer is yes!
A great place to start is @{bertsekasADP}'s book.
Just be warned that there are a lot more technicalities to deal with in those cases.

Value Iteration
---------------

Thus, we arrive at our first algorithm: _value iteration_.
Recall from the previous discussion
that iterating the Bellman operator over any input converges towards the optimal value function.
The algorithms main idea comes quite straightforwardly from it:
convert the Bellman equation into an update rule to find its fixed point.

$$ v \gets \Bellman v. $$

We can thus start with any initial value function $v_0$ and iterate the update rule above.
By the magic of the Banach Fixed Point theorem,
this will converge towards the optimum.
This procedure repeats until the uniform error $\| v - \Bellman v \|_\infty$
becomes less than a previously set tolerance.

Each iteration of our algorithm comes from evaluating
the Bellman operator in our previously chosen representation.
Let's thus use our `total_cost` to write it.

```julia
# The Bellman operator corresponding to a decision process.
# It uses a storage representation `Values` for the value function.
function bellman_operator(prob :: Process)
  return function(v)
    Bv = Values{States}()  # Empty representation
    for s in States
      Bv[s] = minimum(a -> total_cost(prob)(v, s, a), Actions(s))
    end

    return Bv
  end
end
```

Finally, the algorithm comes directly from finding the fixed point
the procedure above generates.
To calculate the policy, we find the `argmin` for the optimal value function at each state.
The name _value iteration_ is because it only uses the value function in the update process,
with the calculated policy playing no role.

```julia
function value_iteration( prob :: Process     # Data for decision process
                        ; v0 = zeros(States)  # Warm start --- all zeros if you don't know any better
                        , tol)                # Stopping tolerance
  # The optimal value function is the fixed point of the Bellman Operator
  v_opt  = fixed_point(bellman_operator(prob); v0, tol)

  # The optimal policy is the choice of action for the total cost with the optimal value function.
  π_opt  = Policy{States, Actions}()
  for s in States
    π[s] = argmin(a -> total_cost(p)(v_opt, s, a), Actions(s))
  end

  return π_opt, v_opt
end
```

The algorithm above comes from directly implementing the Fixed Point Theorem,
and, because of this, is guaranteed to [converge linearly](https://en.wikipedia.org/wiki/Rate_of_convergence) to the optimum.
Furthermore, at each iteration, the minimization procedures happen independently
for each state, making the evaluation of $\Bellman v$ embarrassingly parallel.

In-place Value Iteration
------------------------

Despite the parallelization opportunities shown by the previous implementation,
it can feel too sluggish when implemented sequentially,
because it waits until after traversing all states to update the value function.
Another approach, better suited for a sequential machine,
is to update the value function in-place in order to promptly propagate
the improved information to the other states.
The trade-off is that this approach is no longer able
to broadcast the optimization across many processes in parallel.

Algorithmically speaking, the only required change
is rewriting the fixed point iteration procedure to calculate in-place.

```julia
function fixed_point_inplace!(f, v; tol)
  maxerr = Inf
  while maxerr > tol
    maxerr = 0  # Start with smallest error possible
    for s in States
      prev = v[s]
      v[s] = f(v)[s]
      # Estimate ||f(v) - v||_∞ component by component
      maxerr = max(maxerr, abs(v[s] - prev))
    end
  end

  return v
end

function value_iteration_inplace!(prob, v0 = zeros(States) ; tol)
  operator(v) = Values(minimum(a -> total_cost(prob)(v, s, a), Actions(s)) for s in States)
  v_opt  = fixed_point_inplace!(operator, v0 ; tol)

  π_opt  = Policy{States, Actions}()
  for s in States
    π[s] = argmin(a -> total_cost(p)(v_opt, s, a), Actions(s))
  end

  return π_opt, v_opt
end
```

In the animation below,
we can see in-place value iteration in action for the problem of escaping from a maze.
In this model, each state is a cell in the grid
and the actions are the directions one can take at that cell (neighbours without a wall).
The objective is to reach the right-bottom edge in the minimum amount of steps possible.
We do so by starting with a uniformly zero value and a random policy.
In the left we see the value function at each iteration
and in the right the associated policy.

![](labyrinth-value-iteration.webm)

The implementations above are, indeed, just variations on the same idea:
iterating the Bellman operator in order to converge to the optimal value function.
There are many other tweaks we could make to it which, nevertheless, don't affect the algorithm's essence:
choosing good warm starts, parallelizing, changing the state traversal order in the in-place version, etc.
The best approach tends to be problem-dependent.

Policy Iteration
----------------

One issue with value iteration is that all policy calculations are implicit,
since we just work with value functions.
Therefore, it is possible to reach an optimal policy but keep iterating
the algorithm because the value function has not converged yet.
In this section, we will get acquainted with _policy iteration_,
an algorithm that uses policies to calculate value functions
and value functions to calculate policies
until it converges towards the optimal.
It's selling point is being able to
directly calculate an optimal policy in a finite number of steps.

### Policy Evaluation

Let's say somebody gave you a policy $\pi$ and told you nothing more about it.
How can you calculate the value function $v^\pi$ associated with it?
One way is to notice that it satisfies a recursion
similar to the Bellman equation, but without the minimization step.

$$
  \begin{array}{rl}
   v^\pi(s) =
     & c(s, a) + \gamma v^\pi(s') \\
    & \quad\textrm{where}\; s' = T(s, \pi(s)).
  \end{array}
$$

We can also transform this equation into a fixed point problem
by defining an operator

$$
\begin{array}{rl}
 (\Bellman^\pi v)(s) =
     & c(s, a) + \gamma v(s') \\
     & \quad\textrm{where}\; s' = T(s, \pi(s)).
\end{array}
$$

The above can be readily written as a computational procedure:

```julia
function policy_bellman_operator(prob :: Process, pi :: Policy)
  return function(v)
    Bv = Values{States}()  # Empty representation
    for s in States
      Bv[s] = total_cost(prob)(v, s, pi[s])
    end
    return Bv
  end
end
```

Now, we can look at $\Bellman^\pi$ as the cost for a decision process
where the action set for each state was collapsed into a single option.
Hence, we know that, under the same assumptions as before,
it has a unique fixed point.

Moreover, turning it into an update procedure
converges towards $v^\pi$ for any initial value function.
This way, we arrive at an algorithm for evaluating the cost of a policy,
unimaginatively called _policy evaluation_.

```julia
function policy_evaluation(prob :: Process
                          , π   :: Policy
                          ; v0 = zeros(States)
                          , tol)
  return fixed_point(policy_bellman_operator(prob, pi); v0, tol)
end
```

Notice the similarity with value iteration.
The only true difference is on which operator we pass to the fixed point:
instead of choosing an optimal action, it just follows along with the policy.
Moreover, all the discussion above on the variations of value iteration
also holds for policy evaluation.
You can do the same modifications with same effects.

### Policy Improvement

After we know a policy's value function,
our next question is how to update it into a better policy.
That is, how can we use this information to get
nearer to the optimal.

Remember from the previous discussions that applying
the Bellman operator $\Bellman$ to any non-optimal value function
produces a strictly better result.
It can, therefore, improve a policy's value function.

$$ (\Bellman v^\pi)(s) = \min_{a \in \Actions(s)} c(s, a) + v^\pi(T(s, a)) \le c(s, \pi(s)) + v^\pi(T(s, \pi(s))) = v^\pi(s).$$

The value function $\Bellman v^\pi$ encodes the cost of choosing the best action right now
while following $\pi$ on all future steps.
We can get a policy from it by taking the solution to the optimization problem.

$$
\begin{array}{rl}
 \pi'(s) =
  \argmin\limits_{a} & c(s, a) + \gamma v^\pi(s') \\
  \textrm{s.t.}  & s' = T(s, a), \\
                 & a \in \Actions(s).
\end{array}
$$

Since, unless $\pi$ was optimal,
the equation above generates a strictly better policy.
From it we can define a procedure, called _policy_improvement_,
which turns a value function $v$ into a policy $\pi$
that is better than whatever policy v represented.

```julia
function policy_improvement(prob :: Process, v :: Values)
  π = Policy{States, Actions}()
  for s in States
    π[s] = argmin(a -> total_cost(v_π, s, a), Actions(s))
  end
  return π
end
```

### Alternating Evaluation and Improvement

By starting from any random policy $\pi_0$,
and alternatively running policy evaluation and improvement,
we generate a sequence of policies and value functions

$$
\pi_0 \xrightarrow{\textrm{evaluation}} v^{\pi_0} \xrightarrow{\textrm{improvement}}
\pi_1 \xrightarrow{\textrm{evaluation}} v^{\pi_1} \xrightarrow{\textrm{improvement}}
\ldots
$$

The value functions are monotonically decreasing
while the policies strictly improve until converging to the optimum.

We thus arrive at another dynamic programming algorithm: _policy iteration_.
It consists of iteratively taking a policy $\pi$,
finding its value function $v^\pi$ through policy evaluation
and finally using policy improvement to arrive at a better policy.
Since there are only finitely many policies,
and we always obtain a strictly better policy,
this algorithm is guaranteed to converge to an optimal policy
in a finite amount of steps.

```julia
function policy_iteration(prob :: Process
                         ; v0 = zeros(States)
                         , π0 = rand(Policy{States, Actions)
                         , tol)
  v  = policy_evaluation(prob,  π_0 ; v0 = v_0, tol = tol)
  π = policy_improvement(prob, v)

  while π != π0
    π0 = π
    # Use previous v as warm start
    v  = policy_evaluation(prob,  π ; v0 = v, tol = tol)
    π  = policy_improvement(prob, v)
  end
  return π, v
end
```

Just like value iteration,
policy iteration also accepts many variations on how we traverse the states.
The implementation above is close to the theory and is embarrassingly parallel
on both the evaluation and the improvement step.
Nevertheless, it is useful to think of policy iteration more as an algorithmic principle
than as an algorithm itself and adapt the steps to consider
any problem specific information that may be available.

Backward Induction over a Finite Horizon
----------------------------------------

Until now, we haven't assumed anything about the decision process.
By exploiting the problem's state space structure,
we can turn these algorithms into much faster ones.
In this section we deal with _finite horizon problems_
and show that, for them, we can make value iteration converge in a single iteration!

When the dynamics ends at a certain number $N$ of time steps,
we say that the problem has a finite horizon.
In this case, the stage $t$ is part of the state variable,
(Because how else would we know when to stop?)
and there is a _terminal state_ $\blacksquare$
with zero cost and representing anything that happens after the dynamics end.
Essentially, we work for $N$ stages and then reach $\blacksquare$,
where we can just relax and do nothing for the rest of eternity.


<object data="finite-horizon.svg" type="image/svg+xml" width="100%">
</object>


If you prefer equations to figures,
a finite horizon state machine is one where
the transition function looks like this:

$$ \bar{T}((t, s),\, a) = \begin{cases}
    (t + 1, T(s, a)), & t < N \\
    \blacksquare, & t = N.
  \end{cases}
$$

But what is so special about finite horizons?
After all, the equation above seems much more confusing than what we had before.
Well... what we gain is that the state space $\States$
is clustered into smaller spaces that are visited in sequence:
$\States_1, \States_2, \ldots, \States_N, \States_{N+1} = \{\blacksquare\}$.

The _backward induction_ algorithm consists of value iteration
but exploiting the structure we just discussed.
At the terminal state, the cost is always zero, so we can set $v(\blacksquare) = 0$.
But this, means that the _future cost_ for all states in $\States_N$ is fixed,
and the optimal policy for them is just to choose the cheapest action.
Now that we know which action to choose in $\States_N$,
the future is fixed for all states in $\States_{N-1}$,
reducing the problem in them to the routine we just did.
We repeat this procedure until we reach the first stage.
This calculates an optimal policy by induction,
but moving "backwards" in time.

```{=html}
<object data="backward-induction.svg" type="image/svg+xml" width="100%">
  <img src="backward-induction.svg"
       alt=""
       title="Backward induction algorithm"
       width="100%" />
</object>
```

In Julia code, the algorithm looks like this:

```julia
function backward_induction()
  v  = Values{States}()
  π  = Policy{States, Actions}()
  for t in N:1
    for s in States(t)
      v[s], π[s] = findmin(a -> total_cost(v, s, a), Actions(s))
    end
  end
  return π, v
end
```

I recommend you to compare this with the generic value iteration
in order to see what we gain.
One thing should be clear:
backward induction does the exact same operation as value iteration for each state,
but only requires a single pass over the state space.
This makes it much more efficient.
Another more subtle detail is that since we have more structure to exploit
in the dynamics, the algorithm doesn't have to assume so much about the Bellman operator.
For example, although hand-wavy, we just gave a proof of convergence above
that has no dependence on the Banach fixed-point theorem.
Thus, as a bonus, backward induction works for any discount factor $\gamma$.[^real-bi]

[^real-bi]: In contrast with value iteration, it also has no dependence
on the costs being real numbers. In this case, any closed Semiring would do.
But I digress... [This is out of scope for this post](/posts/algebraic-path).

Nondeterminism: One Cannot Know It All
======================================

Ok, it's time to recapitulate a bit.
We started our journey looking at automata and controllable dynamics,
showing how we can represent the best set of actions
as those who fulfill a certain recursive relation on their value functions,
called the Bellman equation.
Then, we explored a couple ways of solving this equation
through methods for finding fixed points.
A detail that stands out on all of that, thought,
is that we always had perfect information about our systems.

In the real world,
there are no guarantees that taking an action will put you in a determinate state.
In general, there is a myriad of possible states --- perhaps all of them ---
that the system could transition to.
Let's model this by modifying the transition function's type to

$$ T : (s : \States) \times \Actions(s) \to M \States $$

where $M$ converts a type to some uncertain context.[^monads]
Common examples of such operators are

* $M$ is the power set $\powerset$.
In this case,
the transition output enumerates all possible ensuing states,
defining a [nondeterministic automaton](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton).
* $M$ takes sets to probability distributions over them.
This way, we have stochastic systems with multiple possible futures.
Differently from the previous example, though,
in this case there is a notion of how probable is each following state.
* M is the identity.
This way, $M \States = \States$.
This serves to show that the deterministic case is also encompassed the nondeterministic one.
* $M$ represents actions taken by others.
In many situations --- such as a game ---
there are other players besides you who are capable of taking actions
modifying the states.
Since their actions also influence on the outcomes,
the transition can only return a function $\Actions(s) \to \States$.

[^monads]: The technical definition is that $M$ should be a [Monad](https://en.wikipedia.org/wiki/Monad_(category_theory)).
But discussing those details is out of scope here.
See [this other post on autamata with contexts](/posts/automata-monads)
for a discussion of this structure in a similar context.

To deal with nondeterministic transitions,
we need some way to take a value function
and aggregate it over all possible costs
for the uncertain future into a single real number,

$$ \rho : (\States \to \R) \times M \States \to \R. $$

The function $\rho$ depends on which uncertainty context we're dealing with,
and will shortly see some examples of it.
Using it, we can define a Bellman operator

$$
\begin{array}{rl}
 (\Bellman v)(s) =
  \min\limits_{a} & c(s, a) + \gamma \rho(v, s') \\
  \textrm{s.t.}  & s' = T(s, a), \\
                 & a \in \Actions(s).
\end{array}
$$

Quite similar to what we had before, don't you think?
With some mild conditions over $\rho$,
all derivations on the [appendix](#appendix)
for existence and uniqueness of solutions still work in this context.
Moreover, all our algorithms --- value iteration, policy iteration, backwards induction ---
only used the system's total cost,
which is still a deterministic function.
Thus we can apply them to this context _without any need for modification_.
How cool is that?

Example: Recurrence Equations and Fibonacci
-------------------------------------------

What would be of a dynamic programming tutorial
without calculating the good ol' Fibonacci numbers, right?
This example is actually pretty simple
and using the full power we've developed is a real overkill.
Nevertheless, as it is a rather common first example to encounter
when learning dynamic programming in Computer Science classes,
it is worth it to view this kind of problem in the light of the formalism we've developed.

A recurrence relation is any function $\N \to \R$,
where the nth term is recursively defined through the previous terms.

$$
f(n) = \begin{cases}
  c_n,                          & n < k \\
  g(n, f(n-1), \ldots, f(n-k)),    & n \ge k.
\end{cases}
$$

The $c_n$ are constants corresponding to the base case of the recursion
while other values $f(n)$ depends directly on the previous $k$ terms.

Famous examples with this structure are the factorial and Fibonacci functions:

\def\fib{\mathrm{fib}}

\begin{align*}
  \mathrm{fat}(n) &= \begin{cases}
    1,                         & n = 0 \\
    n \cdot \mathrm{fat}(n-1), & n \ge 1.
  \end{cases} \\


  \fib(n) &= \begin{cases}
    0,                     & n = 0 \\
    1,                     & n = 1 \\
    \fib(n-1) + \fib(n-2), & n \ge 2.
  \end{cases} \\
\end{align*}

In general, evaluating $f(n)$ directly via the definition may be exponentially slow.
But dynamic programming allows you to calculate it time that's linear in $n$.

The idea is to define a nondeterministic decision process
whose Bellman equation is exactly the recurrence relation.
Then, the optimal value function will equal $f$
for all values until the $N$ we want to evaluate.
The states are the numbers $0,\ldots, N$
while there is only a single dummy action $\dummy$.
For the transition, we follow the indices recurrence
using the power set as our source of nondeterminism.
Notice that this is a finite horizon process.

$$
T(s, \dummy) = \begin{cases}
  \emptyset,                    & s < k \\
  \{s-1, s-2, \ldots, s-k\},    & s \ge k.
\end{cases}
$$

For the immediate cost,
we use the base cases $c_n$ for the first $k$ stages
while the others are all zero.

$$
c(s, \dummy) = \begin{cases}
  c_s,  & s < k \\
  0,    & s \ge k.
\end{cases}
$$

And to aggregate over the set of future indices,
what a better choice than using the relation $g$ itself?

$$ \rho(v, s') = g(s, \{ v(n) \mid n \in s' \}). $$

With this setup,
this system's Bellman equation looks a lot like the original recurrence.

$$
v(s) = \min_{a \in \{\dummy\}} \begin{cases}
  c_s,                          & s < k \\
  g(s, v(s-1), \ldots, v(s-k)), & s \ge k.
\end{cases}
$$

Since, there is only a single action, the minimization is redundant
and its fixed point satisfies the original recurrence.
Since the problem at hand has a finite horizon,
we can solve it via Value Iteration or Backwards Induction
even without a discount factor $\gamma$.


As an example, let's calculate the first 15 Fibonacci numbers.
The following video shows the steps for value iteration.

![](fibonacci-value-iteration.webm)

Since the horizon is finite,
we can improve it even more by using Backwards Induction!
The process we've just constructed has a single state per stage
where we consider the initial state to be $N$
and the final one $0$
(which means going backwards, in this case).
We can, therefore, use backwards induction
to evaluate the Fibonacci equation in exactly $n$ steps.

![](fibonacci-backward-induction.webm)

Stochastic Dynamic Programming
------------------------------

Until now, we've only dealt with deterministic processes.
Life, on the other hand, is full of uncertainty and, being a nice applied field,
dynamic programming was created from its inception keeping in mind stochasticity.

We call a state machine where the transitions $T(s, a)$
are stochastic a _Markov Decision Process_ (MDP for short).
This name comes from the fact that the new state only depends on the current state
and action, being independent of the process' history;
Just like a Markov chain.
A usual intuition for this kind of processes is as the interaction between
an actor and an environment.
At each time step, the environment is at a state $s$ (which is known to the actor),
and the actor may choose among different actions $a \in \Actions(s)$,
each one incurring a certain cost[^sto-cost], to interact with it.
This action affects the environment in some way that is out of reach to the actor
(thus stochastic / non-deterministic), changing its state to $s' = T(s, a)$.
This is illustrated in the diagram below.[^ref-diagram-mdp]

[^sto-cost]: It is also possible to define MDPs where the cost is stochastic.
The formulations are equivalent, and one may choose which one best models the problem at hand.

[^ref-diagram-mdp]: Loosely based on the diagram found in @{rl2018}.

<object data="mdp.svg" type="image/svg+xml">
  <img src="mdp.svg" alt="" title="Markov Decision Processes" />
</object>

Allowing non-determinism opens the way for modeling a lot more cool situations.
For example, robots that play video games!
The states may be the internal state of the game or some partial observation of them
that is available to the player
and the actions are the buttons on the controller.
The transitions are internal to the game and the costs are related to some winning/losing factor.
Have you ever heard of @{atariDRL2013}'s
_Playing Atari with Deep Reinforcement Learning_ paper?
In it, they use reinforcement learning to train a robot capable of playing Atari 2600 games
and all modeling is done via Markov decision processes in a way that is really similar
to the discussion in this paragraph. I really recommend checking it out.

With a stochastic environment,
we can't necessarily predict the total costs and transitions, only estimate them.
To accommodate that, we will have to change a few things
in our definition of policies and value functions.

Even if fix a policy $\pi$,
we cannot be certain of what is going to happen,
because the costs and transitions are stochastic.
We are, however, able to calculate the average cost
by taking the expected value among all possible outcomes,
conditioned to following the policy $\pi$.

$$
  v^\pi(s) = \E \left[ \sum\limits_{t=1}^\infty \gamma^{t-1}c(s_t, a_t) \middle| s_1 = s,\, s_{t+1} = T(s_t, a_t),\,  a_t = \pi(s_t) \right]
$$

Using that the expected value is linear,
one can redo the reasoning from the deterministic case
to arrive at a _stochastic Bellman equation_:

$$
\begin{array}{rl}
 v^\star(s) =
  \min\limits_{a} & c(s, a) + \gamma \E \left[v^\star(s') \right] \\
  \textrm{s.t.}  & s' = T(s, a), \\
                 & a \in \Actions(s).
\end{array}
$$

For finite state spaces,
this equation has the same contraction properties as the deterministic one.
Hence, the tools of value and policy iteration are also readily available to solve MDPs.
The optimization problems are harder, because of the expected value,
but conceptually it is the same.


End of our Journey
==================

Well, we finally reached the end of our overview of dynamic programming.
I sincerely hope it was as fun for you to read as it was for me to write.
And that DP gets the honor place it deserves in your problem solving toolkit![^fp-rec]

Of course, a single blog post is too tinny to encompass a subject as vast as DP.
There are matters about estimating the value function instead of simply calculating it,
infinite state spaces, continuous time and a plethora of cool stuff we can do.
There are also a lot of connections with reinforcement learning,
for which we only scraped the surface in this post.
Unfortunately, these will remain as stories for another night.

Farewell and see you next time!

[^fp-rec]: In fact, fixed points and recursion as a whole deserve the spot.
They're everywhere!


Acknowledgements
================

This post come to life after a series of conversations I had
with [Pedro Xavier](https://pedromxavier.github.io).
The good thing of explaining something to a smart person
is that you end up learning a lot in the process.
Sometimes you even learn enough to write a blog post about it.

I'm also in debt with Ivani Ivanova for being such a great typo hunter.
If there is any typo left, it is because I'm lazy... She did a marvelous job.

Appendix (Convergence in Infinite Horizon) {#appendix}
================================

In this appendix we show that the Bellman Operator

$$
\begin{array}{rl}
   (\Bellman v)(s) =
    \min\limits_{a} & c(s, a) + \gamma v(s') \\
    \textrm{s.t.}  & s' = T(s, a), \\
                   & a \in \Actions(s)
\end{array}
$$

is a _monotone contraction_ over the space of bounded continuous functions.

We begin our proof with _monotonicity_.
For that, let's introduce a partial order on the space of value function $\States \to \R$
given via uniform ordering on all states,

$$ v \le w \iff \forall s \in \States,\, v(s) \le w(s).$$

::: {.Theorem data-title="Monotonicity"}
The Bellman Operator preserves uniform ordering of value functions:

$$v \le w \implies \Bellman v \le \Bellman w.$$
:::

:::Proof
The hypothesis $v \le w$ implies
for any state $s$ and action $a$ that

$$ c(s, a) + \gamma v(T(s,a)) \le c(s, a) + \gamma w(T(s,a)). $$

Since this is valid for any $a$,
taking the minimum on both sides preserves the inequality.

$$
\min_{a \in \Actions(s)} c(s, a) + v(T(s,a)) \le \min_{a \in \Actions(s)} c(s, a) + w(T(s, a)) \\
(\Bellman v)(s) \le (\Bellman w)(s).
$$

The line above is valid for all states, thus concluding the proof.
:::

Another important property of $\Bellman$ is that uniform translations
of the input $v$ also translate the output uniformly.

:::Theorem
For any constant $k$, $\Bellman(v + k) = \Bellman v + \gamma k$.
:::

:::Proof

$$ \begin{array}{rlll}
      \Bellman(v + k)(s) &= &\min\limits_{a} & c(s, a) + \gamma (v(s') + k) \\
      &&\textrm{s.t.}  & s' = T(s, a), \\
      &&               & a \in \Actions(s) \\
      &=&  \min\limits_{a} & c(s, a) + \gamma v(s') + \gamma k\\
      &&\textrm{s.t.}  & s' = T(s, a), \\
      &&               & a \in \Actions(s).
    \end{array}
$$

Since the term $\gamma k$ does not depend on the action $a$,
we may take it out of the optimization,

$$ \Bellman(v + k)(s) = \Bellman(v)(s) + \gamma k.$$

This concludes the theorem.
:::


Finally, let's prove that the Bellman operator
contracts the space of bounded continuous functions by the discount factor.

::: {.Theorem data-title="Contraction"}
the Bellman Operator contracts the space of continuous bounded functions
with Lipschitz constant $\gamma$,

$$ \|\Bellman v - \Bellman w\|_\infty \le \gamma \|v - w\|_\infty.$$

When $\gamma < 1$, it is a contraction.
:::

:::Proof

From the definition of the uniform norm, we get that for any state $s$,

$$
v(s) - w(s) \le \|v - w\|_\infty \\
v(s) \le w(s) + \|v - w\|_\infty.
$$

From the monotonicity we just proved,
applying $\Bellman$ to both sides preserves this inequality:

$$ (\Bellman v)(s) \le \Bellman(w + \|v - w\|_\infty)(s). $$

And since the right-hand side above has a uniform translation,
we can take the constant out:

$$ \begin{aligned}
      (\Bellman v)(s) &\le (\Bellman w)(s) + \gamma \|v - w\|_\infty \\
      (\Bellman v)(s) - (\Bellman w)(s) &\le \gamma \|v - w\|_\infty.
    \end{aligned}
$$

Using that the norm is symmetric, we can do the same derivation
in the opposite direction (for $w - v$)
to get an inequality for the absolute value.
Finally, applying the supremum, it becomes the result we want.

$$ \begin{aligned}
      |(\Bellman v)(s) - (\Bellman w)(s)| &\le \gamma \|v - w\|_\infty \\
      \sup_{s\in\States} |(\Bellman v)(s) - (\Bellman w)(s)| &\le \gamma \|v - w\|_\infty \\
      \|\Bellman v - \Bellman w\|_\infty &\le \gamma \|v - w\|_\infty.
  \end{aligned}
$$
:::

Finally, from the Banach fixed point theorem
and the above, we conclude that whenever $\gamma < 1$, the operator $\Bellman$ has a unique fixed point.
Hence, any decision process with a discount factor is solvable and has a unique optimal value function $v^\star$.
