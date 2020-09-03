---
title: 'Recursion Schemes: Applying Greek to Programming'
tags: category-theory recursion-schemes functional-programming
---

\DeclareMathOperator{\fat}{fat}

Recursion schemes are a neat construction that allows one
to better organize and reason about recursive functions.
Instead of viewing recursion as functions that call themselves,
these schemes allow us to take a higher-order point of view.
Most of recursive definitions may be abstracted as some operator
that takes a function which knows how to deal with individual parts of a structure
and turns it into a function defined for the entire structure.
The canonical example for this perspective is turning the binary function $+$,
which adds two numbers,
into a new function that sums all elements of a list with arbitrary length[^sum].

[^sum]: This sum example is simple to code with a loop or linear recursion.
Recursion schemes really shine when manipulating more complex data structures,
such as trees with varying arity.

The first time I heard about recursion schemes was after stumbling
with the paper [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf) by Erik Meijer, Maarten Fokkinga, and Ross Paterson.
It is a real gem of functional wisdom but the authors choice of using the
[Squiggol](https://en.wikipedia.org/wiki/Bird%E2%80%93Meertens_formalism) notation
made some parts really hard to decipher.
Fortunately,
I later found [Patrick Thomson's excellent series of posts](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html),
which explain recursion schemes using Haskell code.
After absorbing this content,
I tried to explain the idea to some friends who don't know Haskell
but know some category theory.
This post is an enriched version of that presentation
with more cohesion and much less of my bad drawings.

In the spirit of the original presentation,
I've decided to make this post completely language-agnostic[^cd].
I also tried to keep any type theory or lambda calculus to a minimum
in order to make it easier to grasp.
My hope is that anyone with some experience in programming and mathematics
will be able to read this,
instead of confining this post only to people that know a certain programming language.
Of course, in doing that I'm risking that no one will understand this.
But, very well, I will do my best.

[^cd]: Well, in terms of programming languages. There will be commutative diagrams, be warned.

## A not so historical introduction

We begin our journey in the prehistory of programming;
that time when computers where still mostly made of coconuts and mammoth fur.
At the time,
the way to program was quite unstructured.
The code of a program was essentially given by a sequence of commands
whereas the data was simply piled up in a stack.

    +---+---+---+---+---+---+---+---+---+---+---+          +---+
    |   |   |   |   |   |   |   |   |   |   |   |          |   |
    +---+---+---+---+---+---+---+---+---+---+---+          +---+
     Control Flow                                          |   |
                                                           +---+
                                                           |   |
                                                           +---+
                                                           |   |
                                                           +---+
                                                           Stack

But the program does not have to execute the instructions in order.
No, it would be rather boring and inexpressive...
In this programming paradigm,
there is a special command called `goto`
which allows one to jump to any labeled part of the program.
In fact,
this is the only command responsible for the program's control flow.

           +-----------------------------+
           |   +--------------+          |
           |   |              |          |
           v   |              v          |
    +---+---+----+---+---+---+---+---+----+---+---+
    |   |   |goto|   |   |   |   |   |goto|   |   |
    +---+---+----+---+---+---+---+---+----+-+---+-+
      |  ^ |  ^                |  ^ |  ^
      |  | |  |                |  | |  |
      +--+ +--+                +--+ +--+

The `goto` is a pretty powerful construction,
but it also has its caveats.
So much for its use to be [considered harmful](https://dl.acm.org/doi/10.1145/362929.362947).
Although any kind of program flow can be constructed using only `goto`,
it may become too confuse.
The `goto` statement makes it too easy to write spaghetti code,
which is practically impossible to debug.

After prehistory,
we arrive into the societal period of imperative programming.
In here,
the programming languages become _structured_.
Data is no long viewed as simply a stack of memory
but classified into types.
There are some primitive types
as well as structures to combine them into more complex types.
In a language like C, for example,
there are `struct` types, `union` types, and array types.

The changes also arrived to the control flow
and an effort was made to tame the once wild `goto`.
The programmers from that time analysed its most common use cases
and created new statements fulfill each of these.
You are probably acquainted to them as
`for` loops,
`while` loops,
`switch/case` statements,
function and subroutine declarations
and `if/else` statements;
just to name a few.

Both in regards to data and control flow,
the movement we encounter in here
consists of substituting general purpose structures
that concretely represent the instructions we are given to the processor
for more specific structures,
having more abstract roles in the code.
In terms of computational expressiveness,
nothing changes.
What the processor sees is the same in both unstructured and structured programming.
The benefit lies in the programmer's expressiveness.
With more specific statements,
it becomes easier to write larger, more complex programs
as well as properly debug them.
As an example,
we may notice that it is possible to guarantee that a `for` loop
always terminates if it is iterating over a block of code that is known to terminate.
No strange thing can happen.
On the other side,
the general character of the `goto` allows us to simulate the behavior of a `for`
but there is no guarantee that a program with a `goto` statement must terminate.

Until now, we were looking at a programming paradigm called _imperative programming_,
but the world of programming languages is not so uniform.
Other paradigms exist.
And the one that mostly fits what we will see today is called
_functional programming_.

While the imperative paradigm views the code as a sequence of commands
the programmer gives the computer to execute (hence the name, _imperative_),
the functional paradigm views the code as a composition of functions in the mathematical sense.
A function takes a value as input, does some processing to it, calling other functions for it,
and returns another value as output.

        +---+             +---+             +---+
    ----| f |------------>| g |------------>| h |------
        +---+             +---+             +---+

If every program consists only of applying a finite amount of previously defined functions,
the language's expressiveness seems rather limited.
To overcome this, we need some form of control flow,
which is achieved via recursion.

A _recursive function_ is a function that,
in order to process its input into the output,
may call itself in an intermediate step.
Probably the most famous recursive function is the factorial,
defined as
$$ \fat(n) = \begin{cases}
        1,& n = 0 \\
        n \cdot \fat(n-1),& \text{otherwise}.
    \end{cases}
$$

The expressiveness gained from recursion is essentially the same as
the one gained from `goto` in imperative languages.
That is, the control flow given by function composition together with recursion
allows one to do anything imaginable with the programming language.
However,
all this expressiveness comes with its caveats.
It is too easy to write a functional spaghetti code if recursion is used indiscriminately.
Because of all its power, recursive code lacks in safety.
It would even be fair to say that, like `goto`, it is too unstructured.

The idea of structured control flow really caught on in the imperative world.
One hardly sees a wild `goto` in the middle of a modern piece of code.
In fact, many languages don't even allow it.
In the functional world, on the other side,
the trendy for taming recursion never really caught on.
Despite many functional languages organizing their data using types,
control is still done using crude recursion.

Recursion schemes are ways to organize and structure
different kinds of recursive functions.
Instead of writing a recursive function in terms of itself,
we define higher order functions
that receive an ordinary function as argument,
do some recursive magic on it,
and return a recursive analogue of that function as output.
It is similar to how a `while` loop takes a boolean statement
and a block of code,
and turns them into a repeating block of code.
If it seems too confusing, just keep on.
What I mean in here will become clearer after we [construct catamorphisms](#sec:f-algebras).

Before we end this motivation and proceed to the actual construction of recursion schemes,
there is an intuition that, I think, is useful to have in mind.
In imperative programming,
there is a close relationship between how we structure data and how we structure the control flow.
Working with arrays almost asks the programmer to design programs with `for` loops.
Similarly, it is natural to deal with `union` types using `switch` statements
(also called `case` or `cond` in some languages).
Recursion schemes will arise as an analogous to this idea in the functional programming setting.
So far so good for motivation, let's dive into some math.

##  Algebraic Data Types {#sec:ADT}

List, Nat, some trees

# Functors and their fixed points {#sec:f-algebras}

F-algebras

<div class="theorem">
    Lasagna.
</div>
<div class="proof">
So that is...
</div>

# The good side of a catastrophe

\begin{tikzpicture}

\def \n {5}
\def \radius {3cm}
\def \margin {8} % margin in angles, depends on the radius

\foreach \s in {1,...,\n}
{
  \node[draw, circle] at ({360/\n * (\s - 1)}:\radius) {$\s$};
  \draw[->, >=latex] ({360/\n * (\s - 1)+\margin}:\radius)
    arc ({360/\n * (\s - 1)+\margin}:{360/\n * (\s)-\margin}:\radius);
}
\end{tikzpicture}

Before proceeding, I must allow myself a little rant:
mathematicians and theoretical computer scientists
are simply _terrible_ name-givers.
What in hell is a catamorphism?
Sometimes, when I am working with other, more applied, fields
###
_extreme gradient boosting_ or _uncertainty principles_.
At the same time,
a super cool concept such as the catamorphism requires one to know ancient Greek in order to get some intuition.

Luckily for me, I like ancient Greek.
Thus, let's take a break from the mathematics
to unravel the catamorphism from an etymological point of view.

# Growing your lists up

anamorphisms

# Dividing to conquer

hylomorphisms

# Summary

# Bibliography
