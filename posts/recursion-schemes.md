---
title: 'Recursion Schemes: Applying Greek to Programming'
tags: category-theory recursion-schemes functional-programming
---

\DeclareMathOperator{\fat}{fat}

## A not so historical introduction

### Imperative programming

We begin our journey in the prehistory of imperative programming;
that time when computers where still made of coconuts and mammoth fur.
At the time,
the way to program was quite _unestructured_.
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
this is the _only_ command responsible for the program's control flow.

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
For example,
in a language like C, there are `struct` types, `union` types and arrays.

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
The benefit lies in the _programmer's expressivenes_.
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

### Functional Programming

So far so good for the development of imperative programming,
but the world of programming languages is not so uniform.
Other paradigms exist.
And the one that mostly fits in what we will see today is
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
which is achieved via _recursion_.

A recursive function is a function that,
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
It is easy to write a functional spaghetti code if recursion is used too indiscriminately.
Because of all its power, recursive code lacks in safety.
It would even be fair to say that, like `goto`, it is too _unestructured_.

##  Algebraic Data Types

List, Nat, some trees

# Functors and their fixed points

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
