---
title: 'Recursion Schemes: Applying Greek to Programming'
tags: category-theory recursion-schemes functional-programming
---

\def\op#1{\operatorname{\mathrm{#1}}}
\def\catC{\mathcal{C}}
\def\abs#1{\left|#1\right|}
\DeclareMathOperator{\fat}{fat}
\DeclareMathOperator{\maybe}{\mathtt{maybe}}
\DeclareMathOperator{\true}{true}
\DeclareMathOperator{\false}{false}
\def\Bool{\mathtt{Bool}}
\def\nil{\op{nil}}
\def\cons{\op{cons}}
\def\N{\mathbb{N}}
\def\Z{\mathbb{Z}}
\def\R{\mathbb{R}}

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

The simplest form to write a type is by enumerating its elements
such as
$$\Bool \coloneqq \true \mid \false.$$
This way,
we define the type of boolean values.
That is, a type with exactly two terms called $\true$ and $\false$.
In general any finite type can be written just by enumerating its elements.
As another example, there is the type of musical notes
$$ \mathtt{Notes} \coloneqq
    \mathtt{do} \mid
    \mathtt{re} \mid
    \mathtt{mi} \mid
    \mathtt{fa} \mid
    \mathtt{sol}\mid
    \mathtt{la} \mid
    \mathtt{si}.$$
Nevertheless, in practice we also want to deal with types that are more complex than simply
finite collections.
The solution to this is assuming that the programming language comes with some built-in types
such as integers, characters, and floating-point numbers,
together with structures that allow us to compose these types.

One common compound type consists of a structure capable of storing
data of more than one type at the same time.
As an example, let's say we are in a spacial war against many alien species.
Your job is to catalogue how many battleships each army has.
One way to store this data is with a type containing a string for the species which the army belongs
together with a natural number for how many battleships they have.
We will write this type as
$$\mathtt{Army} \coloneqq \op{ships}\; \mathtt{String} \times \N.$$
Here, $\op{ships} \colon \mathtt{String} \times \N \to \mathtt{Army}$
is called a _constructor_ for the type $\mathtt{Army}$.
Constructors are (possibly multivariate) functions that receive terms as arguments
and return an term of a compound type.

After you finish your catalogue,
orders arrive for you to develop a new laser cannon to be used in the war.
This weapon should scan the sky to find the enemy armies' positions (modeled in a type $\mathtt{Pos}$ and shoot them.
But beware! There are also allied bases around, encapsulated in the type $\mathtt{Base}$.
Since friendly fire is really far from the ideal,
our target type should have two different constructors,
one for allies and another for enemies:
$$\mathtt{Target} \coloneqq \op{ally}\; \mathtt{Position} \times \mathtt{Base}
                  \mid \op{enemy}\; \mathtt{Position} \times \mathtt{Army}.$$
In here we extended our notation for enumerating types to also accept constructors.
Different constructors always produce different terms of the compound type,
thus, we may view the functions
$\op{ally} \colon \mathtt{Position} \times \mathtt{Base} \to \mathtt{Target}$
and $\op{enemy} \colon \mathtt{Position} \times \mathtt{Army} \to \mathtt{Target}$
as tags representing from which type our $\mathtt{Target}$ was constructed.
This works as if we are storing a element of type $\mathtt{Army}$ together with a tag $\op{enemy}$
on the type $\mathtt{Target}$.
So the definition of target is saying that its terms are of the form $\op{ally}(p, x)$
or $\op{enemy}(p,x)$,
just like we previously enumerated the terms of finite types.

This point of view allows us to define functions on compound types
by enumerating what it does on the different constructors,
a method called _pattern matching_.
For example, the function $\op{not} \colon \Bool \to \Bool$
is defined as
$$ \begin{aligned}
    &\op{not} \true &=&\; \false& \\
    &\op{not} \false&=&\;\true.&
\end{aligned}$$
While our ally-aware cannon shooting function may be defined as something like
$$ \begin{aligned}
    &\op{shoot}(\op{enemy}(p, y)) &=&\; \mathtt{laser\_blast}(p) \\
    &\op{shoot}(\op{ally}(p, x)) &=&\; \mathtt{wave\_your\_hand}(p).
\end{aligned}$$
Taking advantage of the type system via pattern matching
is a nice way to make it obvious that our code does what it should.
In this case, the types don't allow us to obliterate our friends.

Although these compound types are nice to organize our data,
the true power of types comes from taking advantage of two other constructions:
_function types_ and _fixed point types_.
The function type between two types $A$ and $B$,
represents all the functions receiving an input of type $A$ and returning an argument of type $B$.
To mimic the usual notation for type signatures,
this function type is denoted as $A \to B$.
A type system with function types allows us to define higher-order functions.
For example, given a function $\phi \colon A \to B$,
the composition operator $K_\phi$ defined as
$$ K_\phi f = f \circ \phi$$
has type signature $K_\phi \colon (B \to C) \to (A \to C)$.
It takes a function and turns it into another one.
This was a simple example but be sure that many more will soon come.
Higher-order functions are at the heart of recursion schemes.

Now we arrive at the last kind of compound type we are going to discuss today:
fixed point types.
These are the star of today's show, so pay attention.
We may define a compound data type that is parameterized by some variable
(a function between types, if you prefer) such as
$$\maybe A \coloneqq \op{nothing} \mid \op{just} A.$$
Given a type $A$, an term of type $\maybe A$
is either $\op{nothing}$ or the constructor $\op{just}$
applied to a term of type $A$.
Thus, $\maybe$ receives a type and augments it with a special point[^maybe].

[^maybe]: This is also called `Option` in some languages and is specially useful to safely model partial functions and computations that may fail.

Given a type operator $F$,
a fixed point of $F$ is a type $X$ satisfying
$$X \simeq FX.$$
In here, we use the isomorphism sign $\simeq$ instead of equality
because there is some boilerplate involved regarding tags.
Although this is a simple construction,
the concept is extremely powerful,
being directly connected to recursion.

As an example let's explore the fixed point of $\maybe$.
It is a type $N$ satisfying
$$N \simeq \maybe N = \op{nothing} \mid \op{just} N.$$
This means that any term of $N$ is either $\op{nothing}$.
or $\op{just}$ a term of $N$.
Since we know $\op{nothing}$,
we can construct a new term $\op{just}(\op{nothing})$,
then $\op{just}(\op{just}(\op{nothing}))$,
and proceed successively in this way.
Thus for any natural number $n$,
applying $\op{just}$ to $\op{nothing}$ $n$ times
defines a unique term of $N$.
Moreover, the definition of $N$ says that all of its terms are of this form,
meaning that $N$ is isomorphic to the natural numbers[^nat].

[^nat]: Choosing the letter $N$ was no accident.

If you find this last result strange,
remember that the natural numbers are inductively defined
as being either zero or the successor of another natural number.
Using our notation for types, this is equivalent to saying
$$\N = \op{zero} \mid \op{succ} \N.$$
If we alter the tag names,
this tells us that $\N$ is a fixed point of $\maybe$ as expected.

### An example with lists

The natural numbers are surely the most famous example of an inductive type.
Nevertheless,
almost no one thing of them like that while programming.
Treating $3$ as $\op{succ}(\op{succ}(\op{succ}(\op{zero})))$
would be cumbersome, to say the least.
Any language comes with a (possibly signed) integer type
already bundled with the usual arithmetic operations defined for it.
Thus, let's discuss a little bit about another inductive type
that is also pretty famous but has more of a inductive data structure flavour to it:
the _linked list_.

Let $A$ be your favorite type.
Intuitively, a list over $A$ is a finite-length sequence of elements of $A$.
The simplest case possible is an empty list.
To represent a non-empty list,
we notice that any non-empty list with $n$ elements
may be "factorized" into its first element and another list with the remaining $n-1$ elements.

    +-------+               +-------+               +-------+               +-------+               +---+
    |   |   +-------------->|   |   +-------------->|   |   +-------------->|   |   +-------------->+   |
    +-------+               +-------+               +-------+               +-------+               +---+

Thus, by defining a type operator $P$ as
$$P X \coloneqq \nil \mid \cons\, A \times X.$$
The previous discussion shows that the type of lists over $A$,
hereby denoted $L(A)$ is a fixed point of $P$,
$$L(A) \simeq \nil \mid \cons\, A \times L(A).$$
Here,
the constructor $\nil$ takes the role of the empty list
while the constructor $\cons$ represents a pair
with an element of $A$ and another list.

This inductive definition of lists
allows us to recursively define operations over it.
For example,
let's construct a function that sums all the elements in a list of integers.
Its signature is $\op{sum} \colon L(\Z) \to \Z$.
If the list is empty, the sum returns zero.
Otherwise, it takes the first element and adds it to the sum of what remains,
$$ \begin{aligned}
\op{sum}(\nil) &= 0, \\
\op{sum}(\cons(x, l)) &= x + \op{sum}(l).
\end{aligned}$$
This last definition conceals an extremely useful pattern.
It is the programming analogue of a proof by induction over the list.
The empty list is the base case, which we set to zero.
On a list of length $n>0$, we assume that we've already solved the computation
for the sublist of length $n-1$ and them add it to the remaining element.
Just like an inductive proof, see?

As it stands,
you would be right to guess that constructing functions in this
way is so pervasive in programming as proofs by induction are in mathematics.
A simple variation of $\op{sum}$ would be a function that multiplies a list of real numbers,
$\op{prod} \colon L(\R) \to \R$.
To construct it, all we need to do is take the definition of $\op{sum}$,
replace $0$ by $1$, replace addition by multiplication,
and it is all done!
$$ \begin{aligned}
\op{prod}(\nil) &= 1, \\
\op{prod}(\cons(x, l)) &= x \cdot \op{prod}(l).
\end{aligned}$$
As expected, this pattern appears every time
we want to somehow combine the elements of a list into a single value.
It is so common that people have abstracted it
on a function called $\op{reduce}$[^foldr].
Its type signature is
$$\op{reduce} \colon B \times (A \times B \to B) \to (L(A) \to B).$$
Pretty scary, right?
Let's break it down to see how it is just good ol' induction.
When reducing a list of type $L(A)$ into a value of type $B$,
there are two situations we may encounter.
In the base case the list is empty, thus we must specify a value of type $B$ to be returned.
In the other steps,
we consider that we already know the solution for the sublist of length $n-1$,
which yields a value of type $B$,
and then need a rule to combine this value of the remaining term of type $A$
into another term of type $B$.
Thus, $\op{reduce}$ is an instance of what we call an _higher-order function_.
It is a machine that takes an initial value and a binary function
and outputs another function, now defined on lists.
We call it higher-order because it doesn't work with ordinary data, no,
it transforms simple functions into recursive ones!
Considering what it does, its definition is actually rather simple.
The result $h = \op{reduce}(v,g)$ is the function that does
$$ \begin{aligned}
h(\nil) &= v, \\
h(\cons(x,l)) &= g(x,h(l)).
\end{aligned}$$
Take a moment to absorb this definition, there really is a lot encapsulated on this.
First substitute $v$ by $0$ and $g$ by $+$ to see that it becomes $\op{sum}$.
Then, substitute $v$ by $1$ and $g$ by $\cdot$ to see that it becomes $\op{prod}$.
Finally, congratulate yourself because you just understood your first recursion scheme!

[^foldr]: Also known as `accumulate` or `foldr` in some languages. The 'r' means that this function folds a list from the right.

If the way $\op{reduce}$ was introduced
made you think that it is only used to collapse a list
into a primitive type,
you should know that it is much more ubiquitous than that.
Two of the most used list-manipulating functions in functional programming
are $\op{map}$ and $\op{filter}$. And, guess what, both are implementable in terms of $\op{reduce}$.
The first, $\op{map}$,
takes a function $f \colon A \to B$
and turns it into another function that applies $f$ elementwisely to a list.
Its type signature for $\op{map}$ is therefore
$$ \op{map} \colon (A \to B) \to (L(A) \to L(B)).$$
On the empty list, $\op{map}(f)$ does nothing since it has no elements.
On a $\cons$ node, it should apply $f$ to the element stored on it
and then proceed to apply $f$ to the list's tail.
Thus, the definition of $\op{map}$ in terms of $\op{reduce}$ is
$$ \begin{aligned}
g(x, l) &= \cons(f(x), l), \\
\op{map}(f) &= \op{reduce}(\nil, g).
\end{aligned} $$
If it is hard to wrap your head around this last definition,
try doing it step by step with a simple function such as $x^2$ and a small list such as $(1,2,3,4)$.
I promise you it will be enlightening.

The $\op{filter}$ function takes a predicate, represented as
$p \colon A \to \Bool$,
and outputs a function that filters a list.
That is removes all elements of the list for which $p$ is false.
Its type signature is thus
$$ \op{filter} \colon (A \to \Bool) \to (L(A) \to L(A)).$$
Since the empty list has no elements, there is nothing to do in this case.
In a $\cons(x,l)$, we should return it exactly if $p(x)$ is true
and return just $l$ otherwise.
We can assembly this in terms of $\op{reduce}$ as
$$ \begin{aligned}
h(x, l) &= \begin{cases}
    \cons(x,l),& \text{if } p(x) = \true \\
    l,& \text{if } p(x) = \false
\end{cases}\\
\op{reduce}(f) &= \op{reduce}(\nil, h).
\end{aligned}$$

I hope you liked these examples because the next step in our journey
is generalizing $\op{reduce}$ to any inductive datatype!
To achieve this, we must pass through the land of category theory.

## Functors and their fixed points {#sec:f-algebras}

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
