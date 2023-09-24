---
title: Picturing Finite Relations as Graphs
keywords: [math, theory]
date: 2023-08-24
---

On the previous post about [Algebraic Path Finding](/posts/algebraic-path),
I made some figures illustrating how a finite relation can be viewed a graph.
This got me thinking about how certain usual notions on relations would
translate to this graph world.

This is a mostly visual post illustrating the graphs for many common kinds of relations.

Common Properties
=================

```{=tex}
\usetikzlibrary{graphs}

\tikzset{
  every node/.style = {circle, draw=black, thin, outer sep=1mm, minimum size=2mm},
  every edge/.style = {{Round Cap}-Kite, draw},
  every loop/.style = {{Round Cap}-Kite, draw},
  unfocused/.style  = {color = blue, opacity = 0.2},
}
```

- **Reflexivity**: Every vertex has a self-edge.

```tikz
\node [fill = cyan!20         ] (A) []                       {};
\node [fill = orange!50       ] (B) [right       =     of A] {};
\node [fill = red!30!blue!50  ] (C) [below right = 1cm of A] {};
\node [fill = yellow!90!black ] (D) [right       = 2cm of A] {};

\path[->, unfocused]
    (A) edge[unfocused] (B)
    (B) edge[unfocused] (C)
    (B) edge[unfocused] (D)
    (C) edge[unfocused] (A);

\path[->] (A) edge[loop left ] ()
          (B) edge[loop above] ()
          (C) edge[loop right] ()
          (D) edge[loop right] ();
```


- **Transitivity**: Connected vertices are also adjacent.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [right = of A] {};
\node [fill = red!30!blue!50  ] (C) [right = of B] {};
\node [fill = yellow!90!black ] (D) [above = of B] {};

\path[->, unfocused]
    (A) edge (B)
    (B) edge (C)
    (D) edge (B);

\path[->]
    (A) edge[bend right] (C)
    (D) edge[bend left] (C);

```

- **Symmetry**: the graph is undirected.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [right = of A] {};
\node [fill = red!30!blue!50  ] (C) [above right = 1cm of B] {};
\node [fill = yellow!90!black ] (D) [below right = 1cm of B] {};

\path[]
    (A) edge [loop left] ()
    (A) edge (B)
    (B) edge (A)
    (B) edge (C)
    (C) edge (B)
    (B) edge (D)
    (D) edge (B)
    (C) edge (D)
    (D) edge (C);
```

- **Antisymmetry**: All cycles are self-edges.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [above right = 1cm of A] {};
\node [fill = red!30!blue!50  ] (C) [below right = 1cm of A] {};
\node [fill = yellow!90!black ] (D) [right       = 2cm of A] {};

\path[unfocused]
    (A) edge (B)
    (A) edge (C)
    (B) edge (D)
    (C) edge (D);

\path (D) edge[loop right] ()
      (A) edge[loop left]  ();
```

- **Connected**: there is an edge between all vertices (no matter the direction)

```tikz
\node [fill = cyan!20         ] (A) []                       {};
\node [fill = orange!50       ] (B) [above left  = 1cm of A] {};
\node [fill = red!30!blue!50  ] (C) [below left  = 1cm of A] {};
\node [fill = yellow!90!black ] (D) [right       = 1cm of A] {};

\path[]
    (A) edge (B)
    (A) edge (C)
    (B) edge (D)
    (B) edge (C)
    (C) edge (B)
    (D) edge (C)
    (D) edge (A);

\path (A) edge[loop left]   ()
      (B) edge[loop left]   ()
      (C) edge[loop left]   ()
      (D) edge[loop right]  ();
```

- **Strict**: no self-edges.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [right = of A] {};
\node [fill = red!30!blue!50  ] (C) [above right = 1cm of B] {};
\node [fill = yellow!90!black ] (D) [below right = 1cm of B] {};

\path[]
    (A) edge (B)
    (B) edge (C)
    (B) edge (D)
    (D) edge (B);
```

- **Totality**: Every vertex has an outgoing edge.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [right = of A] {};
\node [fill = red!30!blue!50  ] (C) [above right = 1cm of B] {};
\node [fill = yellow!90!black ] (D) [below right = 1cm of B] {};
\node [fill = green!50!cyan!50] (E) [right       = of B] {};

\path[]
    (A) edge (B)
    (A) edge[bend left] (C)
    (B) edge (C)
    (B) edge (D)
    (C) edge (E)
    (D) edge (B)
    (D) edge[loop above] (D)
    (E) edge[loop right] (E);
```

- **Deterministic**: Each vertex has at most one outgoing edge.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [right = of A] {};
\node [fill = red!30!blue!50  ] (C) [above right = 1cm of B] {};
\node [fill = yellow!90!black ] (D) [below right = 1cm of B] {};
\node [fill = green!50!cyan!50] (E) [right       = of B] {};

\path[]
    (A) edge[bend left] (C)
    (B) edge (C)
    (D) edge[loop above] (D);
```

Common Relations
================

Equality
--------

Possibly the most famous relation among all is equality.
An element is only equal to itself,
meaning that there are only self-edges.

```tikz
\node [fill = cyan!20         ] (A) []                       {};
\node [fill = orange!50       ] (B) [above right =     of A] {};
\node [fill = red!30!blue!50  ] (C) [right       = 2cm of A] {};
\node [fill = yellow!90!black ] (D) [right       =     of C] {};
\node [fill = green!50!cyan!50] (E) [above right =     of C] {};

\path[->] (A) edge[loop below] ()
          (B) edge[loop right] ()
          (C) edge[loop left] ()
          (D) edge[loop right] ()
          (E) edge[loop above] ();
```

Equivalence Relations
---------------------

_Transitive, Reflexive and Symmetric._

Sometimes it is too strong to require equality of objects.
It may be more interesting to look at things that are _equivalent_.

In an equivalence relation, the vertices are clustered into equivalence classes.
An edge exists if and only if the vertices are on the same class.

```tikz
\node [fill = cyan!20         ] (A) []                       {};
\node [fill = orange!50       ] (B) [above right =     of A] {};
\node [fill = red!30!blue!50  ] (C) [right       = 2cm of A] {};
\node [fill = yellow!90!black ] (D) [right       =     of C] {};
\node [fill = green!50!cyan!50] (E) [above right =     of C] {};

\path[->] (A) edge[loop below] ()
          (B) edge[loop right] ()
          (C) edge[loop left] ()
          (D) edge[loop right] ()
          (E) edge[loop above] ();

\path[->] (A) edge (B)
          (B) edge (A)
          (C) edge (D)
          (C) edge (E)
          (D) edge (C)
          (D) edge (E)
          (E) edge (C)
          (E) edge (D);
```

Preorders
---------

_Transitive and Reflexive._

These are graphs where every path corresponds to an edge.
That is, if two vertices are connected, then there is an edge between them.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [right = of A] {};
\node [fill = red!30!blue!50  ] (C) [right = of B] {};

\path[->] (A) edge[loop left]  ()
          (B) edge[loop above] ()
          (C) edge[loop right] ()
          (A) edge[bend right=90] (C);


\path[->] (A) edge[bend left] (B)
          (B) edge[bend left] (A)
          (B) edge (C);

```

Preorders contain all self-edges and composite edges.
Thus, in order to reduce all the noise created by edges that must be there anyway,
it is common to only picture the essential edges that cannot be generated from other paths.
This is called the relation's transitive reduction or its **Hasse Diagram**.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [right = of A] {};
\node [fill = red!30!blue!50  ] (C) [right = of B] {};

\path[->, unfocused]
    (A) edge[loop left]  ()
    (B) edge[loop above] ()
    (C) edge[loop right] ()
    (A) edge[bend right=90] (C);

\path[->] (A) edge[bend left] (B)
          (B) edge[bend left] (A)
          (B) edge (C);
```

Partial Orders
--------------

_Transitive, Reflexive and Antisymmetric._

By disallowing cycles in a preorder, what get what is called a partial order.
These graphs flow in a certain direction,
since a path cannot pass through the same vertices twice.

```tikz
\node [fill = cyan!20         ] (A) []                       {};
\node [fill = orange!50       ] (B) [above right =     of A] {};
\node [fill = red!30!blue!50  ] (C) [below right =     of A] {};
\node [fill = yellow!90!black ] (D) [right       =     of B] {};
\node [fill = green!50!cyan!50] (E) [right       =     of C] {};
\node [fill = green!70!cyan!50] (F) [right       = 4cm of A] {};

\path[->]
    (A) edge[loop left] ()
    (B) edge[loop left] ()
    (C) edge[loop below] ()
    (D) edge[loop below] ()
    (E) edge[loop right] ()
    (F) edge[loop right] ()
    (A) edge (D)
    (A) edge (E)
    (A) edge (F)
    (B) edge[bend left=100] (F);

\path[->] (A) edge (B)
          (A) edge (C)
          (B) edge (D)
          (C) edge (E)
          (D) edge (F);
```

Again, to prevent all the noise, it is useful to look at the Hasse Diagram.

```tikz
\node [fill = cyan!20         ] (A) []                       {};
\node [fill = orange!50       ] (B) [above right =     of A] {};
\node [fill = red!30!blue!50  ] (C) [below right =     of A] {};
\node [fill = yellow!90!black ] (D) [right       =     of B] {};
\node [fill = green!50!cyan!50] (E) [right       =     of C] {};
\node [fill = green!70!cyan!50] (F) [right       = 4cm of A] {};

\path[->, unfocused]
    (A) edge[loop left] ()
    (B) edge[loop left] ()
    (C) edge[loop below] ()
    (D) edge[loop below] ()
    (E) edge[loop right] ()
    (F) edge[loop right] ()
    (A) edge (D)
    (A) edge (E)
    (A) edge (F)
    (B) edge[bend left=100] (F);

\path[->] (A) edge (B)
          (A) edge (C)
          (B) edge (D)
          (C) edge (E)
          (D) edge (F);
```

Total Orders
------------

_Transitive, Reflexive, Antisymmetric and Connected._

For these orders, there is also the requirement of existing
and (undirected) edge between any pair of vertices.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [right = of A] {};
\node [fill = red!30!blue!50  ] (C) [right = of B] {};
\node [fill = yellow!90!black ] (D) [right = of C] {};

\path[->] (A) edge[loop left]  ()
          (B) edge[loop below] ()
          (C) edge[loop above] ()
          (D) edge[loop right] ();

\path[->] (A) edge (B)
          (A) edge[bend left] (C)
          (A) edge[bend left] (D)
          (B) edge (C)
          (B) edge[bend right] (D)
          (C) edge (D);

```

They are also called **Linear Orders**
because their Hasse Diagram consists of a single path.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [right = of A] {};
\node [fill = red!30!blue!50  ] (C) [right = of B] {};
\node [fill = yellow!90!black ] (D) [right = of C] {};

\path[->, unfocused]
    (A) edge[loop left]  ()
    (B) edge[loop below] ()
    (C) edge[loop above] ()

    (A) edge[bend left] (C)
    (A) edge[bend left] (D)
    (B) edge[bend right] (D)
    (D) edge[loop right] ();

\path[->] (A) edge (B)
          (B) edge (C)
          (C) edge (D);

```

Functions
---------

_Deterministic and Total._

How could I end this post without illustrating the most pervasive kind of relation
in the entire field of mathematics?
I'm so used to looking at functions as maps that I even forget that they are relations.

Functions are graphs where all vertices have a unique outgoing edge.

```tikz
\node [fill = cyan!20         ] (A) []             {};
\node [fill = orange!50       ] (B) [right = of A] {};
\node [fill = red!30!blue!50  ] (C) [below = of B] {};
\node [fill = yellow!90!black ] (D) [below = of A] {};


\path[->] (A) edge[] (B)
          (B) edge[] (C)
          (C) edge[bend right] (B)
          (D) edge[] (C);

```
