---
title: Picturing Finite Relations as Graphs
keywords: [math, theory]
date: 2023-03-24
---

```{=tex}
\usetikzlibrary{graphs}

\tikzset{
  every node/.style = {circle, draw=black, thin, outer sep=1mm, minimum size=2mm},
  every edge/.style = {{Round Cap}-Kite, draw},
  every loop/.style = {{Round Cap}-Kite, draw},
  unfocused/.style  = {color = blue, opacity = 0.2},
}
```

- Reflexivity: Every vertex has a self-edge

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

\path[->] (A) edge[loop left] ()
          (B) edge[loop above] ()
          (C) edge[loop right] ()
          (D) edge[loop right] ();
```


- Transitivity: Connected vertices are also adjacent.

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

- Symmetry: the graph is undirected

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

- Antisymmetry: All cycles are self-edges

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

- Connected: there is an edge between all vertices (no matter the direction)

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

- Strict: no self-edges

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

- Totality: Every vertex has an outgoing edge.

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
    (D) edge (B)
    (D) edge[loop above] (D)
    (E) edge[loop right] (E);
```


- Deterministic: Each vertex has at most one outgoing edge.

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
    (D) edge (B)
    (D) edge[loop above] (D)
    (E) edge[loop right] (E);
```

Equality
========

```tikz
\node [fill = cyan!20         ] (A) []                       {};
\node [fill = orange!50       ] (B) [above right =     of A] {};
\node [fill = red!30!blue!50  ] (C) [below right = 2cm of A] {};
\node [fill = yellow!90!black ] (D) [right       = 4cm of A] {};
\node [fill = green!50!cyan!50] (E) [left        = 2cm of A] {};

\path[->] (A) edge[loop above] ()
          (B) edge[loop above] ()
          (C) edge[loop above] ()
          (D) edge[loop above] ()
          (E) edge[loop above] ();
```

Equivalence Relations
=====================

Transitive, Symmetric and Reflexive

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
          (B) edge (A);

\path[->] (C) edge (D)
          (C) edge (E)
          (D) edge (C)
          (D) edge (E)
          (E) edge (C)
          (E) edge (D);
```

Preorders
=========

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
==============

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
============

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
=========
