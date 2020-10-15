---
title: Projects
---

These are some of the projects
that I have been engaged in the last few years.
Mostly, the stuff in here is concerned with
optimization problems or mathematical modeling.

<header class="subheading">
<h2 id="convexification-by-averages">Convexification by Averages</h2>
<p>Master's thesis</p>
</header>

<img src="/img/cvxavg-thumbnail.svg"
     alt="Average of non-convex functions"
     title="Average of non-convex functions can be convex."
     width="95%"/>

This is a work in **non-convex stochastic optimization**.
The main concern consists of taking advantage of
the natural probabilistic nature of these problems
to make them more computationally tractable.

You can read the [thesis](data/mscthesis.pdf)
or just take a peek at the [defense's slides](data/mscslides.pdf)
to know what it is all about.

Throughout my Masters,
I've worked in two technical collaboration projects
between UFRJ and the Brazilian Operator of the National Electricity System (ONS).
The last project applied the ideas in my thesis
to show the possibility of
improvement in the convergence of multistage stochastic optimization problems
that are fundamental to the Brazilian distribution of energy.
We wrote two technical reports (in Portuguese) for these projects,
which can be found [here](http://www.im.ufrj.br/bernardofpc/relatorios/Relatorio-20172018.pdf) and [here](http://www.im.ufrj.br/bernardofpc/relatorios/Relatorio-2019.pdf).

<header class="subheading">
<h2 id="hodge-jl">Hodge.jl</h2>
<p>Applications of Hodge Theory to statistical rankings</p>
</header>

One of the most powerful tools available to study
the algebraic topology of manifolds is the Hodge decomposition.
Recently,
a discrete analogous of it has been successfully applied
for better understanding how one may transform
a pairwise ranking that is cyclically inconsistent
into some kind of global ordering.

The Julia package `Hodge.jl` is a library implementing
the necessary tools of computational algebraic topology
to easily utilize the Hodge decomposition.
The available structures are representations for simplicial complexes
and the algebra of cochains (discrete analogues of differential forms)
as well as methods for calculating Betti numbers and the Hodge decomposition.

If you want to see more of this package,
feel free to read the
[project's documentation](https://iagoleal.github.io/Hodge.jl/dev/)
or take a look at the
[source code](https://github.com/iagoleal/Hodge.jl).
Comments and contributions are always welcome!

<header class="subheading">
<h2 id="deformablebodies-jl">DeformableBodies.jl</h2>
<p>Simulate the dynamics of self-deforming bodies</p>
</header>

<img src="/img/falling-cat.gif"
     alt="Falling cat simulation"
     title="A falling cat, as seem from both the perspective of a rotating frame of reference and an inertial one."
     width="95%"/>

A common problem when engineering or studying mechanical systems
consists of studying the possible motions of a given body
in a controlled setting, such as in a lab,
and then wanting to understand what will be the body’s motion “in the wild”,
when viewed from an inertial frame of reference.
Examples where this happens are satellites self-adjusting in space
or the complicated moves a cat does to right itself midair.

`DeformableBodies.jl` is a Julia package dedicated to
aid in the construction and simulation of such problems.
It allows the user to enter the body's motion
as seem from an arbitrary frame of reference
and calculates the motion from the perspective of an inertial frame.

The package is freely available from the Julia package system,
all you have to do is enter `]` on the Julia REPL and then run

```julia
pkg> add DeformableBodies
```

and you’re ready to go!
If you are interested in using this package,
be sure to check the guides on the
[documentation](https://iagoleal.github.io/DeformableBodies.jl/dev/)
and also know that the
[source code](https://github.com/iagoleal/DeformableBodies.jl)
is available to everybody.
