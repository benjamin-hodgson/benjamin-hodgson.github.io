---
title: Going Cubical
date: 2022-06-1
---

I wanted to understand Cubical Type Theory a little better. For me, two of the best ways to learn about something are to implement it and to write about it. So here we are: I implemented a toy cubical type system and now I'm writing about it.


The Roadmap
-----------

### Very Brief Intro to HoTT

Homotopy Type Theory is built on the intuition that types behave like topological spaces. Members of a type correspond to (0-dimensional) points in a space. If two values are equal, we say that those points are connected: equalities between values in a type correspond to 1-dimensional paths between points in a space. But equality proofs are themselves elements of a type! This means you can talk about whether two proofs of equality are equal: an equality between equalities corresponds to a 2-dimensional surface between two paths. You can iterate this process indefinitely to get 3-dimensional volumes, 4-dimensional spaces, and so on. (See [the second half of this lecture](https://youtu.be/ow91cvfR-VY?t=2329) from Robert Harper for a detailed and amusing explanation.)

**PICTURE** - types as spaces

In the spatial interpretation, a function `A -> B` is a topological mapping, producing a point in `B` for each point in `A` _and_ producing a path (/surface/volume/etc) in `B` for each path in `A`. (This is just a geometric way of saying that functions take equal inputs to equal outputs.)

This multi-dimensional structure existed in Martin-Löf's original formulation of Type Theory from the 70s, but it wasn't very interesting because there was only one type of path (namely `refl`). Eventually people realised that we could add new types of paths — interesting new notions of equality — into the system, and Homotopy Type Theory was born.

The most important of these new notions of equality is called _univalence_. Univalence is the principle that you should be able to swap out isomorphic structures and not notice the difference. In other words, types which behave in the same way as one another are considered equal — if it looks like a duck and quacks like a duck, it's homotopy-equivalent to a duck. HoTT has a tool called the _univalence axiom_ `ua : Iso A B -> A == B` which takes a proof that `A` and `B` are isomorphic and turns it into a path between `A` and `B`.

But HoTT had a shortcoming, which was that no one could figure out how to build a computational interpretation of univalence. You can write well-typed programs with `ua`, but you can't run them! Cubical Type Theory is an attempt to solve that.


### Very Brief Intro to CuTT

Cubical Type Theory takes the types-as-multidimensional-spaces interpretation literally. In HoTT, the spatial interpretation was an emergent phenomenon — equalities happen to _behave_ like paths and so we say they _are_ paths. CuTT flips that on its head; we're going to take the notion of a path as a primitive and bolt behaviours on to allow us to use paths as equalities.

Concretely, the main innovation of CuTT is to start with a new type `I` (for _interval_) representing the (continuous) set of real numbers between 0 and 1. We can then represent a path in a type `A` as a (continuous) function `I -> A`. A path between paths would be a function `I -> (I -> A)`, but if you uncurry that you'll find that we're talking about a 2-dimensional square `(I, I) -> A`. 3-dimensional cubes are 3-argument functions `I -> I -> I -> A`, and so on. Everything in CuTT is a cube! Variables of type `I` are called _dimension variables_; a term with _n_ dimension variables represents an _n_-dimensional cube.[^1]

[^1]: I was playing fast and loose with the terminology above, so let me clean up my mess in this footnote. A term of type `A` mentioning a free dimension variable represents a path in `A`. If you lambda-abstract that free dimension variable, you'll get a (closed) term of type `I -> A` — a point in `A`'s path space. It corresponds to the original path in `A` but it's not strictly the same thing. The type of paths `I -> A` lets us talk about terms with free dimension variables as first-class things, just as the type of functions `A -> B` lets us talk directly about terms with free `A`-variables.

**PICTURE** - mapping out of the interval

Well, we call them cubes, but you shouldn't necessarily visualise them as ordinary cubes in Euclidean space like dice. In general these cubes might be folded up and twisted around in an interesting way. You can stitch cubes together to get a higher-dimensional cube; you can pinch two corners of a cube down into one; you can stretch a cube out and loop it around on itself.

I mentioned bolting behaviours on to paths. The bolted-on behaviours are called the _Kan operations_, and they allow us to treat paths like equalities: there's a Kan operation to coerce a value along a line, and another to compose paths end-on-end to get a longer path. Every type in CuTT is required to support the Kan operations; as long as all the types in the system are sufficiently well-behaved then we can treat these `I -> A` paths as if they were equalities and use them to implement HoTT.

The notion of a _continuous_ interval made me nervous at first. Computers are digital and aren't naturally given to representing continuous data! For example, it wouldn't be right to use a `float` to represent a point in the interval — floating point numbers aren't continuous, they're just very close together.

CuTT gets around this by treating `I` as an abstract datatype. Here are the rules:

* You can't directly mention any points in the interval other than the endpoints 0 and 1. (There's no explicit concept of "half way along a path".)
* But you can't assume that 0 and 1 are the only points! "Half way along" really does exist, even if we can't speak its name. That means you're not allowed to pattern match on `I`.

This is akin to how time is treated in functional reactive programming. The underlying model in FRP is that time is continuous — you can ask for the current value of a `Behaviour` at any moment. We get away with it, even though computers run on quantised clocks, by computing `Behaviour`s lazily, since in practice the program state changes only at discrete moments.

In fact, I'd argue that this style of thinking should be familiar to anyone who's written a functional program! Functional programmers pass functions around as if they were values, but any code that eventually runs will be operating on plain old first-order data. (The slogan is "open terms don't compute".) In theory your compiler could inline your entire program into the `main` method and erase all of the higher-order-ness. Likewise, in CuTT we program as if cubes were continuous, even though at runtime all of the concrete values will be found at the corners.

Enough philosophy. Let's write some code!


Starting Point: Dependent Lambda Calculus
-----------------------------------------

I'll keep this brief, since I don't want to focus on the general topic of dependent types. For some introductory material on implementing a dependent type checker, see [_Simply Easy_](http://strictlypositive.org/Easy.pdf) (or [_Simpler Easier_](http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html)) or Weirich's [lectures (scroll down)](https://www.cs.uoregon.edu/research/summerschool/summer14/curriculum.html) and [notes](https://github.com/sweirich/pi-forall/tree/2022) on the Pi-Forall language.

OK. When you're implementing a term rewriting system such as lambda calculus, you have to answer three questions:

1. What is the syntax of the terms of your language?
2. How do the terms compute?
3. What are the typing rules for the terms?

(I'm not going to bother with concrete syntax or parsing.) Let's start by answering those questions for a simple dependently typed language. The syntax of our lambda calculus is going to consist of:

* Variables,
* Pi types, lambda abstractions, and applications like `f x`,
* Sigma types, pairs, and projections,
* The Universe type (the type of types), and
* User-supplied type annotations.

**CODE** - AST

My goal was to learn, not to make a remotely usable theorem prover, so I made a couple of design decisions in order to simplify the code: lambda terms always include a type annotation for their parameter, and we have a single universe type (not an infinite tower). I've also said nothing about propositional equality or inductive types, but I will later.

Now, how can we compute with these terms? Here's a function which reduces a term to [weak head normal form](https://stackoverflow.com/q/6872898/1523776).

**CODE** - whnf

You'll notice that `whnf` runs in the type checker monad `Tc`, even though it doesn't presently access the typing context. Later on we're going to want to give certain types extra definitional equality rules, so `whnf` will need to know the terms' types in order to know when to apply those rules.

Finally, how can we type check these terms? Let's start with definitional equality. 

**CODE** - assertEqual

Note that `assertEqual` performs some evaluation, including evaluation inside lambdas. When it encounters two terms which aren't immediately alpha-equivalent (via `==`), it reduces them to weak head normal form before doing any further comparison. This is to make expressions in types work the way you expect — `Vec (1+2) a` should be definitionally equal to `Vec 3 a`. I've also written down the eta rules: `\x -> f x` is definitionally equal to `f` and `(fst p, snd p)` is definitionally equal to `p`.

Finally, here's the type checker. It's written in a _bidirectional_ style, meaning we have pair of functions `check` and `infer`. `check` takes a type as an input and checks a term against it, whereas `infer` takes a term and synthesises its type from the context. Some terms are only `check`able (like `Pair`s), and some terms must be `infer`red (such as the left hand side of a function application).

**CODE** - check and infer
