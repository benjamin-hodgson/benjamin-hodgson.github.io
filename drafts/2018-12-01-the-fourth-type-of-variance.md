---
title: The Fourth Type of Variance
---

_Variance_ is a way to describe a relationship between different instantiations of the same polymorphic type. Many programmers know about covariance, contravariance, and invariance. Not so many know about the fourth type of variance, or the relationship between them.


Covariance
----------

Haskellers'll be familiar with _covariant functors_. It's the type of functor exhibited the the standard `Functor` class.

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

This chimes with the intuition that a functor is a container of sorts. If you can turn each `a` in the container into a `b`, then you can turn a container of `a`s into a container of `b`s by converting each item in the container. The arrows go in the same direction.

A similar idea is visible in Scala's lattice of subtypes. Type parameters annotated with a `+` are covariant.

```scala
sealed abstract class List[+A]
```

A list of `Cat`s is a list of `Animal`s, because every `Cat` is an `Animal`. The subtype relationship of the container goes in the same direction as the subtype relationship of the elements. (In C# `+` is pronounced `out`, as in `IEnumerable<out T>`.)

```scala
val cats : List[Cat] = List(Cat("Tilly"), Cat("Charlie"), Cat("Izzy"))
val animals : List[Animal] = cats
```

A type is covariant if its parameter appears as an output. Covariant things are producers.


Contravariance
--------------

Less familiar are _contravariant functors_:

```haskell
class Contravariant f where
    contramap :: (b -> a) -> f a -> f b
```

The arrows go in opposite directions. A contravariant functor is a _consumer_ of `a`s. If you can turn `b`s into `a`s, then you can turn a consumer of `a`s into a consumer of `b`s by converting the `b`s into `a`s before they go into the consumer.

```haskell
newtype Comparer a = Comparer (a -> a -> Ord)

instance Contravariant Comparer where
    contramap f (Comparer p) = Comparer (\x y -> p (f x) (f y))
```

Note how `f` is applied to `p`'s inputs.

Scalaists use the `-` symbol to denote a contravariant parameter. (C#ers say `in`, as in `IComparer<in T>`.)

```scala
trait Ordering[-A] {
    def apply(x : A, y : A) : Int
}
```

An ordering of `Animal`s is an ordering of `Cats`. If you can compare any two `Animals` (perhaps by comparing their cuteness), then you can certainly compare two cats. The subtype relationship goes in the opposite direction to that of the type parameter.

```scala
val animalOrdering : Ordering[Animal] = Ordering.by[Animal, Int](x => x.cuteness)
val catOrdering : Ordering[Cat] = animalOrdering
```

A type is contravariant if its parameter appears as an input. Contravariant things are consumers. (Julie Moronuki has [the best explanation of contravariance](https://typeclasses.com/contravariance) that I know of.)


Invariance
----------

_Invariant functors_ are both producers and consumers.

```haskell
class Invariant f where
    invmap :: (a -> b) -> (b -> a) -> f a -> f b
```

You have to be able to map `a`s and `b`s in both directions to convert an invariant functor. This implies that the functor both consumes and produces `a`s: you map items on the way out and on the way in.

```haskell
newtype Operation a = Operation (a -> a -> a)

instance Invariant Operation where
    invmap f g (Operation op) = Operation (\x y -> f (g x `op` g y))
```

Note how we use `f` on the output of `op` and `g` on the inputs.

The only time I've actually seen this class used is in Ed Kmett's [article about attempting to represent higher-order abstract syntax generically](http://comonad.com/reader/2008/rotten-bananas/).

Invariance is more familiar in Scala. A type parameter unadorned with a sign is invariant. It means there's no subtyping relationship between the parameter and the type.

```scala
class Container[A](private var value : A) {
    // A appears as both an input and an output
    def get() : A = value
    def set(x : A) : Unit = {
        value = x
    }
}
```

A `Container[Cat]` is not a `Container[Animal]`. If it was, you'd be allowed to upcast it and then call `set` with a `Dog`:

```scala
val catContainer = new Container[Cat](Cat("Tilly"))
val animalContainer : Container[Animal] = catContainer

animalContainer.set(Dog("Richard"))
val cat : Cat = catContainer.get()  // uh oh, this'd return a `Dog`!
```

By the same logic, a `Container[Animal]` is not a `Container[Cat]`.

Let me spell out the similarity between this and Haskell's `Invariant` functors. For `Operation a` to be convertible to `Operation b`, `a` must be convertible to `b` _and_ `b` must be convertible to `a`. For `Container[A]` to be a subtype of `Container[B]`, `A` must be a subtype of `B` _and_ `B` must be a subtype of `A` (that is, they must be the same type).

Note that variance is a property of the type parameter (`A`), not the type constructor (`List`/`Ordering`). A given type constructor may have multiple parameters with different variances. `Function1[-A, +B]`, for example.

<img src="/images/2018-12-01-the-fourth-type-of-variance/hierarchy.png" width="900" />


The Semilattice of Variances
----------------------------

Now, it turns out that these three types of variance have a relationship to each other. Invariance generalises both covariance and contravariance. Covariant things are also invariant, and contravariant things are also also invariant.

```haskell
defaultInvmapCo :: Functor f => (a -> b) -> (b -> a) -> f a -> f b
defaultInvmapCo f _ x = fmap f x

defaultInvmapContra :: Contravariant f => (a -> b) -> (b -> a) -> f a -> f b
defaultInvmapContra _ g x = contramap g x
```

If I was in the business of redesigning Haskell's libraries, I'd even consider making `Invariant` a superclass of `Functor` and `Contravariant`.

```haskell
class Invariant f where {- ... -}

class Invariant f => Functor f where {- ... -}

class Invariant f => Contravariant f where {- ... -}
```

<img src="/images/2018-12-01-the-fourth-type-of-variance/semilattice.jpg" width="900" />

So there's this interesting relationship between the three types of variance. They form a little semilattice, of which `Invariant` is the supremum.

But, hmm, the picture seems asymmetric. Is variance really only a semilattice? Or is there something lurking at the bottom of that picture?


Phantom Variance
----------------

Looking at the code above, it appears that `Functor` and `Contravariant` both specialise `Invariant` by ignoring one of `Invariant`'s function parameters. What if we ignored both of them?

```haskell
class (Functor f, Contravariant f) => Phantom f where
    pmap :: f a -> f b
```

This strange class says that you can map an `f a` to an `f b` without needing to map `a`s or `b`s at all! Intuitively, you can only convert `f a` to `f b` for free when `f` doesn't mention `a` anywhere in its body.

A functor is `Invariant` when it has `a`s both as inputs and outputs. `Functor` specialises `Invariant` by promising that `f` doesn't have any input `a`s, so all you need to do is map the outputs. `Contravariant` specialises `Invariant` by promising that there are no output `a`s and all you need to do is map the inputs. `Phantom`, being a special case of both covariance and contravariance, guarantees that there are no `a`s at all in the `f`.

So the four types of variance form a nice lattice.

<img src="/images/2018-12-01-the-fourth-type-of-variance/lattice.jpg" width="900" />

For completeness, here are witnesses to the superclass constraints:

```haskell
defaultFmap :: Phantom f => (a -> b) -> f a -> f b
defaultFmap _ = pmap

defaultContramap :: Phantom f => (b -> a) -> f a -> f b
defaultContramap _ = pmap
```

Phantom types show up every now and then in Haskell. They're used to decorate ordinary values with additional type-level information, either to layer on additional type safety or to give GHC a hint for type inference.

```haskell
data Proxy a = Proxy  -- from Data.Proxy
instance Phantom Proxy where
    pmap _ = Proxy
```

Haskell is the only language I know of with proper support for phantom types, in its [role system](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#roles). (`Phantom` roughly means `forall a b. Coercible (f a) (f b)`.) Scala doesn't support it, but it'd mean that a type is always a subtype of any other instantiation of that type, even if the type arguments have no relationship.

```scala
case class Proxy[±A]  // hypothetical syntax

val catProxy = Proxy[Cat]()
val dogProxy : Proxy[Dog] = catProxy
```

`Proxy[A]` is always a subtype of `Proxy[B]` (and vice versa!), even when `A` and `B` are nothing to do with each other. To a certain extent this defeats the purpose of phantom types. It also breaks antisymmetry --- two different types can both be a subtype of each other --- so subtyping is no longer a partial order. As a language feature, phantom variance probably isn't actually all that desirable.
