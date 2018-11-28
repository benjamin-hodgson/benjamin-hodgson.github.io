---
title: What Lurks Beneath the Waves
---

Many programmers know about covariance, contravariance, and invariance. Not so many know about the fourth type.


Covariance
----------

Haskellers'll be familiar with _covariant functors_. It's the type of functor exhibited the the standard `Functor` class.

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

This chimes with the intuition that a functor is a container of sorts. If you can turn each `a` in the container into a `b`, then you can turn a container of `a`s into a container of `b`s by converting each item in the container. The arrows go in the same direction.

A similar idea is visible in Scala's hierarchy of subtypes. Type parameters annotated with a `+` are _covariant_.

```scala
sealed abstract class List[+A]
```

A list of `Cat`s is a list of `Animal`s, because every `Cat` is an `Animal`. The subtype relationship of the container goes in the same direction as the subtype relationship of the elements. (In C# `+` is pronounced `out`, as in `IEnumerable<out T>`.)

```scala
val cats : List[Cat] = List(Cat("Tilly"), Cat("Charlie"), Cat("Izzy"))
val animals : List[Animal] = cats
```

Covariant things are producers.


Contravariance
--------------

Less familiar are _contravariant functors_:

```haskell
class Contravariant f where
    contramap :: (b -> a) -> f a -> f b
```

A contravariant functor is a _consumer_ of `a`s. If you can turn `b`s into `a`s, then you can turn a consumer of `a`s into a consumer of `b`s by converting the `b`s into `a`s before they go into the consumer.

```haskell
newtype Comparer a = Comparer (a -> a -> Ord)

instance Contravariant Comparer where
    contramap f (Comparer p) = Comparer (\x y -> p (f x) (f y))
```

The arrows go in opposite directions.

Scalaists use the `-` symbol to denote a contravariant parameter. (C#ers say `in`, as in `IComparer<in T>`.)

```scala
trait Ordering[-A] {
    def apply(x : A, y : A) : Int
}
```

An ordering of `Animal`s _is an- ordering of `Cats`. If you can compare any two `Animals` (perhaps by comparing their cuteness), then you can certainly compare two cats. The subtype relationship goes in the opposite direction to that of the type parameter.

```scala
val animalOrdering : Ordering[Animal] = Ordering.by[Animal, Int](x => x.cuteness)
var catOrdering : Ordering[Cat] = animalOrdering
```

Contravariant things are consumers. (Julie Moronuki has [the best explanation of contravariance](https://typeclasses.com/contravariance) that I know of.)


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

The only time I've actually seen this class used is in Ed Kmett's [article about attempting to represent higher-order abstract syntax generically](http://comonad.com/reader/2008/rotten-bananas/).

Invariance is more familiar in Scala. A type parameter unadorned with a sign is invariant. It means there's no subtyping relationship between the parameter and the type.

```scala
class Container[A](var value : A) {
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
val cat : Cat = catContainer.get()  // uh oh, this'll return a `Dog`!
```

By the same logic, a `Container[Animal]` is not a `Container[Cat]`.


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

**PICTURE**

So there's this interesting relationship between the three types of variance. They form a little semilattice, of which `Invariant` is the supremum.

But, hmm, the picture seems asymmetric. Is variance really only a semilattice? Or is there something lurking at the bottom of that picture?


Phantom Variance
----------------

Looking at the code above, it appears that `Functor` and `Contravariant` both specialise `Invariant` by not requiring one of `Invariant`'s function parameters. What if we didn't require either of them?

```haskell
class (Functor f, Contravariant f) => Phantom f where
    pmap :: f a -> f b
```

This strange class says that you can map an `f a` to an `f b` without needing to map `a`s or `b`s at all! What does this mean, intuitively?

A functor is `Invariant` when it has `a`s both as inputs and outputs. `Functor` specialises `Invariant` by promising that `f` doesn't have any input `a`s, so all you need to do is map the outputs. `Contravariant` specialises `Invariant` by promising that there are no output `a`s and all you need to do is map the inputs. So `Phantom` guarantees that there are no `a`s at all in the `f`!

So the four types of variance form a nice lattice.

**PICTURE**

For completeness, here are witnesses to the superclass constraints:

```haskell
defaultFmap :: Phantom f => (a -> b) -> f a -> f b
defaultFmap _ = pmap

defaultContramap :: Phantom f => (b -> a) -> f a -> f b
defaultContramap _ = pmap
```

Phantom types show up every now and then in Haskell. They're used to decorate ordinary values with additional type-level information, either to layer on additional type safety or to give GHC a hint for type inference.

```haskell
data Proxy a = Proxy
instance Phantom Proxy where
    pmap _ = Proxy
```

Haskell is the only mainstream language I know of with any real support for phantom types, in its [role system](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#roles). (With GHC's new `QuantifiedConstraints` feature, I'd consider adding `forall a b. Coercible (f a) (f b)` to `Phantom`'s superclasses.) There's no syntax for it in Scala, but if there was it'd mean that a type is always a subtype of any other instantiation of that type, even if the type arguments have no relationship.

```scala
case class Proxy[+-A]

val catProxy = Proxy[Cat]()
val dogProxy : Proxy[Dog] = catProxy
```
