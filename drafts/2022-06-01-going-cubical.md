---
title: Going Cubical
date: 2022-06-1
---

I wanted to understand Cubical Type Theory a little better. For me, two of the best ways to learn about something are to implement it and to write about it. So here we are: I implemented a toy cubical system and now I'm writing about it.


The Roadmap
-----------

One of Homotopy Type Theory's shortcomings was that it didn't make a good basis for a programming language --- you couldn't build a theorem prover out of it. Cubical Type Theory is an attempt to solve that.

Homotopy Type Theory is built on the intuition that types behave like topological _spaces_. Members of a type correspond to (0-dimensional) points in a space. If two values are equal, we say that those points are connected: equalities between values in a type correspond to (1-dimensional) lines between points in a space. Since equalities are themselves elements of a type, you can have identifications between them --- 2-dimensional surfaces between lines, 3-dimensional volumes between surfaces, and so on. (Algebraically, this structure is called an _∞_-groupoid --- see [the second half of this lecture](https://youtu.be/ow91cvfR-VY?t=2329) from Robert Harper for a detailed and amusing explanation.) Types in type theory behaved like spaces all along, but for a long time the correspondence wasn't very interesting because there was only one variety of path (namely `refl`). Eventually folks realised that we could make the structure more interesting by adding new paths, and HoTT was born.

Cubical Type Theory takes the types-as-multidimensional-spaces interpretation literally. In HoTT, the spatial interpretation was an emergent phenomenon --- equalities happen to _behave_ like paths and so we say they _are_ paths. CuTT flips that on its head; we're going to take the notion of a path as a primitive and bolt behaviours on to allow us to use paths as equalities.

Concretely, the main innovation of CuTT is to start with a new type `I` (for _interval_) representing the (continuous) set of real numbers between 0 and 1. We can then represent a line in a type `A` as a (continuous) function `I -> A`. A line between lines would be a function `I -> (I -> A)`, but if you uncurry that you'll find that we're talking about a 2-dimensional square `(I, I) -> A`. 3-dimensional cubes are 3-argument functions `I -> I -> I -> A`. Everything in CuTT is a cube! Variables of type `I` are called _dimension variables_; a value mentioning _n_ dimension variables represents an _n_-dimensional cube.

PICTURE - mapping out of the interval

Well, we call them cubes, but you shouldn't necessarily visualise them as ordinary cubes in Euclidean space like dice. In general these cubes might be folded up and twisted around in an interesting way, depending on the topology of the type they live in. You can stitch cubes together to get a higher-dimensional cube; you can pinch two corners of a cube together; you can stretch a cube out and loop it around on itself.

I mentioned bolting behaviours on to paths. The bolted-on behaviours are called the _Kan operations_, and they allow us to treat paths like equalities: you can coerce a value from one end of a line to the other, and you can compose lines end-on-end to get a longer line. Every type in CuTT is required to support the Kan operations; as long as all the types in the system are sufficiently well-behaved then we can treat these `I -> A` paths as if they were equalities and use them to implement HoTT.

The notion of a _continuous_ interval made me nervous at first. Computers are digital and aren't naturally given to representing continuous data! For example, it wouldn't be right to use a `float` to represent a point in the interval --- floating point numbers aren't continuous, they're just very close together.

CuTT gets around this by treating `I` as an abstract datatype. Here are the rules:

* You can't directly mention any points in the interval other than the endpoints 0 and 1. (There's no explicit concept of "half way along a path".)
* But you can't assume that 0 and 1 are the only points! "Half way along" really does exist, even if we can't speak its name. You're not allowed to do pattern-matching on `I`.

This is akin to how time is treated in functional reactive programming. The underlying model in FRP is that time is continuous --- you can ask for the current value of a `Behaviour` at any moment. We get away with it, even though computers run on quantised clocks, by computing `Behaviour`s lazily, since in practice the program state changes only at discrete moments.

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

```haskell
-- I'm using the `bound` library to manage variables and scopes for me
-- because I don't want to worry about alpha equivalence.
-- https://hackage.haskell.org/package/bound
import Bound

type Type = Term
data Term n
    = U  -- for Universe
    | Var n
    | Ann (Term n) (Type n)  -- type annotation
    | Term n :$ Term n  -- function application
    | Lam (Type n) (Scope () Term n)
    | Pi (Type n) (Scope () Type n)
    | Pair (Term n) (Term n)
    | Sig (Type n) (Scope () Type n)
    | Fst (Term n)
    | Snd (Term n)
    deriving (Eq, Show, Read, Functor, Foldable, Traversable, Generic, Generic1)
    deriving (Eq1, Show1, Read1) via FunctorClassesDefault Term

-- `bound` requires a Monad instance which does substitution
instance Monad Term where
    -- ...
```

My goal was to learn, not to make a remotely usable theorem prover, so I made a couple of design decisions in order to simplify the code: lambda terms always include a type annotation for their parameter, and we have a single universe type (not an infinite tower). I've also said nothing about propositional equality or inductive types, but I will later.

Now, how can we compute with these terms? Here's a function which reduces a term to [weak head normal form](https://stackoverflow.com/q/6872898/1523776).

```haskell
type Ctx n = n -> Type n
type Tc n = ReaderT (Ctx n) (Except String)

whnf :: (Show n, Eq n) => Term n -> Tc n (Term n)
whnf U = return U
whnf v@(Var _) = return v  -- free variables are stuck
whnf (Ann x _) = whnf x  -- discard annotations when evaluating
whnf a@(f :$ x) =
    asum [
        -- if f is a lambda term, we can reduce it
        handleLam =<< assert #_Lam =<< whnf f,
        return a  -- otherwise, it’s stuck
        ]
    where
        handleLam (_, b) = whnf $ instantiate1 x b
whnf l@(Lam _ _) = return l
whnf p@(Pi _ _) = return p
whnf p@(Pair _ _ ) = return p
whnf s@(Sig _ _) = return s
whnf s@(Fst p) = asum [
        assert (#_Pair % _1) =<< whnf p,
        return s  -- stuck
    ]
whnf s@(Snd p) = asum [
        assert (#_Pair % _2) =<< whnf p,
        return s  -- stuck
    ]

assert :: Is k An_AffineFold => Optic' k is s a -> s -> Tc n a
assert l x = case x^?l of
    Just y -> return y
    Nothing -> throwError "mismatched type"
```

You'll notice that `whnf` runs in the type checker monad `Tc`, even though it doesn’t presently access the typing context. Later on we’re going to want to give certain types extra definitional equality rules, so `whnf` will need to know the terms' types in order to know when to apply those rules.

Finally, how can we type check these terms? Let's start with definitional equality. 

```haskell
assertEqual :: (Show n, Eq n) => Term n -> Term n -> Tc n ()
assertEqual x y | x == y = return ()  -- alpha equiv
                | otherwise = bind2 eq (whnf x) (whnf y)
    where
        eq (f :$ x) (g :$ y)
            = assertEqual f g >> assertEqual x y

        -- alpha equiv
        eq (Lam t (fromScope -> b1)) (Lam _ (fromScope -> b2))
            = extend t $ assertEqual b1 b2  -- assume types equal
        -- beta-eta equiv
        eq (Lam t (fromScope -> b1)) y
            = extend t $ assertEqual b1 (suc y :$ Var (B ()))
        eq x (Lam t (fromScope -> b1))
            = extend t $ assertEqual (suc x :$ Var (B ())) b1

        eq (Pi d1 (fromScope -> r1)) (Pi d2 (fromScope -> r2)) = do
            assertEqual d1 d2
            extend d1 $ assertEqual r1 r2  -- d1 == d2 by now

        eq (Pair x1 y1) (Pair x2 y2) = do
            assertEqual x1 x2
            assertEqual y1 y2
        -- eta equiv
        eq (Pair x y) p = do
            assertEqual x (Fst p)
            assertEqual y (Snd p)
        eq p (Pair x y) = do
            assertEqual (Fst p) x
            assertEqual (Snd p) y

        eq (Fst p1) (Fst p2) = assertEqual p1 p2
        eq (Snd p1) (Snd p2) = assertEqual p1 p2
        
        eq (Sig a1 (fromScope -> b1)) (Sig a2 (fromScope -> b2)) = do
            assertEqual a1 a2
            extend a1 $ assertEqual b1 b2

        eq x y
            | x == y = return ()  -- alpha equiv
            | otherwise = throwError $ "mismatched type: tried to compare\n  " ++ pprint' x ++ "\nto\n  " ++ pprint' y

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 m x y = do
    x' <- x
    y' <- y
    m x' y'
```

Note that `assertEqual` performs some evaluation, including evaluation inside lambdas. When it encounters two terms which aren't immediately alpha-equivalent (via `==`), it reduces them to weak head normal form before doing any further comparison. This is to make expressions in types work the way you expect --- `Vec (1+2) a` should be definitionally equal to `Vec 3 a`. I've also written down the eta rules: `\x -> f x` is definitionally equal to `f` and `(fst p, snd p)` is definitionally equal to `p`.

Finally, here's the type checker. It’s written in a _bidirectional_ style, meaning we have pair of functions `check` and `infer`. `check` takes a type as an input and checks a term against it, whereas `infer` takes a term and synthesises its type from the context. Some terms are only `check`able (like `Pair`s), and some terms must be `infer`red (such as the left hand side of a function application).

```haskell
check :: (Show n, Eq n) => Term n -> Type n -> Tc n ()
check x t = ck x =<< whnf t
    where
        ck (Pair x y) (Sig a b) = do
            check x a
            check y (instantiate1 x b)
        ck (Lam d (fromScope -> b)) (Pi d' (fromScope -> r)) = do
            assertEqual d d'
            extend d' $ check b r
        ck (DLam b) (PathD (fromScope -> a) x y) = do
            extend I $ check (fromScope b) a
            assertEqual (instantiate1 I0 b) x
            assertEqual (instantiate1 I1 b) y
        ck (Let t x (fromScope -> b)) t1 = do
            check t U
            check x t
            extend t $ check b (suc t1)
        ck x t = do
            t1 <- infer x
            catchError (assertEqual t t1) $ \e ->
                throwError $ e ++ "\nwhen checking\n  " ++ pprint' x ++ "\nagainst type\n " ++ pprint' t

infer :: (Show n, Eq n) => Term n -> Tc n (Type n)
infer Hole = throwError "can't infer hole"
infer U = return U  -- type in type
infer I = throwError "I is not a type"
infer I0 = return I
infer I1 = return I
infer (Var x) = lookupTy x
infer (Ann x t) = do
    check t U
    check x t
    return t
infer (f :$ x) = do
    fty <- infer f
    case fty of
        Pi d r -> do
            xSys <- check x d
            return $ instantiate1 x r
        _ -> throwError "expected a function type"
infer (Lam d (fromScope -> b)) = do
    check d U
    r <- extend d $ infer b
    return $ Pi d (toScope r)
infer (Pi d (fromScope -> r)) = do
    check d U
    extend d $ check r U
    return U
infer p@(Pair _ _) = throwError $ "Need a type annotation for pair: " ++ pprint' p
infer (Sig a (fromScope -> b)) = do
    check a U
    extend a $ check b U
    return U
infer (Fst p) = do
    (a, _) <- assert #_Sig =<< whnf =<< infer p
    return a
infer (Snd p) = do
    (_, b) <- assert #_Sig =<< whnf =<< infer p
    return $ instantiate1 (Fst p) b
```


The Interval
------------

As I mentioned above, being cubical means working with the interval `I` --- an abstract data type representing a line from 0 to 1. So let's start by extending the syntax of our language to include the `I` type and its endpoints `I0` and `I1`.

CODE

All three of these are already irreducible values; `whnf` doesn't need to do anything with them.

