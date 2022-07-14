---
title: Parallelising Source Positions
---

My parsing library [Pidgin](https://github.com/benjamin-hodgson/Pidgin) has some infrastructure to track positions in a textual input file, for the purposes of error reporting. The library's written in C#, but for today let's work in Haskell.

```haskell
data SourcePos = SourcePos {
    line :: Natural,
    col :: Natural
} deriving (Eq, Ord)
```

The parser keeps track of the current `SourcePos` by looping over the characters in the input file and calling a method called `calculateSourcePos`. `calculateSourcePos`'s job is to update the current `SourcePos` for a single character.

```haskell
newSourcePos = foldl' calculateSourcePos oldSourcePos inputText
```

You can supply your own `calculateSourcePos` implementation, but the default one looks roughly like this:

```haskell
calculateSourcePos :: SourcePos -> Char -> SourcePos
calculateSourcePos sp '\n' = SourcePos {
    line = line sp + 1,  -- increment the line count
    col = 1  -- and reset the column count
    }
calculateSourcePos sp _ = sp { col = col sp + 1 }
```

When the parser encounters a new line, the column counter is reset back to 1.

The signature of `calculateSourcePos` poses a performance problem: it takes the previous `SourcePos` as an argument. Each iteration of the loop depends on the result of the previous iteration. This _data dependency_ means the loop can't be parallelised --- you have to wait for each iteration of the loop to finish before you can start the next one.

This article is about redesigning `SourcePos` to be more parallelisable.


Monoids are Embarrassingly Parallel
-----------------------------------

_Monoid_ is the high-falutin' mathsy name for a certain variety of composable objects. Composability is very important in programming, and consequently monoids show up all over the place. There are plenty of introductions to monoids out there (mostly by [better](https://blog.ploeh.dk/2017/10/06/monoids/) [teachers](https://www.youtube.com/watch?v=-mnA8_DWfik) than me), so I'll keep it brief.

For an object to be a monoid you need two things:

1. An operator `<>` which combines two values of your type into a bigger value.

    * It shouldn't matter how you nest a sequence of `<>` operations: `(x <> y) <> z == x <> (y <> z)`.

2. A `mempty` value, representing some notion of "zero".

    * Combining `mempty` with another value should leave that value unchanged: `mempty <> x == x == x <> mempty`.

```haskell
class Monoid m where
    mempty :: m
    (<>) :: m -> m -> m
```

The rule about nesting `<>` is what makes monoids good for parallelism. Suppose you have a big array of monoidal values, and you want to combine them all into a single value using `<>`. It doesn't matter what order you perform the additions in, so you can safely divide your array into chunks, sum up each chunk in parallel, and then combine the results. (This is the basic idea behind MapReduce.)

```haskell
mconcat :: Monoid m => Vector m -> m
mconcat = Vector.foldr (<>) mempty

-- performs the same computation but in parallel
pmconcat :: Monoid m => Vector m -> m
pmconcat = List.foldr (<>) mempty
    . parMap mconcat
    . chunksOf 1024
```

To put it another way, monoids don't suffer from the data dependency which made `calculateSourcePos` hard to parallelise. The challenge, then, is to come up with a way to make `SourcePos` into a monoid.


Deltas
------

If you go hiking, you might bring with you a list of directions on a piece of paper. Directions are monoidal: if you have directions from your house to the beach, and from the beach to the pub, you can follow those directions sequentially to get from your house to the pub. (I'll tolerate hiking as long as the destination is a pub.)

Directions are relative, not absolute. The directions on your paper might tell you how to get from your house to the pub, but if you start somewhere other than your house you can still follow the directions --- you just have to hope you end up in a different pub.

So by analogy, let's stop worrying about absolute locations in a source file and instead think about offsets relative to an arbitrary location.

```haskell
data SourcePosDelta = SourcePosDelta {
    lines :: Natural,
    cols :: Natural
}
```

You can add a relative `SourcePosDelta` to an absolute `SourcePos` to get a new absolute `SourcePos`. This is analogous to setting off on a hike from a given starting location.

```haskell
add :: SourcePos -> SourcePosDelta -> SourcePos
add sp delta = SourcePos {
    line = line sp + lines delta,
    col = (if lines delta == 0 then col sp else 0) + cols delta
}
```

When the delta spans multiple lines, we discard `sp^.col` and only take `delta^.Cols`. This reflects the behaviour of `computeSourcePos`, which resets the column counter when a new line is encountered. The asymmetry is interesting, though; the `line` calculation depends only on the two `lines` fields, but the `col` field has some interference from the `lines`.

Likewise, you can find the difference between two `SourcePos`es to get the path that would take you from one to the other:

```haskell
subtract :: SourcePos -> SourcePos -> SourcePosDelta
subtract start end = SourcePosDelta {
    lines = line end - line start,
    cols = col end - (if line end == line start then col start else 0)
}
```

From here we can see how to add a pair of `SourcePosDelta`s (and write the `Monoid` instance). In fact, the code is more or less identical to `add`:

```haskell
instance Monoid SourcePosDelta where
    mempty = SourcePosDelta 0 0
    
    delta1 <> delta2 = SourcePosDelta {
        lines = lines delta1 + lines delta2,
        cols = (if lines delta2 == 0 then cols delta1 else 0) + cols delta2
    }
```

I'll leave it up to you to convince yourself that this definition satisfies the monoid laws.

Each character in the input file corresponds to a small `SourcePosDelta`: `'\n'` corresponds to `SourcePosDelta 1 0` and each other character corresponds to `SourcePosDelta 0 1`. The parser can map each character to a `SourcePosDelta`, add up the monoidal deltas (possibly in parallel), and then add the result to the `SourcePos` corresponding to the start of the `inputText`.

```haskell
calculateSourcePosDelta inputText = pmconcat $ parMap toDelta inputText
    where
        toDelta '\n' = SourcePosDelta 1 0
        toDelta _ = SourcePosDelta 0 1
```


Hardware Parallelism
--------------------

[I tested some implementations of `<>` (in C#)](https://github.com/benjamin-hodgson/Pidgin/blob/sourceposdelta/Pidgin.Bench/SourcePosDeltaBench.cs) which made use of modern CPUs' support for _hardware parallelism_ (also known as SIMD, for Single Instruction Multiple Data). Recent versions of .NET come bundled with opt-in SIMD support, to be found in [the `System.Numerics` namespace](https://docs.microsoft.com/en-us/dotnet/api/system.numerics?view=net-6.0).

CPUs with SIMD have extra-wide registers (like, 256 bits) divided into _lanes_. You can stuff four 64-bit integers (or eight 32-bit ones) into a single register, and the CPU supports special instructions to perform the same operation on each of those four integers at once --- so (for example) you can add four sets of two integers with a single instruction. Each SIMD operation is a little bit slower than its corresponding single-target instruction, but since they operate on four times the amount of data you can often get reasonable speedups when you're processing lots of data.

[My fastest attempt](https://github.com/benjamin-hodgson/Pidgin/blob/539ecf23b8ebf0f601a48ade7576343671ef075c/Pidgin.Bench/SourcePosDeltaBench.cs#L278) involved packing `SourcePosDelta`'s two integers into [a single 64-bit integer](https://github.com/benjamin-hodgson/Pidgin/blob/539ecf23b8ebf0f601a48ade7576343671ef075c/Pidgin.Bench/SourcePosDeltaBench.cs#L320) and doing some [bit manipulation](https://github.com/benjamin-hodgson/Pidgin/blob/539ecf23b8ebf0f601a48ade7576343671ef075c/Pidgin.Bench/SourcePosDeltaBench.cs#L334) to implement `<>`'s "annihilation" semantics. Then I divided the input array into four chunks --- one per lane --- and ran the bit manipulation algorithm on each lane. Since it's best to load contiguous data into a SIMD register's lanes, I had to [rearrange the input array](https://github.com/benjamin-hodgson/Pidgin/blob/539ecf23b8ebf0f601a48ade7576343671ef075c/Pidgin.Bench/SourcePosDeltaBench.cs#L51) to line the chunks up with their SIMD lanes.

I decided against putting the SIMD code into the library, because in practice the number of `SourcePosDelta`s which need summing is often not very large. In any case, the SIMD code offered comparable performance to a much simpler algorithm: loop backwards until you find the last newline in a chunk of text, then ignore any non-newline characters to the left of it.


Spans and Shifts
----------------

Here's another cool thing you can do with `SourcePosDelta`.

In general, each node in a parse tree corresponds to a certain _span_ of the input document: the section of the input text containing a given syntactic construct.

```haskell
data Span = Span {
    start :: SourcePos,
    width :: SourcePosDelta
}

end :: Span -> SourcePos
end span = add (start span) (width span)

containsPos :: Span -> SourcePos -> Bool
span `containsPos` pos = pos >= start span && pos <= end span
```

It's common in compilers to keep track of these spans in order to report error messages. For example, you might want to colour a piece of code red in an IDE if it contains an error.

```haskell
data Node = Node {
    span :: Span,
    data :: Expr
}
data Expr = Lit Int | Add Node Node
```

In an interactive IDE, you have a programmer typing code; the IDE's parser needs to respond in real time to each keystroke. So, for performance's sake, you only want to re-parse the fragment of code that the user is currently typing, and reuse the rest of the document's parse tree from the last time you parsed it.

But when the user types characters into the middle of a document, all of the constructs after it get moved right and down. So you need to `shift` the parse tree over by the amount that the user typed.

```haskell
shift :: SourcePosDelta -> Node -> Node
shift d (Node span data) = Node (shiftSpan d span) (shiftData data)
    where
        shiftData l@(Lit _) = l
        shiftData (Add l r) = Add (shift d l) (shift d r)

shiftSpan :: SourcePosDelta -> Span -> Span
shiftSpan d (Span start width) = Span (add start d) width
```

`shift` rebuilds the entire tree! That's asymptotically as bad as re-parsing the entire file --- clearly not something we want to do on every keystroke. Instead, let's annotate the tree with the amount by which it's been shifted. This lets us shift a whole subtree at once without mutating it.

```haskell
data Node = Node {
    shiftedBy :: SourcePosDelta,
    originalSpan :: Span,
    expr :: Expr
}

span :: Node -> Span
span (Node shift span _) = shiftSpan shift span

shift :: SourcePosDelta -> Node -> Node
shift delta node = node { shiftedBy = delta <> shiftedBy node }
```

`shift` is very fast now; it only rebuilds a single node. When the compiler traverses the parse tree, it simply needs to keep track of the current shift amount and apply that shift to any locations reported in error messages.

For completeness, here is some code to apply an edit to a parse tree. `edit` searches the parse tree to find the node where the edit occurred, replaces that node, and `shifts` everything that was textually to the right of that node.

```haskell
editSpan :: Span  -- ^ The location and amount the user typed
         -> Span  -- ^ The original span
         -> Maybe Span  -- ^ The span with the edit applied
editSpan edit span
    | not (span `containsPos` (start edit)) = Nothing
    | otherwise = Just $ Span
        (start span)
        -- insert `edit`'s `SourcePosDelta` into `span`'s
        (subtract (start span) (start edit)
            <> width edit
            <> subtract (end span) (end edit))

edit :: Span  -- ^ The location and amount the user typed
     -> Expr  -- ^ The replacement parse tree
     -> Node  -- ^ The original parse tree
     -> Node  -- ^ The edited parse tree
edit s replacement node = Node
    mempty
    -- this `fromJust` is ok assuming that the edit was inside `span node`
    (fromJust $ editSpan s (span node))
    (editExpr (expr node))
    where
        editExpr (Lit _) = replacement
        editExpr (Add l r)
            | span l `containsPos` start s = Add (edit s replacement l) (shift (width s) r)
            | span r `containsPos` start s = Add l (edit s replacement r)
            | otherwise = replacement
```

This code is linear in the _depth_ of the tree, which is usually shallow (typically dozens of nodes deep, rather than hundreds or thousands). You could do even better asymptotically by storing the parse tree in a [zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)) focused on the node the user is currently editing, and lazily apply shifts as the zipper moves around.


Monoid Actions
--------------

Let's look a little closer at the formalism underlying `SourcePosDelta`.

This idea of building up a monoid and then using it to transform some other value is formalised using the notion of _actions_ (also known as _modules_). A monoidal object is said to be a _monoid action_ on some other object `a` if it can be used to transform `a`:

```haskell
-- "Action a m" means "m acts on a"
class Monoid m => Action a m where
    act :: m -> a -> a
```

I always thought _action monoid_ would make a better name for this concept than _monoid action_. The monoidal value `m` represents an action to be carried out on some other object `a`.

To be a well-behaved monoid action, the `act` method should respect the monoidal nature of the action:

1. `mempty` should be a no-op action: `act mempty == id`.

2. Building a monoidal value with `<>` and then acting with it should be the same as acting with the individual pieces of the monoid: ``(m <> n) `act` x == m `act` (n `act` x)``.

    * This rule defines a "left" monoid action. It turns out that `SourcePosDelta` is actually a "right" monoid action, for which this rule is flipped: ``(m <> n) `act` x == n `act` (m `act` x)``.

    * In fact, let's separate the ideas of "left" and "right" actions. I find that the composition law for right actions makes more sense when the arguments are the other way around.

        ```haskell
        class Monoid m => LAction a m where
            lact :: m -> a -> a
        
        class Monoid m => RAction a m where
            ract :: a -> m -> a
        ```

        So now `RAction`'s composition law reads ``x `ract` (m <> n) == (x `ract` m) `ract` n``.

Basically, these laws assert that you can build actions with `<>` and it'll behave in the way you expect.

Here are a couple of examples to help you think about monoid actions.

1. Yes, `SourcePosDelta` is a monoid action on `SourcePos`. Adding a `SourcePosDelta` to a `SourcePos` gives you a new `SourcePos` representing somewhere later in the file --- a `SourcePosDelta` represents the action of moving to a later location. You can convince yourself that the definition of `ract` does indeed satisfy the laws I outlined above:

    ```haskell
    instance RAction SourcePos SourcePosDelta where
        ract sp delta = SourcePos {
            line = line sp + lines delta,
            col = (if lines delta == 0 then col sp else 0) + cols delta
        }
    ```
2. I first learned about monoid actions (well, group actions) during a crystallography course at university. Crystallographers are very concerned with symmetry, because crystals are repeating structures which look the same in every direction. Crystals can be categorised by the collection of rotations, reflections and translations (the _space group_) under which the crystal looks the same. You monoidally build up a sequence of spatial transformations, and then use those transformations to act on a crystal.

    A more familiar way of phrasing the same example: When you go hiking with written directions, you can think of the directions as acting on your current location. When you follow a direction such as "walk 200 metres", you update your location accordingly.

3. Any monoid can always be thought of as acting upon itself, simply by defining `act = (<>)`.


Actions All the Way Down
------------------------

It turns out monoid actions can also explain the strange asymmetry in `SourcePosDelta`'s `(<>)` method. When computing `delta1 <> delta2`, we can think of `delta2`'s `lines` as acting on `delta1`'s `cols`.

```haskell
newtype Lines = Lines Natural deriving Monoid via (Sum Natural)
newtype Cols = Cols Natural deriving Monoid via (Sum Natural)

instance RAction Cols Lines where
    ract c (Lines 0) = c
    ract _ _ = Cols 0
```

`Lines`'s action on `Cols` is to erase the `Cols` altogether when the number of `Lines` is not zero. It's a weirdly degenerate monoid action, but it's an action nonetheless. (It seems like `Lines` basically represents an [annihilator](https://en.wikipedia.org/wiki/Annihilator_(ring_theory)) for `Cols`.)

With `Lines`'s action on `Cols` in hand, `SourcePosDelta`'s `Monoid` instance can be explained as an instance of a (right) _semi-direct product_. A semi-direct product is like a regular product type `(a, b)`, except its `Monoid` instance allows for one of the fields to interfere with the other by acting on it:

```haskell
data RSemiDirect a b = RSD a b

instance (Monoid a, Monoid b, RAction a b) => Monoid (RSemiDirect a b) where
    mempty = RSD mempty mempty

    (RSD x1 y1) <> (RSD x2 y2) = RSD
        (ract x1 y2 <> x2)
        (y1 <> y2)
```

`RSemiDirect a b` is a monoid when `b`'s action on `a` respects `a`'s monoidal structure:

1. Acting on an empty monoid produces another empty monoid: `ract mempty x == mempty`.

2. `b`'s monoidal structure distributes over `a`'s: `ract (x <> y) z == ract x z <> ract y z`.

I won't prove it here, but these do hold for `Lines`'s action on `Cols`. So we have a clean explanation for `SourcePosDelta`'s asymmetric monoidal structure in terms of the semi-direct product:

```haskell
type SourcePosDelta = RSemiDirect Cols Lines
```

I first read about semi-direct products a few years ago in [the "twisted functors" paper](http://ozark.hendrix.edu/~yorgey/pub/twisted.pdf). They use semi-direct products to manage pointer arithmetic while writing to buffers --- monoidally building up an offset and using it to act on a base pointer. They also give a couple of other examples.
