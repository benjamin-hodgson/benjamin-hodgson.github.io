---
title: Imperatively Pretty
---

There's a classic paper by Phil Wadler called [_A Prettier Printer_](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf). (I recommend reading it before this post!) He walks us through the design and implementation of a library for laying out monospaced text. It's a beautiful example of composable code. Wadler defines a core type named `Doc`(ument), some ways to combine documents, and some relationships between those combining operations. The rest of the library almost seems to write itself.

But I think there's another dimension to the paper which is no less beautiful: the specifics of the layout algorithm's decision making process. How does the algorithm decide on a final layout for a document? I think Wadler's code leaves a few interesting details and decisions implicit. It turns out switching to an imperative perspective has something to teach us here.

So I wanted to try to explicate and amplify Wadler's paper. I'm reporting largely from my experience in porting the library to C#. My library is called [Gutenberg](https://github.com/benjamin-hodgson/Gutenberg) and it's [available on Nuget](https://www.nuget.org/packages/Gutenberg). The most mature implementation in Haskell that I know of is [the `prettyprinter` library](https://hackage.haskell.org/package/prettyprinter) — I referred to its design frequently while I was working on Gutenberg.


Two Types of Documents
----------------------

In Section 3 of the paper, Wadler introduces a new representation for documents in the name of efficiency.

```haskell
-- | Section 2's representation
data Doc
    = Nil  -- ^ empty doc
    | Text String Doc  -- ^ literal text
    | Line Int Doc  -- ^ newline followed by indentation
    | Union Doc Doc  -- ^ prioritised choice between two layouts

-- | Section 3's representation
data DOC
    = NIL  -- ^ empty doc
    | DOC :<> DOC  -- ^ concatenation
    | NEST Int DOC  -- ^ increase the indentation of the contained doc
    | TEXT String  -- ^ literal text
    | LINE  -- ^ newline followed by indentation
    | DOC :<|> DOC  -- ^ prioritised choice
```

It took me a few tries to understand what Wadler was trying to achieve. Hopefully I can offer some clarity:

* The original `Doc` is a _normalised_ representation.
    * All of the document's nesting has been factored into the `Line` constructor.
    * All of the concatenation has been flattened into a list-shaped representation. You could squash the `Text` and `Line` constructors into `[Either String Int]`.
* Normalising a document is (asymptotically) costly!
    * `nest i doc` combinator traverses the `doc` to find all of the `Line`s and increase their indentation by `i`.
    * `l <> r` walks to the end of the (list-shaped) `l` document to attach `r`.
* `DOC` instead has explicit constructors for nesting and concatenation.
    * This is a _non_-normalised representation. (For example, `Nest 4 (Nest 4 doc)` is equivalent to `Nest 8 doc`.)
    * We normalise the document's representation only once, when laying out the document. This fixes the asymptotics.
* Wadler plans to keep `Doc` around as an _output_ of the layout algorithm.
    * Wadler's final `Doc` has no `Union` constructor. So you can think of `Doc` as a _list of instructions_ for a rendering system which knows how to output text, newlines and indentation. Or, to put it another way, a `Doc` represents a concrete, final layout for a given piece of text.
    * He thinks of `DOC` as representing a set of `Doc`s — a set of options for laying out a given piece of text.
    * So laying out a document means two things: choosing a particular layout from the choices within the document, and normalising it into a list-shaped representation.

I think a better name for `Doc` would've been something like `LayoutInstructionStream`. (The `prettyprinter` library calls it `SimpleDocStream`.) It's quite an imperative-feeling datatype to me — it's all about writing chunks of text into an output stream. For Gutenberg I embraced the `void`:

```csharp
// An object which knows how to write
// text into an output stream
interface IDocumentRenderer
{
    // In the library these methods are async
    void NewLine();
    void WhiteSpace(int amount);
    void Text(string text);
}
```

Rather than emitting a sequence of instructions to be interpreted, my layout engine imperatively calls an `IDocumentRenderer`.


Grouping, Buffering, and Backtracking
-------------------------------------

For Wadler, the main job of a layout algorithm is to figure out how to use the available horizontal space efficiently. He describes a way to choose between two options for laying out a single line:

> If both lines are shorter than the available width, the longer one is better. If one line fits in the available width and the other does not, the one that fits is better. If both lines are longer than the available width, the shorter one is better.




The Work Stream
---------------

Wadler's core layout function is called `be` (because it chooses the `be`st layout for a document). It took me a while to understand why `be` consumes a list of documents; Wadler somewhat nonchalantly mentions a "list of indentation-document pairs" without fully detailing his thought process.

Here is `be`'s signature:

```haskell
be :: Int  -- ^ The page width
   -> Int  -- ^ The current nesting level
   -> [(Int, DOC)]  -- ^ A list of nesting-document pairs
   -> Doc  -- ^ The resulting laid-out and normalised document
```




Laziness
--------


Line Break Hints
----------------


Annotations
-----------


