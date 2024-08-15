---
title: Prettier. Happier. More Imperative.
subtitle: Understanding _A Prettier Printer_ by Porting It
date: 2024-08-15
---

Phil Wadler’s pearl [_A Prettier Printer_](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf) is a classic example of functional design. Starting with a simple model and some algebraic laws, Wadler _derives_ an implementation of a well behaved layout algorithm. It’s a great read — go and read it if you haven’t! (I’m going to assume you have read it and try not to recapitulate too much below.)

I learned a lot about Wadler’s algorithm by translating it to an imperative language. I think Wadler’s explanation skims over a couple of interesting behavioural details of his algorithm, and those details are actually a little easier to see in an imperative setting.

You can find (a productionised version of) the code I’m going to present today [in my library `Gutenberg`](https://github.com/benjamin-hodgson/Gutenberg/blob/main/Gutenberg/LayoutEngine.cs). As you’ll see, the code fuses together lots of my favourite programming ideas: it’s a combinator library, with a purely functional front end, backed by a logic programming engine, written as a stack machine! Let’s dive in.


## Wadler’s Code

Wadler’s library is designed around a datatype of _documents_. A document represents a block of text with some optional line breaks and indentation. To use the library, you construct a document using the provided combinators, and then invoke the layout algorithm to render the document as a string.

For example, you might construct a document containing a code listing for a function call. If the function’s arguments can all fit on one line then the expression should be flattened,

```c
printf("My %s is named %s.", "cat", "Prune");
```

but a longer expression might need to be broken up with each argument on its own line.

```c
printf(
    "My %s is named %s. She eats %s",
    "cat",
    "Prune",
    "Tender Chicken Chunks"
);
```

Typically you'll annotate your document with several different sets of line break hints. So a given document can be laid out in a number of ways. The layout algorithm’s job is to choose the best layout — meaning one which makes good use of the available horizontal space and doesn’t introduce more line breaks than necessary.

Internally, the notion of a _choice of layouts_ is represented by the `:<|>` constructor: `doc1 :<|> doc2` means “`doc1` is a preferable layout, but `doc2` can be used as a fallback if `doc1` causes the text to overflow the page width”. The layout algorithm takes a document containing `:<|>`s and produces one without.

The work is done by the function `best`. Here it is, with some cosmetic tweaks:

```haskell
best
    :: Int  -- ^ The page width
    -> Int  -- ^ The number of characters already consumed on the current line
    -> DOC  -- ^ The document to lay out
    -> Doc  -- ^ The laid-out document
best w k x = be k [(0, x)]
    where
        be k [] = Nil
        be k ((i, NIL):z) = be k z
        be k ((i, x :<> y):z) = be k ((i, x):(i, y):z)
        be k ((i, NEST j x):z) = be k ((i+j, x):z)
        be k ((i, TEXT s):z) = Text s (be (k + length s) z)
        be k ((i, LINE):z) = Line i (be i z)
        be k ((i, x :<|> y):z)
            = better k
                (be k ((i, x):z))
                (be k ((i, y):z))

        better k x y =
            if fits (w-k) x
            then x
            else y

fits w x | w < 0 = False
fits w Nil = True
fits w (Text s x) = fits (w - length s) x
fits w (Line i x) = True
```

Let’s try to reconstruct Wadler’s code in an imperative style.


## Wadler’s Two Documents

Section 3 of the paper is all about designing an efficient representation for documents. Wadler calls it `DOC`. It took me a while to grasp the reason for the new name: he’s keeping the old `Doc` around (in modified form — it lacks the `Union` constructor).

* **`DOC` is the API**. It’s the core representation of documents that users of the library interact with. It’s an abstract type — you create `DOC`s using the provided combinators, not by calling its constructors directly.
* **`Doc` is an intermediate representation**. It’s the result of laying out a document (it’s returned by `best`). Wadler gives a function (awkwardly named `layout`) to display a `Doc` as a string but you can imagine other backends (for example, writing directly to the console).

I think Wadler’s choice of names could use some improvement — [the modern `prettyprinter` library](https://hackage.haskell.org/package/prettyprinter) uses `SimpleDocStream` for the latter. Structurally, `Doc`(/`SimpleDocStream`) is a _list_ of literal text chunks interspersed with line-breaks-with-indentation:

```haskell
data Doc
    = Nil
    | Text String Doc
    | Line Int Doc
-- equivalently,
type Doc = [Either String Int]
```

But in an imperative language it might be better to conceive of `Doc` as a _sequence of instructions_ for a rendering backend. In C#, it’s more natural to abstract over backends using an interface, rather than an intermediate data structure:

```csharp
interface IDocumentRenderer
{
    void WriteText(string text);
    void WriteLineBreak(int indentation);
}
```


## `best` is a Stack Machine

In Section 3, Wadler somewhat opaquely describes “generalising each operation to work on a list of indentation-document pairs”. He’s talking specifically about `be` (`best`’s inner loop). Let’s take a look at a few of its clauses to figure out what he means:

```haskell
best w k x = be k [(0, x)]
    where
        be k [] = Nil
        be k ((i, NIL):z) = be k z
        be k ((i, x :<> y):z) = be k ((i, x):(i, y):z)
```

The list is initialised to a single item and thereafter the code only manipulates the front few items. The list is being treated as a stack. This stack represents a work stream — each item in the stack is a fragment of the document which hasn’t been laid out yet. The top item is the _next_ fragment to be laid out.

So let’s write a loop that mutates a stack. (I’ll come back to the `:<|>` case — it requires special consideration!)

```csharp
void Layout(Document doc, IDocumentRenderer renderer)
{
    var stack = new Stack<(int, Document)>();
    stack.Push((0, doc));

    while (stack.Count > 0)
    {
        var (indentation, current) = stack.Pop();
        switch (current)
        {
            case Empty:  // NIL
                break;
            case LineBreak:  // LINE
                renderer.WriteLineBreak(indentation);
                break;
            case Text(var s):  // TEXT
                renderer.WriteText(s);
                break;
            case Concat(var l, var r):  // l :<> r
                stack.Push((indentation, r));
                stack.Push((indentation, l));
                break;
            case Nested(var i, var d):  // NEST i d
                stack.Push((indentation + i, d));
                break;
        }
    }
}
```

For example, here's a diagram of what happens to the stack when processing a document consisting of two literal strings.

```
                             +-----------+
                             |   "foo"   |
+---------------------+      +-----------+      +-----------+
| Concat("foo","bar") |      |   "bar"   |      |   "bar"   |
+---------------------+  =>  +-----------+  =>  +-----------+  =>  +-------+
|          ...        |      |    ...    |      |    ...    |      |  ...  |
                                         Output:    "foo"          "foobar"
```


## Backtracking

Now let’s look at the `:<|>` case.

```haskell
be k ((i, x :<|> y):z)
    = better k
        (be k ((i, x):z))
        (be k ((i, y):z))

better k x y =
    if fits (w-k) x
    then x
    else y

fits w x | w < 0 = False
fits w Nil = True
fits w (Text s x) = fits (w - length s) x
fits w (Line i x) = True
```

This is the key part of the layout algorithm: deciding which of two alternative layouts to use, based on whether it causes the document to overflow the available horizontal space.

The stack `z` represents _the rest of the document_ to be laid out. The code concatenates the first alternative `x` onto the front of the document `z` and lays out the document to see if it’ll fit in the allocated page width. If it doesn’t fit, we’ll push the fallback layout `y` onto the stack and lay that out instead.

It’s important that we attempt to lay out the whole document — `x` _and_ `z`, not just `x` — when seeing if it’ll fit. Choosing the `x` alternative might cause an overflow later (when processing a document that's right now buried somewhere below the current top of the stack), even if `x` fits on its own. In other words this is a _backtracking_ operation: if a layout with `x` overflows later in the line, we backtrack to the point at which we chose `x`, forgetting any layout decisions we made while rendering `x:z`.

This code is tricky to port to an imperative style because it makes central use of immutability and laziness:

* **The rest of the document is duplicated**. Using `z` twice like that requires `z` to be immutable (well, persistent). If we mutate `z` while laying out the first alternative, by pushing and popping things from it, it won’t be in the same state by the time we decide to fall back on `y` if it doesn’t fit!
* **`fits` only reads as far as the end of the line**, so backtracking is bounded to `lineWidth` characters. If the current line fits, then there’s nothing to be gained by trying a less-flat alternative. This requires lazy evaluation: we don’t want to lay out the entire document just to see whether the first line fits!
* We need some way of _undoing_ any calls to the `IDocumentRenderer` that were made in the course of laying out `x`.

> [!note] Aside: A Thought On Laziness
>
> Wadler wrote a function that looks like it lays out the entire document, but because the language is lazy it never actually processes more than one line at a time. Cool! But, I dunno, understanding this took me some head-scratching because this important aspect of the control flow is not visible in the text of the code. Laziness can be a mixed blessing — it lets us separate producers and consumers in novel ways, _but_ it makes control flow implicit. Perhaps I'm still just not good enough at thinking lazily.
>
> (I tried implementing the backtracking behaviour [using a failure continuation](https://gist.github.com/benjamin-hodgson/8b3062e9715f10b6c74f65424cab9b27). The code is certainly more complex and uglier than Wadler's — there are two mutually recursive continuation types! — but it has the benefit of being very explicit about [when backtracking can happen](https://gist.github.com/benjamin-hodgson/8b3062e9715f10b6c74f65424cab9b27#file-prettiercps-hs-L48) and [what to do when it does](https://gist.github.com/benjamin-hodgson/8b3062e9715f10b6c74f65424cab9b27#file-prettiercps-hs-L61).)

Anyway, we can handle the latter two issues imperatively by _buffering_ the output text one line at a time. If we reach the end of the line without overflowing, we can flush the buffer’s contents out to the `IDocumentRenderer`.

```csharp
void Layout(Document doc, IDocumentRenderer renderer)
{
    var buffer = new List<char>();
    var stack = new Stack<(int, Document)>();
    stack.Push((0, doc));

    while (stack.Count > 0)
    {
        var (indentation, current) = stack.Pop();
        switch (current)
        {
            case LineBreak:
                // Flush the buffer
                renderer.WriteText(new string(buffer.ToArray()));
                renderer.WriteLineBreak(indentation);
                buffer.Clear();
                break;
            case Text(var s):
                buffer.Add(s);
                break;
            // ... other cases unchanged
        }
    }
}
```

What to do in the case of an overflow? We need to reset the state of the layout algorithm to how it was before we tried to lay out `x`. This involves putting the stack back how it was at that time and also dropping any chunks that were written to the buffer since then.

Let’s record every alternative we’ve tried in a separate stack of _choice points_. A choice point is a data structure containing a fallback alternative and the current state of the layout algorithm — consisting of the length of the buffer and the state of the stack. We have to make a copy of the stack because it’s mutable.

```csharp
var choicePoints = new Stack<ChoicePoint>();

// ... in the switch block:
case ChoiceDoc(var x, var y):  // x :<|> y
    choicePoints.Push(new ChoicePoint(
        indentation,
        y,
        buffer.Count,
        Copy(stack)
    ));
    stack.Push((indentation, x));
    break;
```

Now when an overflow happens we can backtrack to the last choice point. And if we reach the end of the line without overflowing, we can discard any remaining choice points.

```csharp
case Text(var s):
    buffer.AddRange(s);
    if (buffer.Count > LineWidth && choicePoints.Count > 0)
    {
        Backtrack();
    }
    break;
case LineBreak:
    renderer.WriteText(new string(buffer.ToArray()));
    renderer.WriteLineBreak(indentation);
    buffer.Clear();
    Commit();
    break;


void Backtrack()
{
    var (i, y, bufLen, s) = choicePoints.Pop();
    stack = s;
    buffer.RemoveRange(bufLen, buffer.Count - bufLen);
    stack.Push((i, y));
}

void Commit()
{
    choicePoints.Clear();
}
```


## A Cue From Prolog

Copying the stack when we create a choice point is ugly and inefficient: each copy operation takes linear time and space, and we make a linear number of copies (one for every choice point), resulting in quadratic performance.

I thought about trying to amortise the copying operation with some sort of copy-on-write mechanism. But around the time I was working on this library I’d been [reading about Warren’s Abstract Machine](https://github.com/a-yiorgos/wambook) — a backend for Prolog implementations — and that gave me a better idea.

In the WAM, choice points are stored _on the execution stack_ interspersed with the predicates’ stack frames. Things are arranged such that choice points are only popped from the stack after backtracking, not during normal execution. This prevents any preceding stack frames from being discarded, so that the state of the call stack can be restored upon backtracking. (The relevant section of [the book](https://github.com/a-yiorgos/wambook) is Section 4.1, _Environment Protection_.)

We can apply this idea to the layout engine. The items in the work stream will now be either a `Document` to be rendered or a choice point.

```csharp
case ChoiceDoc(var x, var y):
    stack.Push((
        indentation,
        new ChoicePoint(
            y,
            buffer.Count,
            canBacktrack,
            continuation: stack.Count - 1  // see below
        )
    ));
    stack.Push((indentation, x));
    canBacktrack = true;
    break;
```

The stack transition looks like this (supposing that the `Choice` document is encountered when the stack has two items):

```
                          +------------------+
3                         |         x        |
    +--------------+      +------------------+
    |              |      |   ChoicePoint    |
2   | Choice(x, y) |      | Continuation = 1 |
    |              |      |   Fallback = y   |
    +--------------+  =>  +------------------+
1   |     ...      |      |        ...       |
    +--------------+      +------------------+
0   |              |      |                  |
```

Upon encountering a choice point in the work stream, we’ll _leave the choice point where it is_ and continue rendering the document that’s underneath it. This may result in new document fragments being piled on top of the choice point (and then being processed themselves). That means we’ll likely encounter each choice point multiple times. Each choice point will maintain a _continuation pointer_ to the next document fragment to render in the non-backtracking code path.

```csharp
case ChoicePoint cp:
    var continuation = GetContinuation(cp.Continuation);
    if (continuation < 0)
    {
        // We’ve processed the whole document!
        renderer.WriteText(new string(buffer.ToArray()));
        return;
    }
    // put the choice point back where it was
    // and process the continuation
    cp.Continuation = continuation - 1;
    stack.Push((indentation, cp));
    stack.Push(stack[continuation]);
    break;

int GetContinuation(int i)
{
    while (i >= 0 && stack[i] is ChoicePoint cp)
    {
        i = cp.Continuation;
    }
    return i;
}
```

The stack transition looks like this. The document fragment that was at the `Continuation` index is duplicated to the top of the stack.

```
                              +------------------+
3                             |        z         |
    +------------------+      +------------------+
    |   ChoicePoint    |      |   ChoicePoint    |
2   | Continuation = 1 |      | Continuation = 0 |
    |   Fallback = y   |      |   Fallback = y   |
    +------------------+  =>  +------------------+
1   |        z         |      |        z         |
    +------------------+      +------------------+
0   |       ...        |      |       ...        |
```

When backtracking, we’ll discard any stack items on top of the latest choice point.

```csharp
void Backtrack()
{
    while (stack.Count > 0)
    {
        var (indentation, item) = stack.Pop();
        if (item is ChoicePoint(var y, var bufLen, var b, _))
        {
            stack.Push((indentation, y));
            buffer.RemoveRange(bufLen, buffer.Count - bufLen);
            canBacktrack = b;
            return;
        }
    }
}
```

Finally, when we reach the end of a line, any choice points (and the document fragments they were protecting) can be discarded. The `Empty` document does double duty as a tombstone.

```csharp
void Commit()
{
    // why iterate backwards?
    // https://github.com/benjamin-hodgson/Gutenberg/blob/9ce96d354806ccb7d9c33d89514108045951b3b8/Gutenberg/LayoutEngine.cs#L395-L400
    for (var i = stack.Count - 1; i >= 0; i--)
    {
        var (_, item) = stack[i];
        if (stack[i] is ChoicePoint cp)
        {
            // Everything between a ChoicePoint and
            // its continuation has been written to
            // the renderer.
            for (var j = cp.Continuation + 1; j <= i; j++)
            {
                stack[j] = (0, new Empty());
            }
        }
    }
}
```
