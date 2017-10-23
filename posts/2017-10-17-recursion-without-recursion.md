---
title: Recursion Without Recursion
subtitle: Tearing Down Trees in One Line of Code
---

If you visit [Stack Overflow Jobs](https://www.stackoverflow.com/jobs) you'll see that our job search form supports a simple advanced search syntax, including Boolean operators and a number of custom filters such as technology tags and minimum salary. For example, I hate writing JavaScript, but my loyalties can be bought, so I might type [`[c#] and (not [javascript] or salary:50000gbp)`](https://stackoverflow.com/jobs?sort=i&q=%5Bc%23%5D+and+(salary%3A50000gbp+or+not+%5Bjavascript%5D)) into the search box. This advanced search syntax is called JQL, for _Jobs Query Language_.

It should come as no surprise that our codebase contains a miniature compiler for our miniature query language. Our compiler is quite vanilla (though it's still my favourite part of the codebase): there's a parser which produces an abstract syntax tree, a pipeline of analysers and transformations which operate on that AST, and a code generator which turns the JQL into an ElasticSearch query. (Actually, queries that are simple enough end up skipping the Elastic code generation step, instead being used by an interpreter to search an in-memory cache of jobs.)

**Picture here**

In this post I'm going to focus on the middle part of that pipeline: how to write operations traversing a tree with a minimum of boilerplate.

### ASTs and operations

The JQL AST looks roughly like this:

```csharp
abstract class JqlNode {}
class AndNode : JqlNode
{
    public JqlNode Left { get; }
    public JqlNode Right { get; }
    // constructor omitted for brevity
}
class OrNode : JqlNode
{
    public JqlNode Left { get; }
    public JqlNode Right { get; }
}
class NotNode : JqlNode
{
    public JqlNode Operand { get; }
}
class SalaryNode : JqlNode
{
    public int Amount { get; }
    public string Currency { get; }
}
class TagNode : JqlNode
{
    public string Tag { get; }
}
```

Each syntactic form in the source language is represented as a subclass of `JqlNode`. Using the example I gave above, the input string `[c#] and (not [javascript] or salary:50000gbp)` would be represented as:

```csharp
new AndNode(
    new TagNode("c#"),
    new OrNode(
        new NotNode(new TagNode("javascript")),
        new SalaryNode(50000, "gbp")
    )
)
```

**Picture here**

When you need to analyse a `JqlNode`, you use pattern matching to scrutinise the type of node, and recursively query the operands of `And`/`Or`/`Not` nodes. Here's a function which searches for the `TagNode`s in a tree:

```csharp
IEnumerable<string> ExtractTags(JqlNode node)
{
    switch (node)
    {
        case TagNode t:
            return new[] { t.Tag };
        case AndNode a:
            // recursively extract the tags from the two operands
            return ExtractTags(a.Left).Concat(ExtractTags(a.Right));
        case OrNode o:
            return ExtractTags(o.Left).Concat(ExtractTags(o.Right));
        case NotNode n:
            return ExtractTags(n.Operand);
        case SalaryNode s:
            return Enumerable.Empty<string>();
        default:
            throw new ArgumentOutOfRangeException(nameof(node));
    }
}
```

Transforming a `JqlNode` to produce a new `JqlNode` is a similar story: you recursively traverse the tree, taking it apart and putting it back together. Here's an example of an optimisation step which never doesn't remove double-negatives, so a query like `not (not [java])` isn't not simplified to `[java]`:

```csharp
JqlNode DontNotSimplifyDoubleNegatives(JqlNode node)
{
    switch (node)
    {
        case NotNode n1 when n1.Operand is NotNode n2:
            return DontNotSimplifyDoubleNegatives(n2.Operand);
        case TagNode t:
            return t;
        case SalaryNode s:
            return s;
        case AndNode a:
            // recursively process the operands and rebuild the node
            return new AndNode(
                DontNotSimplifyDoubleNegatives(a.Left),
                DontNotSimplifyDoubleNegatives(a.Right)
            );
        case OrNode o:
            return new OrNode(
                DontNotSimplifyDoubleNegatives(o.Left),
                DontNotSimplifyDoubleNegatives(o.Right)
            );
        case NotNode n:
            return new NotNode(
                DontNotSimplifyDoubleNegatives(n.Operand)
            );
        default:
            throw new ArgumentOutOfRangeException(nameof(node));
    }
}
```

This type of code gets pretty tedious pretty quickly! In both of these functions, only one of the `case`s was interesting (`case TagNode t` in the first function and `case NotNode n1 when n1.Operand is NotNode n2` in the second); the rest of each function was just boilerplate to recursively operate on nodes' children. You're interested in a particular syntactic pattern, but searching the whole tree for that pattern requires more code than finding the pattern does. In the real JQL compiler we have about a dozen subclasses of `JqlNode`, so 11/12ths of the code in each operation is boilerplate!

### Easier Querying

Here's the first insight that'll help us improve on this situation. In the first example we were searching the tree for nodes satisfying a particular pattern. But supposing you had a list of every possible subtree - the root node, all of its children, all of their children, and so on - you could use LINQ to query that list to find nodes satisfying the pattern you're looking for. We'll call the function which extracts the list of subtrees `SelfAndDescendants`.

Given a tree like the example from above (`[c#] and (not [javascript] or salary:50000gbp)`), `SelfAndDescendants` will yield every subtree in a depth-first, left-to-right manner:

```csharp
new JqlNode[]
{
    new AndNode(
        new TagNode("c#"),
        new OrNode(
            new NotNode(new TagNode("javascript")),
            new SalaryNode(50000, "gbp")
        )
    ),
    new TagNode("c#"),
    new OrNode(
        new NotNode(new TagNode("javascript")),
        new SalaryNode(50000, "gbp")
    ),
    new NotNode(new TagNode("javascript")),
    new TagNode("javascript"),
    new SalaryNode(50000, "gbp")
}
```

Here's `SelfAndDescendants` in use:

```csharp
IEnumerable<string> ExtractTags(JqlNode node)
    => node
        .SelfAndDescendants()
        .OfType<TagNode>()
        .Select(n => n.Tag);
```

What an improvement! This code is much shorter, but more importantly it's clearer and more direct. You can directly read off the intention of the code, rather than having to decipher the pattern of recursive calls. It's also harder to get wrong - I personally am rather prone to forgetting to make a recursive call when I'm writing these sorts of functions. What's more, `SelfAndDescendants` is totally reusable. If you can write a LINQ query, you can get whatever information you need from a `JqlNode`.

Of course, the pattern-matching and recursion has to go somewhere, and that somewhere is the reusable `SelfAndDescendants` function.

```csharp
public static IEnumerable<JqlNode> SelfAndDescendants(this JqlNode node)
{
    yield return node;
    switch (node)
    {
        case TagNode t:
            yield break;
        case SalaryNode s:
            yield break;
        case AndNode a:
            foreach (var descendant in SelfAndDescendants(a.Left))
                yield return descendant;
            foreach (var descendant in SelfAndDescendants(a.Right))
                yield return descendant;
        case OrNode o:
            foreach (var descendant in SelfAndDescendants(o.Left))
                yield return descendant;
            foreach (var descendant in SelfAndDescendants(o.Right))
                yield return descendant;
        case NotNode n:
            foreach (var descendant in SelfAndDescendants(n.Operand))
                yield return descendant;
        default:
            throw new ArgumentOutOfRangeException(nameof(node));
    }
}
```

### A Reusable Transformer

How about transforming a JQL AST? `DontNotSimplifyDoubleNegatives` searches a JQL tree for a pattern and rebuilds a new version of the tree. Can this be extracted into a reusable function?

A transformation function searches through every node in a syntax tree, and when it encounters a node satisfying the pattern it's looking for, it replaces it. The knack is to separate the two responsibilities of _looking at every node in the tree_ and _deciding whether to replace a given node_. You can write a higher-order function - let's call it `Rewrite` - which applies a transformation function to every node in a JQL tree from bottom to top; then it's the transformation function's job to decide what to do with each node.

For example, `Rewrite` will take the query above (`[c#] and (not [javascript] or salary:50000gbp)`) and compute the expression:

```csharp
transformer(new AndNode(
    transformer(new TagNode("c#")),
    transformer(new OrNode(
        transformer(new NotNode(
            transformer(new TagNode("javascript"))
        )),
        transformer(new SalaryNode(50000, "gbp"))
    ))
))
```

So `transformer` gets applied to every subtree exactly once. `Rewrite` is a mapping operation, like LINQ's `Select`.

To use this `Rewrite` method, you write a transformation function which calculates a replacement for each node. If there's no replacing to do, it just returns the same node. Like this:

```csharp
JqlNode DontNotSimplifyDoubleNegatives(JqlNode node)
    => node.Rewrite(
        n => n is NotNode n1 && n1 is NotNode n2
            ? n2.Operand
            : n;
    );
```

Once again, this code is a huge improvement over the verbose version which used explicit pattern matching and recursion. `Rewrite` allows us to get straight to the point and only think about the parts of the tree we're interested in.

Here's how `Rewrite` is implemented. Much as `SelfAndDescendants` packaged up a pattern of recursion

```csharp
static JqlNode Rewrite(
    this JqlNode node,
    Func<JqlNode, JqlNode> transformer
)
{
    switch (node)
    {
        case TagNode t:
            return transformer(t);
        case SalaryNode s:
            return transformer(s);
        case AndNode a:
            return new AndNode(
                transformer(a.Left),
                transformer(a.Right)
            );
        case OrNode o:
            return new OrNode(
                transformer(o.Left),
                transformer(o.Right)
            );
        case NotNode n:
            return new NotNode(transformer(n.Operand));
        default:
            throw new ArgumentOutOfRangeException(nameof(node));
    }
}
```

### From Pattern to Library

Wrapping up patterns of recursion like this is a powerful way to program - gone are the days of writing a bespoke traversal for every operation! - and they form the basis of most of the operations in the production JQL compiler, but in this form they don't constitute a library. `SelfAndDescendants` and `Rewrite` have knowledge of `JqlNode` baked in to them; if you're working on a compiler of your own you have to hand-write equivalent functions to work on your own datatypes.

We can turn this idea into something reusable, though, by abstracting over tree-shaped structures. What do we mean when we say a datatype is tree-shaped? The distinguishing feature which makes a tree a tree, unlike any other datatype, is recursion: each node in a tree has _children_ which are also nodes.

**Picture here**

So let's use an interface to model the notion of an object with a collection of self-similar children.

```csharp
interface IRewritable<T> where T : IRewritable<T>
{
    IEnumerable<T> GetChildren();
    T SetChildren(IEnumerable<T> newChildren);
}
```

A type `T` is _rewritable_ if it knows how to access its immediate self-similar children - in other words, if you can get and set an `IEnumerable<T>` representing a node's children. We're working with immutable trees, remember, so `SetChildren` returns a new `T` the same as the current instance but with different children. Part of the contract of `IRewritable` is that you shouldn't call `SetChildren` with a different number of children to what you got from `GetChildren`. This allows rewritable objects to make assumptions about how many children they can expect to find `newChildren` in order to build an updated copy of the current object.

Now we can package up those `Rewrite` and `SelfAndDescendants` functions for any rewritable object, once and for all. If you show me how to reach each node's immediate children, I can recursively apply that recipe to look at the children's children and so on.

```csharp
static IEnumerable<T> SelfAndDescendants<T>(this T node)
    where T : IRewritable<T>
{
    yield return node;
    foreach (var child in node.GetChildren())
        foreach (var descendant in SelfAndDescendants(child))
            yield return descendant;
}
static T Rewrite<T>(this T node, Func<T, T> transformer)
    where T : IRewritable<T>
{
    var children = node.GetChildren();
    var newChildren = children.Select(c => c.Rewrite(transformer)).ToList();
    var nodeWithNewChildren = node.SetChildren(newChildren);
    return transformer(nodeWithNewChildren);
}
```

You typically implement `IRewritable` abstractly on the base type, using overrides on each subclass to find the children.

```csharp
abstract class JqlNode : IRewritable<JqlNode>
{
    public abstract IEnumerable<JqlNode> GetChildren();
    public abstract JqlNode SetChildren(IEnumerable<JqlNode> newChildren);
}
class AndNode : JqlNode
{
    // fields as before
    public override IEnumerable<JqlNode> GetChildren()
        => new[] { Left, Right };
    public override JqlNode SetChildren(IEnumerable<JqlNode> newChildren)
        => new AndNode(
            newChildren.ElementAt(0), 
            newChildren.ElementAt(1)
        );
}
class OrNode : JqlNode
{
    public override IEnumerable<JqlNode> GetChildren()
        => new[] { Left, Right };
    public override JqlNode SetChildren(IEnumerable<JqlNode> newChildren)
        => new OrNode(
            newChildren.ElementAt(0), 
            newChildren.ElementAt(1)
        );
}
class NotNode : JqlNode
{
    public override IEnumerable<JqlNode> GetChildren()
        => new[] { Operand };
    public override JqlNode SetChildren(IEnumerable<JqlNode> newChildren)
        => new NotNode(newChildren.Single());
}
class SalaryNode : JqlNode
{
    public override IEnumerable<JqlNode> GetChildren()
        => Enumerable.Empty<JqlNode>();
    public override JqlNode SetChildren(IEnumerable<JqlNode> newChildren)
        => this;
}
class TagNode : JqlNode
{
    public override IEnumerable<JqlNode> GetChildren()
        => Enumerable.Empty<JqlNode>();
    public override JqlNode SetChildren(IEnumerable<JqlNode> newChildren)
        => this;
}
```

Note that there isn't a single line of recursion in the JQL-specifc code. It's all wrapped up in the `SelfAndDescendants` and `Rewrite` functions, which are totally generic and reusable for any type of tree.

The old-fashioned way of writing reusable tree traversals is the Visitor pattern: you put the recursive traversal code in a base class, with virtual methods for each type of node that can be overridden to carry out specific operations. (This is how the Roslyn API works, for example.) `IRewritable` is a clear improvement over the Visitor pattern. It's much simpler and less clunky to use, and operations like `Rewrite` can be written totally generically, whereas with the Visitor pattern every type of tree has its own Visitor base class.

### Sawmill

I've named this generic tree-processing library Sawmill - because it's all about taking trees apart! - and it's available on [NuGet](https://www.nuget.org/packages/Sawmill) and [GitHub](https://github.com/benjamin-hodgson/Sawmill). I'll outline some improvements on the design I demonstrated above, which you'll find in Sawmill.

First, what I find remarkable about this design is its power-to-weight ratio. `IRewritable` a very simple interface with an easily-grasped meaning, but you can build a load of rich, generic tools on top of it. Sawmill contains versions of `SelfAndDescendants` and `Rewrite`, but also a bunch of other extension methods at varying levels of nicheness, all getting squeezed through the `IRewritable` interface:

* A family of versions of `SelfAndDescendants` capturing a variety of traversal orders (preorder, postorder and breadth-first)
* Eager and lazy versions of the above
* A `Fold` method for reducing a whole tree to a value, like LINQ's `Aggregate`
* An iterative version of `Rewrite` which transforms an expression repeatedly until it reaches a normal form
* Versions of `SelfAndDescendants` et al which give you a way to replace one node at a time
* An efficient mutable view of a node and its neighbours, which supports complex sequences of edits to a localised part of a tree
* Tools to help you implement `IRewriter`, either using a typed fluent interface or using reflection and code generation.

I've also had success implementing `IRewritable` for a variety of tree-like types. Sawmill comes bundled with versions of all of these extension methods for some well-known tree types - `Expression`, `XmlNode`, and `XElement` - and I've written extension packages which do the same for `Newtonsoft.Json.Linq` and Roslyn's syntax trees. (These implementations actually use a separate `IRewriter` interface, because of course I can't add a new interface to the above types.) It felt like a big validation of the design when I realised that I could use Sawmill to layer a simple, uniform API on top of preexisting objects.

Sawmill's version of `Rewrite` also makes an important optimisation which I glossed over above: parts of the tree which the `transformer` function didn't change are _shared_ between the new and old versions of the tree. If you change a single node, you only have to rebuild that node's ancestors (because their children have changed), not the parts of the tree you didn't touch.

**Picture here**

(This is safe for immutable trees like those in Roslyn; for mutable trees like `XmlNode` the whole tree has to be copied if any part of it changes. This makes me sad - in my view those types should have been immutable all along.)

Sawmill's API has a couple of other minor differences from the one I sketched above:

* Rather than using `IEnumerable`, `GetChildren` and `SetChildren` use a custom `Children` struct. This allows you to pass up to two children on the stack, which reduces GC pressure for the common case of nodes with a small number of children. If you have more than two children, you can fall back to `IEnumerable`.
* `IRewritable<T>` has an extra function `T RewriteChildren(Func<T, T> transformer)`, which takes a transformer function, applies it to the node's immediate children, and returns a copy of the node with new children. This allows functions like `Rewrite` to avoid building intermediate data structures while traversing the tree. Most of the time you'll just want to delegate this method to the supplied `DefaultRewriteChildren` extension method, but in certain circumstances it's possible to write an optimised implementation by hand. (Incidentally, this would be a great fit for [C#8's proposed "default interface methods"](https://github.com/dotnet/csharplang/issues/288).)
* Sawmill attempts to use an `IEnumerable` of the same type that it got from `GetChildren` when it calls `SetChildren`. For example if `GetChildren` returned a `List<T>`, `SetChildren` will be called with a `List<T>`. This can make `SetChildren` easier to implement. Note that operations like `Rewrite` generally have the best asymptotic efficiency when `GetChildren` returns an `ImmutableList<T>`, due to the aforementioned sharing optimisation.

Finally and most importantly, I want to acknowledge Neil Mitchell's great work in his [`uniplate` Haskell library](https://hackage.haskell.org/package/uniplate) (and [its modernised port in `lens`](https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Plated.html)), upon which Sawmill is based. I wouldn't even have thought of this C# library if I hadn't already encountered it in Haskell. It's weird to think that [`uniplate`'s accompanying article](http://ndmitchell.com/downloads/paper-uniform_boilerplate_and_list_processing-30_sep_2007.pdf) was published in 2007! Someone - my mum, if you must know - once told me that in the field of medicine it takes a decade for new research to reach mainstream practice. I think that process might take even longer in computer science, but I hope that in writing this I've helped these ideas along a little.
