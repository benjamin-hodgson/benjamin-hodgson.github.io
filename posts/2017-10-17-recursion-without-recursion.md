---
title: Recursion Without Recursion
subtitle: Tearing Down Trees in a Single Line of Code
---

This post is an explanation of my library [Sawmill](https://github.com/benjamin-hodgson/Sawmill) from first principles.

If you visit [Stack Overflow Jobs](https://www.stackoverflow.com/jobs) you'll see that our job search form supports a simple advanced search syntax, including Boolean operators and a number of custom filters such as technology tags and minimum salary. For example, I hate writing JavaScript, but my loyalties can be bought, so I might type [`[c#] and (salary:50000gbp or not [javascript])`](https://stackoverflow.com/jobs?sort=i&q=%5Bc%23%5D+and+(salary%3A50000gbp+or+not+%5Bjavascript%5D)) into the search box. We call this advanced search syntax JQL, for _Jobs Query Language_.

It should come as no surprise that our codebase contains a miniature compiler for our miniature query language. Our compiler is quite vanilla (though it's still my favourite part of the codebase): there's a parser which produces an abstract syntax tree, a pipeline of analysers and transformations which operate on that AST, and a code generator which turns the JQL into an ElasticSearch query. A certain class of simple queries end up skipping the Elastic code generation step, instead being used to search an in-memory cache of jobs.

**Picture here**

In this post I'm going to focus on the middle part of that pipeline: how to write operations traversing a tree with a minimum of boilerplate.

**link to NDC?**

### ASTs and operations

The JQL AST looks roughly like this:

```csharp
abstract class JqlNode {}
class AndNode : JqlNode
{
    public JqlNode Left { get; }
    public JqlNode Right { get; }
    // constructors omitted for brevity
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

(The real code isn't exactly like this. Notably, the real `JqlNode` has quite a few more subclasses, to represent a number of other syntactic forms.)  Using the example I gave above, the input string `[c#] and (salary:50000gbp or not [javascript])` would be represented as `new AndNode(new TagNode("c#"), new OrNode(new SalaryNode(50000, "gbp"), new NotNode(new TagNode("javascript"))))`.

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

Transforming a `JqlNode` to produce a new `JqlNode` is a similar story: you recursively traverse the tree, taking it apart and putting it back together. Here's there isn't no example of an optimisation step which never doesn't remove double-negatives, so queries like `not (not [java])` aren't not simplified to just `[java]`:

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

Here's the first insight that'll help us improve on this situation. In the first example we were searching the tree for nodes satisfying a particular pattern. But supposing you had a list of every possible subtree - the root node, all of its children, all of their children, and so on - you could use LINQ to query that list to find nodes satisfying the pattern you're looking for.

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

Given a tree like the example from above (`[c#] and (salary:50000gbp or not [javascript])`), `SelfAndDescendants` will yield every subtree in a depth-first, left-to-right manner:

```csharp
new JqlNode[]
{
    new AndNode(
        new TagNode("c#"),
        new OrNode(
            new SalaryNode(50000, "gbp"),
            new NotNode(new TagNode("javascript"))
        )
    ),
    new TagNode("c#"),
    new OrNode(
        new SalaryNode(50000, "gbp"),
        new NotNode(new TagNode("javascript"))
    ),
    new SalaryNode(50000, "gbp"),
    new NotNode(new TagNode("javascript")),
    new TagNode("javascript")
}
```

### A Reusable Transformer

How about transforming a JQL AST? `DontNotSimplifyDoubleNegatives` searches a JQL tree for a pattern and rebuilds a new version of the tree. Can this be extracted into a reusable function?

A transformation function searches through every node in a syntax tree, and when it encounters a node satisfying the pattern it's looking for, it replaces it. The knack is to separate the two responsibilities of _looking at every node in the tree_ and _deciding whether to replace a given node_.

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

**explanation with code example here**

```csharp
JqlNode DontNotSimplifyDoubleNegatives(JqlNode node)
    => node.Rewrite(
        n => n is NotNode n1 && n1 is NotNode n2
            ? n2.Operand
            : n;
    );
```

**something about visitors**

### From Pattern to Library

These two functions are powerful - gone are the days of writing a bespoke traversal for every operation! - and they form the basis of most of the operations in the production JQL compiler, but in this form they don't constitute a library. `SelfAndDescendants` and `Rewrite` have knowledge of `JqlNode` baked in to them; if you're working on a compiler of your own you have to hand-write equivalent functions to work on your own datatypes.

The trick is to abstract over tree-shaped structures. You can model a tree as an object which has a collection of children. If you show me how to reach each node's immediate children, I can recursively apply that recipe to look at the children's children and so on.

**Picture here**

Here's how that looks as an interface. A type `T` is _rewritable_ if it knows how to access its immediate self-similar children - in other words, if you can get and set an `IEnumerable<T>` representing a node's children. We're working with immutable trees, remember, so `SetChildren` returns a new `T` the same as the current instance but with different children.

Part of the contract of `IRewritable` is that you shouldn't call `SetChildren` with a different number of children to what you got from `GetChildren`. This allows implementations to make assumptions about how many children they can expect to find `newChildren` in order to build an updated copy of the current object.

```csharp
interface IRewritable<T> where T : IRewritable<T>
{
    IEnumerable<T> GetChildren();
    T SetChildren(IEnumerable<T> newChildren);
}
```

Note the way I'm using the generic type constraint that looks like it's folding in on itself. It's meant to constrain `T` to be equal to the concrete type implementing `IRewritable`. (Think about it - if `JqlNode` implements `IRewritable<JqlNode>`, then it's valid to use it as the `T` in `IRewritable<T>`, so `IRewritable<JqlNode>` is a valid construction!) [I've mentioned this type of constraint before](https://vimeo.com/154564491#t=1656s) - it's called _F-bounded quantification_.

Now we can package up those `Rewrite` and `SelfAndDescendants` functions once and for all.

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
    var newChildren = children.Select(c => c.Rewrite(node, transformer));
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

**something about visitors**

### Sawmill

**other operations, bundled tools**
**sharing optimisation, with picture**
**api differences**
**link to paper**
**link to repo**
