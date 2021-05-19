---
title: Some Notes on Variables and Binding
date: 2021-05-18
---

I've been thinking a bit about how to build a library of tools to handle variables, capture-avoiding substitution, etc, on top of [Sawmill](https://github.com/benjamin-hodgson/Sawmill). Pretty much every compiler needs to deal with variables, and they're notoriously tricky to handle correctly.

I'm a way off having a final design, but I thought I'd publish my unfinished notes, hastily written in an afternoon. You'll see a few Thinking Emojis throughout this article, indicating things I haven't figured out yet. Please do get in touch if any of this gives you ideas.


A quick look at some prior art
------------------------------

* [`unbound`](https://hackage.haskell.org/package/unbound)/[`unbound-generics`](https://hackage.haskell.org/package/unbound-generics)
    * `Name` is an ADT with fixed representation.
        * `Bound` is a depth and an index. Requires patterns to have a canonical ordering, and bind each name only once
        * `Bound` discards the original name, but library API expects patterns to contain `Name`s
    * Statically encoded patterns with built in type combinators.
        * I like:
            * Statically declare structure of binders
            * Embed your own datatypes as required by language syntax --- method header can be `[(Name, Type)]`
        * I don't like:
            * Generics magic to find/traverse patterns
            * Type constructor soup, eg `| LetRec (Bind (Rec [(Name, Embed Expr)]) Expr)`. Would be even noisier in C#.
    * Traverse your own terms
        * Most users will be using `RepLib`/`GHC.Generics`
        * Lib uses `RepLib`/`GHC.Generics` internally to traverse patterns

* [`bound`](https://www.schoolofhaskell.com/user/edwardk/bound)
    * Choose your own name (`data Expr a = Var a | ...`).
    * Choose your own pattern/index.
        * Library explicitly doesn't manage patterns or names. It just does substitution using your monad. `Scope` doesn't contain a "pattern" object.
        * Me gusta
    * Nested datatype manages de Bruijn depth statically.
        * Nested datatype is quite awkward in practice --- doesn't play nicely with mutual recursion or Plated.
        * Clever trick in `Scope` to speed up shifting at cost of canonicity. Prob a bit of a gimmick unless you're dealing with huge trees --- but also, not super costly complexity-wise.
    * Works very nicely with standard classes: `Foldable`/`Traversable` to look at FVs, `Functor` for renaming, `Monad` for substitution. `Eq1` for alpha equivalence, etc.
    * Overall, probably not such a good fit for C# since we don't have `Monad` etc.

* [`moniker`](https://github.com/brendanzab/moniker)
    * More or less a port of `unbound`.
    * Choose your own name (`Free<N>`/`Bound<N>`; `trait BoundTerm<N>` etc).
    * Library defines a couple of traits (`BoundTerm`/`BoundPattern`) to locate variables, do substitution, etc.
    * Traverse your own terms, but `derive` magic assists with implementing the library's traits.


Representing Names
------------------

I definitely want to use de Bruijn indexes (managed by the library) for bound variables.

```csharp
readonly struct Bound
{
    public int Depth { get; }  // how many binding sites up should I look?
    public int Index { get; }  // where inside the binding site should I look?
}
```

For now, I'm not storing names in `Bound`. If you want to know the original name, go and find it in the pattern. Use of `int` for `Index` requires patterns to have a canonical ordering (and bind each variable only once).

Free variables: need an easy way to freshen them.

```csharp
readonly struct Free
{
    public string OriginalName { get; }
    public int Freshness { get; }
}
```

Now just union them to represent names.

```csharp
readonly struct Name
{
    public Bound Bound { get; }
    public Free Free { get; }
    public Name(Bound bound)
    {
        Bound = bound;
        Free = default;
    }
    public Name(Free free)
    {
        Bound = default;
        Free = free;
    }
    public bool IsBound => free.OriginalName == null;
    public bool IsFree => free.OriginalName != null;
}
```

How much of this should be public? Should these be structs or classes? (`Name` is like 4 words.) 🤔


Representing Scopes
-------------------

I definitely want an explicit type to represent a scope. I personally think `BindingSite` is a slightly better name, although perhaps that should be reserved for names inside patterns? 🤔

The idea is to treat `BindingSite` as an abstract data type. To create a `BindingSite` (such as when parsing a lambda expression), you call `Binder.Bind(variables, body)`. The binder traverses the `body` AST, looking for variables which are bound by the lambda and converting them to `Bound` de Bruijn indexes. Likewise, when you need to go inside a `BindingSite`, you have to explicitly unbind it.

Don't want to discard the original names, of course.

**Idea 1**: Store the original name with the `Bound` variable (just as an annotation, ignore in alpha-equivalence etc).

```csharp
readonly struct Bound
{
    public int Depth { get; }
    public int Index { get; }
    public Free OriginalName { get; }
}
```

Potential consistency issues --- different mentions of the same variable could end up with different tags.

**Idea 2**: Store a flat list of original names in the `BindingSite`.

```csharp
readonly struct BindingSite<T>  // aka Scope (bound/moniker) or Bind (unbound)
{
    // How much of this should be public? 🤔
    public ImmutableArray<Name> OriginalNames { get; }
    public T Body { get; }
}
```

It feels kinda ugly. Also potential consistency issues --- if your binding sites have more syntactic structure than that (patterns, type signatures, etc), you're gonna end up duplicating information. Also, it doesn't seem like this would easily support recursive patterns such as letrec or Agda's dot notation.

**Idea 3**: Store the actual pattern in the `BindingSite`. This is more in line with how `unbound`/`moniker` do it.

```csharp
readonly struct BindingSite<T>
{
    public Pattern Pattern { get; }
    public T Body { get; }
}
abstract class Pattern {}
class Variable : Pattern  // Do I need to support "anonymous" variables? 🤔
{
    public Name Name { get; }
}
class Embed<T> : Pattern
{
    public T Embedded { get; }
}
class Telescope : Pattern  // known as Rebind in other libs
{
    public BindingSite<Pattern> BindingSite { get; }
}
class Rec : Pattern
{
    // technically this is a binding site too. 🤔
    public Pattern Body { get; }
}
```

Some things about this work really well. We can make `Pattern` extensible by having it implement `IRewritable` abstractly. I define a few base patterns which my library recognises, and you can add your own. To (eg) find the variables in a pattern I just need to be able to traverse your custom pattern types with `GetChildren`.

```csharp
abstract class Pattern : IRewritable<Pattern>
{
    // ...
    public IEnumerable<Free> GetNames()
        => this
            .SelfAndDescendants()
            .OfType<Variable>()
            // names which aren't free in a pattern are recursive
            // references to other names bound in this pattern
            .Where(v => v.Name.IsFree)
            .Select(x => x.Name.Free);
}
```

Ways this doesn't scale:
* If your language has multiple types of patterns (eg, type variables and value variables) you can't statically distinguish them.
* The structure of the pattern lives only at runtime --- it all gets stuffed into a `Pattern`-typed variable. You can't _statically_ encode the syntactic structure of the binding site (at which point it's not a whole lot better than Idea 2).

What should be the advice for implementing `IRewritable` on objects containing a binding site? Should you traverse to the body? (Problematic because de Bruijn indexes from different scopes are not comparable.) Are you meant to unbind the body? That would mean you can't use `IRewritable` as is, because `IRewritable` doesn't have a parameter for a source of fresh names. Perhaps we should be using `IRewriter` and not `IRewritable`, although that's a messier API. 🤔

> This ☝🏻 is a pressing concern because `Telescope` features a `BindingSite`!


**Idea 3.1**: Statically type the `Pattern`.

```csharp
readonly struct BindingSite<TPattern, T> where TPattern : Pattern
{
    public TPattern Pattern { get; }
    public T Body { get; }
}
class Telescope<L, R> : Pattern
    where L : Pattern
    where R : Pattern
{
    // open question on how to treat BindingSite with IRewritable
    public BindingSite<L, R> BindingSite { get; }
}
class Rec<T> : Pattern
    where T : Pattern
{
    public T Body { get; }
}
```

Some things about this work really well too, as evidenced by its use in `unbound`/`moniker`. I find the "type soup" usability issue a bit concerning, but more pressingly it doesn't play nicely with `IRewritable`.

```csharp
class Telescope<L, R>
{
    // ...
    public int CountChildren() => 2;
    public void GetChildren(Span<Pattern> children)
    {
        // debatably we should only be descending to the Pattern,
        // because the Body is in a different scope
        children[0] = BindingSite.Pattern;
        children[1] = BindingSite.Body;
    }
    public Pattern SetChildren(ReadOnlySpan<Pattern> children)
        => new Telescope<L, R>((L)children[0], (R)children[1]);
        //                      ^ damn!
}
```

That unsafe cast really is unsafe, because a `Rewrite` operation is liable to change the type of the pattern. Maybe patterns should be read-only? Do I need to split the read/write parts of `IRewritable`? 🤔


ASTs with Binding Structure
---------------------------

This part is fairly worked out I think. I'm assuming that your syntax tree has a case for variables, containing a `Name` (and no children) and a case for binding sites, containing a `BindingSite` (and no children). So all we need to add to `IRewritable` is a way to identify those nodes:

```csharp
interface IBindable<T> : IRewritable<T> where T : IBindable<T>
{
    bool TryGetName(out Name name);
    T SetName(Name newName);

    bool TryGetBindingSite(out BindingSite<T> bindingSite);
    T SetBindingSite(BindingSite<T> newBindingSite);
}
```


Freshening
----------

When you go under a binder you (usually) want to replace the de Bruijn indexes in there with named variables. Those variables need to be _fresh_, that is, they need to not clash with (or _capture_) any other variables in scope. As a UX concern, you probably want the fresh name to be somewhat similar to the name the programmer typed.

So we have `IFreshener`:

```csharp
interface IFreshener
{
    Free Freshen(Free name);
}
```

And a straightforward implementation with a global counter.

```csharp
class Freshener : IFreshener
{
    private int _counter = 0;
    public Free Freshen(Free name)
    {
        _counter++;  // Interlocked.Increment if you need thread safety
        return new Free(name.OriginalName, _counter);
    }
}
```

As an optimisation (and as a UX improvement), you can avoid freshening variables if you're sure they don't clash with any other names which are in scope. That means keeping track of the set of names we want to avoid:

```csharp
interface IFreshener
{
    Free Freshen(Free name);
    void EnterScope(IEnumerable<Free> names);
    void ExitScope();  // free up the names from the last EnterScope call
}
class Freshener : IFreshener
{
    public Free Freshen(Free name) { /* ... */ }
    public void EnterScope(IEnumerable<Free> names) {}
    public void ExitScope() {}
}
class LocalFreshener : IFreshener
{
    // use a better data structure in practice, obv
    private readonly Stack<IEnumerable<Free>> _names = new();
    public Free Freshen(Free name)
    {
        while (_names.Any(ns => ns.Contains(name)))
        {
            name = name.IncrementFreshness();
        }
        return name;
    }
    public void EnterScope(IEnumerable<Free> names)
    {
        _names.Push(names);
    }
    public void ExitScope()
    {
        _names.Pop();
    }
}
```


Traversing the tree
-------------------

OK, finally we have everything we need to bind and unbind variables in ASTs. Let's suppose we went with **Idea 2** from above --- just storing a flat list of names at the binding site.

```csharp
static class Binder
{
    public static BindingSite<T> Bind<T>(ImmutableArray<Free> variables, T body) where T : IBindable<T>
    {
        var variablesLookup = variables
            .Select((x, i) => new KeyValuePair<Free, int>(x, i))
            .ToImmutableDictionary();

        var level = 0;
        T Go(T x)
        {
            if (x.TryGetName(out var name))  // x is a variable
            {
                if (name.IsFree && variablesLookup.TryGetValue(name.Free, out var ix))
                {
                    return x.SetName(new Name(new Bound(level, ix)));
                }
                return x;
            }
            else if (x.TryGetBindingSite(out var bs))  // x is a binding site
            {
                level++;
                var newBody = Go(bs.Body);
                level--;
                return x.SetBindingSite(bs.WithBody(newBody));
            }
            else
            {
                return x.RewriteChildren(Go);
            }
        }
        return new BindingSite<T>(variables, Go(body));
    }

    public static (ImmutableArray<Free>, T) Unbind<T>(BindingSite<T> bindingSite, IFreshener freshener) where T : IBindable<T>
    {
        var variables = bindingSite
            .Variables
            .Select(freshener.Freshen)
            .ToImmutableArray();

        var level = 0;
        T Go(T x)
        {
            if (x.TryGetName(out var name))
            {
                if (name.IsBound && name.Bound.Level == level)
                {
                    return x.SetName(new Name(variables[name.Bound.Index]));
                }
                return x;
            }
            else if (x.TryGetBindingSite(out var bs))
            {
                level++;
                var newBody = Go(bs.Body);
                level--;
                if (!ReferenceEquals(bs.Body, newBody))
                {
                    return x.SetBindingSite(bs.WithBody(newBody));
                }
                return x;
            }
            else
            {
                return x.RewriteChildren(Go);
            }
        }
        return (variables, Go(bindingSite.Body));
    }
}
```

You can develop versions of `Rewrite`, `SelfAndDescendants`, and so on, which appropriately `Bind` and `Unbind` variables inside the tree.

There are a couple of design issues regarding the performance of `Bind`/`Unbind`. Each call traverses and rewrites the whole tree and potentially chews through a bunch of fresh names, so if you bind and unbind a lot then you're gonna have performance problems.

There might be room for improvement here --- off the top of my head, you could rewrite everything up to the next binding site and just store a list of "changes to be applied" there. You can defer applying the changes until you're asked to open that binding site. I suspect that'd give you asymptotic speedups (quadratic -> linear?) for certain algorithms which do lots of renaming.

Also, some tree operations don't require you to `Unbind` and re-`Bind` every single binding site. If you're doing some sort of operation where you're not generating new names or binding sites, or moving nodes across binding sites, you can just leave `Bound` variables where they are. That's an argument for making (eg) `bindingSite.Body` public, and going under it in `GetChildren`/`SetChildren`. Although in practice that's the sort of thing which is hard to get right and can cause subtle (or not-so-subtle) bugs in your language implementation.
