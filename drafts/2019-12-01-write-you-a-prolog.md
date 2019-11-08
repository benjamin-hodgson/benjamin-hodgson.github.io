---
title: Write You a Prolog
---

I figured it'd be useful to have some examples of my language tooling libraries [Sawmill](https://github.com/benjamin-hodgson/Sawmill) and [Pidgin](https://github.com/benjamin-hodgson/Pidgin) in action. I thought it could be fun to use them to write a miniature Prolog interpreter!

1. **Introduction & Syntax**
2. Parsing
3. Unification
4. Proof search


Whistle-Stop Introduction to Prolog
-----------------------------------

Prolog is a _logic programming_ language. Prolog code consists of a collection of _rules_. Each rule says "X is true if Y (and Z and...) is true". A rule is a logical axiom, with a set of premises (on the right) and a conclusion you can draw from those premises (on the left).

As an example, you might say that a `Person` wants a certain `Food` if they're hungry and they like that food. And --- I don't know about you --- but I'll eat something I really love even if I'm not hungry.

```prolog
wants(Person, Food) :- hungry(Person), likes(Person, Food).
wants(Person, Food) :- loves(Person, Food).

% If someone loves a given food, then they also like it.
likes(Person, Food) :- loves(Person, Food).

% If we're going to have dinner, we'd better agree on what to eat.
dinner(Person1, Person2, Food) :- wants(Person1, Food), wants(Person2, Food).
```

Variables in Prolog start with a capital letter. `:-` means "if" and `,` means "and". You can give multiple alternative ways of satisfying the same predicate by just declaring it more than once. In other words, multiple declarations of the same predicate means "or".

Next, we'll prime Prolog's database with a couple of _facts_ about people and foods.

```prolog
loves(benjamin, pizza).
loves(benjamin, asparagus).
likes(benjamin, soup).

loves(clio, salad).
likes(clio, pizza).
likes(clio, soup).
hungry(clio).
```

_Atoms_ in Prolog are somewhat like strings. They begin with a lower case letter.

Finally, we can issue a _query_ to the interactive Prolog interpreter to find out whether we can have soup for dinner.

```prolog
?- dinner(benjamin, clio, soup).
false
```

No soup for you. What _can_ we eat for dinner? If we replace the _atom_ `soup` (a specific food) with a _variable_ `Food` --- note the capital letter --- Prolog will try to find a value for `Food` which satisfies the `dinner` predicate. Prolog's proof search system is **bi-directional** --- parameters can serve as both inputs and outputs.

```prolog
?- dinner(benjamin, clio, Food).
Food = pizza
```

You can use as many variables as you like in a query. Prolog will try and solve all of them.


### Pattern Matching and Recursion

As well as putting _conditions_ on the right-hand side of a rule, you can put _patterns_ on the left. This is somewhat like pattern matching in functional languages --- the right-hand side of a rule is only entered if its arguments match the pattern on the left.

Here's a predicate `last(List, Item)` which succeeds when `Item` is the last element of `List`.

```prolog
last(cons(X, nil), X).
last(cons(X, Xs), Y) :- last(Xs, Y).
```

(I'm using [the `cons` nomenclature](https://en.wikipedia.org/wiki/Cons) from Lisp.) Rules in Prolog are attempted from top to bottom. The first clause is the base case, which states the fact that the last item of the singleton list `cons(X, nil)` is `X`. The second clause (which is only entered when `Xs` is not `nil`) recursively calls `last` --- it says the last item of the list `cons(X, Xs)` is `Y` when the last item of `Xs` is `Y`.

This is another example of Prolog's bi-directional proof search system. You can test whether a certain known item is the last element of a list,

```prolog
?- last(cons(apples, cons(pears, cons(oranges, nil))), oranges).
true
?- last(cons(apples, cons(pears, cons(oranges, nil))), bananas).
false
```

or you can leave the second parameter unspecified to use it as an output.

```prolog
?- last(cons(apples, cons(pears, cons(oranges, nil))), X).
X = oranges
```

You can even ask Prolog to search the other way and come up with an example of a list with a given last element.

```prolog
?- last(Xs, oranges).
Xs = cons(oranges, nil)
```

Prolog's bi-directional pattern matching system works by _unification_. When matching a term like `last(Xs, oranges)` to a rule like `last(cons(X, nil), X).`, it tries to find values for all the variables in scope so that the term matches the rule. In this case, it determines that `Xs = cons(X, nil)` and `X = oranges`. I'll talk about unification in much more detail in a later post.


Representing Prolog Syntax
--------------------------

Hopefully blasting through Prolog's core in only a few paragraphs was enough to get you excited about implementing it! The first step in writing an interpreter for a language is to write down some types representing the language's _abstract syntax tree_. I said a Prolog program was a collection of _rules_, so let's start there.

```csharp
class Rule
{
    // ?
}
```

What constitutes a rule? Looking at our example from earlier,

```prolog
wants(Person, Food) :- hungry(Person), likes(Person, Food).
```

you can see that a rule has two main parts, separated by the `:-` symbol. On the left is the _conclusion_ of the logical statement, in the form of a pattern which the rule can match. On the right are the _premises_ of the logical statement, in the form of a comma-separated list of calls to other predicates. We'll call these the `Head` and the `Body` of the rule. (A _fact_ is just a rule with no right-hand side.)


```csharp
class Rule
{
    public ? Head { get; }
    public ImmutableArray<?> Body { get; }
}
```

(I'm omitting constructors for brevity.) What should the types of these properties be? A rule's head is always a name, followed by a comma-separated list of expressions inside parentheses. We'll call this a `Predicate`. A rule's body is also list of predicates.

```csharp
class Rule
{
    public Predicate Head { get; }
    public ImmutableArray<Predicate> Body { get; }
}

class Predicate
{
    public string Name { get; }
    public ImmutableArray<?> Args { get; }
}
```

Now to fill in the type of `Args`. Looking at the example `last(cons(X, nil), X)`, each argument to a predicate can be one of:

1. Another predicate applied to some arguments (`cons` in this example).
2. A variable (`X`).
3. An atom (`nil`).

We'll refer to all three of these syntactic forms as _terms_. We can use subtyping to represent the fact that each argument could be any one of the three.

```csharp
abstract class Term {}
class Predicate : Term
{
    public string Name { get; }
    public ImmutableArray<Term> Args { get; }
}
class Variable : Term
{
    public string Name { get; }
}
class Atom : Term
{
    public string Value { get; }
}
```

That's our whole abstract syntax! Here's how our `last(cons(X, Xs), Y) :- last(Xs, Y)` example would be represented:

```csharp
new Rule(
    head: new Predicate(
        name: "last",
        args: new[]
        {
            new Predicate("cons", new[] { new Variable("X"), new Variable("Xs") }),
            new Variable("Y")
        }
    ),
    body: new[]
    {
        new Predicate("last", new[] { new Variable("Xs"), new Variable("Y") })
    }
)
```

<img src="/images/2019-12-01-write-you-a-prolog/anatomy.png" alt="Anatomy of a Rule" width="900" />

Prolog's proof search system is based entirely on manipulating terms, so these three classes will turn out to be quite important in our little interpreter.


Implementing `IRewritable`
--------------------------

`Term` is an immutable type with a recursive tree-shaped structure --- a predicate's arguments can be any `Term`, including more predicates. My generic programming library [Sawmill](https://github.com/benjamin-hodgson/Sawmill) is filled with tools for working with immutable trees! As a Sawmill user, you implement its core `IRewritable` interface on your tree structure, and Sawmill takes care of much of the boilerplate of traversing the tree for you. (See [my earlier post](/posts/2017-11-13-recursion-without-recursion.html) for an introduction to Sawmill.)

`IRewritable` is all about addressing the immediate children of the current node in a tree. It has three methods, `CountChildren`, `GetChildren`, and `SetChildren`, which we have to implement for each subclass of our `Term` tree. Variables and atoms don't have child terms --- only predicates.

```csharp
abstract class Term : IRewritable<Term>
{
    public abstract int CountChildren();
    public abstract void GetChildren(Span<Term> childrenReceiver);
    public abstract Term SetChildren(ReadOnlySpan<Term> newChildren);
}
class Predicate : Term
{
    // ...
    public override int CountChildren() => Args.Length;
    public override void GetChildren(Span<Term> childrenReceiver)
    {
        Args.CopyTo(childrenReceiver);
    }
    public override Term SetChildren(ReadOnlySpan<Term> newChildren)
        => new Predicate(Name, newChildren.ToImmutableArray());
}
class Variable : Term
{
    public override int CountChildren() => 0;
    public override void GetChildren(Span<Term> childrenReceiver) { }
    public override Term SetChildren(ReadOnlySpan<Term> newChildren) => this;
}
class Atom : Term
{
    public override int CountChildren() => 0;
    public override void GetChildren(Span<Term> childrenReceiver) { }
    public override Term SetChildren(ReadOnlySpan<Term> newChildren) => this;
}
```

We can now use Sawmill's stock traversals like `SelfAndDescendants` and `Rewrite` to work with terms. For example, here's a method which finds all of the variables mentioned in a term.

```csharp
public static IEnumerable<string> Variables(this Term term)
    => term
        .SelfAndDescendants()
        .OfType<Variable>()
        .Select(v => v.Name)
        .Distinct();
```

Here's a method which writes out a term as a string.

```csharp
public static string Write(this Term term)
    => term.Fold<Term, string>((childStrings, x) =>
    {
        switch (x)
        {
            case Predicate p:
                return p.Name + "(" + string.Join(", ", childStrings.ToArray()) + ")";
            case Variable v:
                return v.Name;
            case Atom a:
                return a.Value;
            default:
                throw new Exception("unknown term");
        }
    });
```

As an exercise, you could try extending this abstract syntax (and `Write`) to support numbers.

Next time we'll write a parser!
