---
title: Type-Level Dependency Injection
date: 2025-05-21
---

Let's have fun with some of C#'s recent features.

Imagine you're working on a recipe app. Users can type in what ingredients they have to hand and the app will suggest some recipe ideas from its library.

As usual, each service in the app gets an `interface`:

```csharp
interface ILogger
{
    void Log(string message);
}
interface IRecipeRepository
{
    IEnumerable<Recipe> FindRecipesWithIngredient(string ingredientName);
}
interface IRecipeSuggester
{
    IEnumerable<Recipe> SuggestRecipes(IEnumerable<string> ingredients);
}
```

When a service needs to depend on another service (via its interface), the traditional way is to use a class constructor: each dependency becomes a constructor parameter. You can change out implementations of a service by passing different arguments, and compose multiple dependencies by using multiple parameters. While it's _possible_ to hand-write your app's composition root with `new` expressions, it can be tedious, so people use an IOC container which calls all of your services' constructors by magic.

The design I want to explore instead involves declaring dependencies by using **constraints on a type parameter**. Each service in the system is going to take a `TServices` type parameter,

```csharp
class TopTenRecipeSuggester<TServices> : IRecipeSuggester
```

and each `interface` will have a corresponding "provider".

```csharp
interface ILoggerProvider
{
    static abstract ILogger Logger { get; }
}
interface IRecipeRepositoryProvider
{
    static abstract IRecipeRepository RecipeRepository { get; }
}
interface IRecipeSuggesterProvider
{
    static abstract IRecipeSuggester RecipeSuggester { get; }
}
```

(Why make these abstract properties `static`? It'll become clear later.) I think of these provider interfaces as defining composable fragments of a _namespace_. Each provider claims one name (or more, why not) for its corresponding interface(s). Generic type constraints let us put the building blocks together.

```csharp
class TopTenRecipeSuggester<TServices> : IRecipeSuggester
    where TServices : IRecipeRepositoryProvider, ILoggerProvider
```

That's why the providers need to be interfaces — so we can use more than one in a constraint.

This is nice and composable. Each service specifies exactly the dependencies it needs, as a set of constraints on the shared `TServices` type parameter. The type system takes care of union-ing these constraints; eventually we'll specify a single concrete type to stand in for all the instances of `TServices` and we'll be required to implement exactly the providers that were requested by the services.

Now when the code needs to call a dependency, it just looks it up in the `TServices` namespace.

```csharp
class TopTenRecipeSuggester<TServices> : IRecipeSuggester
    where TServices : IRecipeRepositoryProvider, ILoggerProvider
{
	public IEnumerable<Recipe> SuggestRecipes(IEnumerable<string> ingredients)
	{
		TServices.Logger.Log("finding recipes...");

		var results = (
			from i in ingredients
			from r in TServices.RecipeRepository.FindRecipesWithIngredient(i)
			group i by r into g
			orderby g.Count()
			select g.Key
		).Take(10).ToList();

		TServices.Logger.Log("found some recipes");

		return results;
	}
}
```

The dependencies are injected through a single (type) parameter. Is this [the dreaded “Service Locator” anti-pattern](https://blog.ploeh.dk/2010/02/03/ServiceLocatorisanAnti-Pattern/)? **No!** The issue with Service Locator is that it makes dependencies invisible: you can't tell how to set up a component's dependencies without reading its source code. The constraints on the `TServices` type parameter are explicit and visible. If you want to use a service, you're forced by the type checker to satisfy its dependencies. And the service class can't access any dependencies other than the ones declared in the `where` clause.

How _do_ you satisfy a set of dependencies? At the app's [composition root](https://blog.ploeh.dk/2012/11/06/WhentouseaDIContainer/#0c19d443c1ad4fd6a4c7883dcc9abb3c), we need to provide a type that can stand in for the `TServices` type parameter. That amounts to defining a class that implements all of the `Provider` interfaces by supplying a concrete implementation of each service.

```csharp
class CompositionRoot : ILoggerProvider, IRecipeRepositoryProvider, IRecipeSuggesterProvider
{
    // CompositionRoot supplies itself as the TServices type parameter
    public static ILogger Logger { get; }
        = new ConsoleLogger<CompositionRoot>();

    public static IRecipeRepository RecipeRepository { get; }
        = new DbRecipeRepository<CompositionRoot>();

    public static IRecipeSuggester RecipeSuggester { get; }
        = new TopTenRecipeSuggester<CompositionRoot>();

	// ...
}
```

Finally, at the app's entry point, you can pick out services directly from the `CompositionRoot`. (Typically you want to keep this layer thin: just call out to a controller object.)

```csharp
// Program.cs
CompositionRoot.RecipeSuggestionController.Run();
```


## Composable Composition Root

One problem with the `CompositionRoot` is that it's difficult to swap out an implementation of a service. `CompositionRoot` passes itself as the `TServices` type parameter — not "the concrete type of the instance" — so you can't override a property by subclassing it.

```csharp
class TestCompositionRoot : CompositionRoot
{
    // This doesn't work - the TopTenRecipeSuggester is still going to
    // look in CompositionRoot to find its dependencies.
    public static override IRecipeRepository RecipeRepository { get; }
        = new MockRecipeRepository<TestCompositionRoot>();
}
```

You'd have to define an entirely separate composition root for testing instead.

>[!aside]
> I actually don't think this is too bad, or at least, it's also a weak spot for classic constructor injection. With classic constructor injection, tests typically `new` up the SUT and specify all of its dependencies by hand anyway. Type-Level DI has somewhat more boilerplate but not, like, asymptotically more.

Anyway, we can patch this up using one of my favourite cursed tricks: _F-bounds_ (aka the _curiously recurring template pattern_). `CompositionRoot` will (become an interface and) take a `TServices` type parameter itself. `TServices` stands in for the eventual concrete type of the composition root.

```csharp
interface ICompositionRoot<TServices> : ILoggerProvider, IRecipeRepositoryProvider, IRecipeSuggesterProvider
    where TServices : ICompositionRoot<TServices>
{
    static ILogger ILoggerProvider.Logger { get; }
        = new ConsoleLogger<TServices>();

    static IRecipeRepository IRecipeRepositoryProvider.RecipeRepository { get; }
        = new DbRecipeRepository<TServices>();

    static IRecipeSuggester IRecipeSuggesterProvider.RecipeSuggester { get; }
        = new TopTenRecipeSuggester<TServices>();
}
```

> [!aside]
> Now you can see why I made the providers' properties `static`. Interfaces can't have (non-static) fields, and accordingly non-static interface properties can't have initialisers because there's no corresponding storage location.

This arrangement allows you to derive another interface from `CompositionRoot`, override whatever you need, and forward the type parameter.

```csharp
interface IMockCompositionRoot<TServices> : ICompositionRoot<TServices>
    where TServices : IMockCompositionRoot<TServices>
{
    static IRecipeRepository IRecipeRepositoryProvider.RecipeRepository { get; }
        = new MockRecipeRepository<TServices>();
}
```

Eventually, when it's time to run code, someone will derive an _empty_ concrete class which plugs itself in as the `TServices` type parameter.

```csharp
class CompositionRoot : ICompositionRoot<CompositionRoot> {}
class MockCompositionRoot : IMockCompositionRoot<MockCompositionRoot> {}
```

Why stop here? Let's split up `CompositionRoot`'s fields into individual interfaces. Each concrete service implementation now has its own provider...

```csharp
interface IConsoleLoggerProvider<TServices> : ILoggerProvider
{
    static ILogger ILoggerProvider.Logger { get; }
        = new ConsoleLogger<TServices>();
}
interface IDbRecipeRepositoryProvider<TServices> : IRecipeRepositoryProvider
{
    static IRecipeRepository IRecipeRepositoryProvider.RecipeRepository { get; }
        = new DbRecipeRepository<TServices>();
}
interface ITopTenRecipeSuggesterProvider<TServices> : IRecipeSuggesterProvider
    where TServices : ILoggerProvider, IRecipeRepositoryProvider
{
    static IRecipeSuggester IRecipeSuggesterProvider.RecipeSuggester { get; }
        = new TopTenRecipeSuggester<TServices>();
}
```

... and the `CompositionRoot` just composes the concrete providers by inheritance, once again passing itself as the `TServices` type parameter. You don't need to write out the concrete service implementations any more.

```csharp
class CompositionRoot :
    IConsoleLoggerProvider<CompositionRoot>,
    IDbRecipeRepositoryProvider<CompositionRoot>,
    ITopTenRecipeSuggesterProvider<CompositionRoot>
{
}
```

Beautiful! (I rather like nesting the concrete providers inside their corresponding concrete service, so, `ConsoleLogger<TServices>.Provider` instead of `IConsoleLoggerProvider<TServices>`. You can even nest the abstract providers in the service interfaces: `ILogger.Provider`.)

You can mix and match if you like: bundle some or all of your services into a single interface, and override them one at a time with inheritance.

```csharp
interface ICompositionRoot<TServices> :
    IConsoleLoggerProvider<TServices>,
    IDbRecipeRepositoryProvider<TServices>,
    ITopTenRecipeSuggesterProvider<TServices>
    where TServices : ICompositionRoot<TServices>
{
}
class CompositionRoot : ICompositionRoot<CompositionRoot>
{
}
class MockCompositionRoot : ICompositionRoot<MockCompositionRoot>
{
    static IRecipeRepository IRecipeRepositoryProvider.RecipeRepository { get; }
        = new MockRecipeRepository<TServices>();
}
```

There's one minor inconvenience with this arrangement: at your app's entry point, it's now a little more difficult to use `CompositionRoot` directly. You have to go via a (constrained) type parameter to access the interface's static properties.

```csharp
// Program.cs
GetController<CompositionRoot>().Run();

static IRecipeSuggestionController GetController<T>()
    where T : IRecipeSuggestionControllerProvider
	=> T.RecipeSuggestionController;
```

Writing an additional provider interface for each component in your system (a concrete provider for each concrete service, and an abstract provider for each of their interfaces) is tedious boilerplate, but at least it's the kind of thing you can automate with a source generator.


## Some Thoughts on Modules

Why do we do dependency injection with constructors and interfaces? Object oriented languages expect you to do all your modelling, at every level of your system, using the tools of objects. But in practice I feel there are (at least) two separate types of objects. Honest-to-goodness _objects_ manage data, have a lifetime, may have many instances. But _services_ are really just organisational conveniences — they simply bundle up a set of functions and dependencies, they're stateless, and there's typically only one of each service in a given application.

It's weird that we model both of these concepts with classes! We want to do **modular** programming: make components loosely coupled, program to well-designed interfaces and hide implementations. But mainstream object oriented languages don't have tools for programming modules, so we're stuck reusing constructs that are better suited to managing units of data. Shut up and write classes!

I freely admit my design for TLDI is grotesque. But let's admit that IOC containers like [MEDI](https://learn.microsoft.com/en-us/dotnet/core/extensions/dependency-injection) are grotesque too. They exist to paper over the deficiency of modular programming in OO languages. It was tedious to write `new` statements to wire up concrete dependencies for every service in your system, and frameworks like ASP.NET needed a generic way to create instances of service classes, manage their lifetimes, and provide their dependencies. I don't think IOC registration code is very much more tasteful than `new` statements, but in any case we should recognise that "instances of service classes" is a thoroughly object-oriented framing of the problem: IOC containers are caused by constructor injection.

I don't quite understand why today's industrial strength languages don't have built-in facilities for managing components and dependencies. I would love to see a language (other than OCaml) take modular programming and dependency injection seriously. Maybe next time I'll jot down some ideas of how to design such a system.

