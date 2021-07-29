---
title: Rewriting IRewritable
date: 2019-10-05
---

> _I got married yesterday! This post is about my C# generic programming library [Sawmill](https://github.com/benjamin-hodgson/Sawmill). Have a read of [my earlier post](/posts/2017-11-13-recursion-without-recursion.html) for an introduction._

## How things were

I recently made a substantial change to Sawmill's core `IRewritable` interface. Here's what it used to look like:

```csharp
interface IRewritable<T> where T : IRewritable<T>
{
    Children<T> GetChildren();
    T SetChildren(Children<T> newChildren);
    T RewriteChildren(Func<T, T> transformer);
}
```

It's a pleasingly simple interface with two core operations --- `GetChildren` returns the current object's immediate children and `SetChildren` replaces them. But there are also some warts.

### Wart 1: `Children<T>`

`Children<T>` is a custom struct, not a standard collection type:

```csharp
struct Children<T>
{
    public NumberOfChildren NumberOfChildren { get; }
    public T First { get; }
    public T Second { get; }
    public ImmutableList<T> Many { get; }
}
enum NumberOfChildren
{
    None, One, Two, Many
}
```

I wanted to avoid boxing when the object has a small number of children (which is fairly common in practice). So `GetChildren` returns a `Children<T>`, passing up to two children on the stack and the rest in an `ImmutableList`. The `NumberOfChildren` property tells the library how many of the struct's fields are filled in.

This custom collection type is an extra hurdle to understanding `IRewritable`'s API. It also makes certain parts of Sawmill's implementation more complex --- many internal methods have to `switch` on the `NumberOfChildren` they're working with and do the same work in four different ways. It's also relatively large for a `struct` (at least 16 bytes and probably more, depending on your processor architecture) so there is a marginal performance cost associated with copying it around.

### Wart 2: Collections

In fact, having `GetChildren` return a collection at all is problematic, because it forces implementations to allocate memory. If your object has three or more children (and they're not already stored in an `ImmutableList`) then you have to create a new `ImmutableList` whenever `GetChildren` is called:

```csharp
class ThreeChildren : IRewritable<ThreeChildren>
{
    private readonly ThreeChildren _child1;
    private readonly ThreeChildren _child2;
    private readonly ThreeChildren _child3;

    public Children<ThreeChildren> GetChildren()
        => new Children<ThreeChildren>(
            Children.Many,
            ImmutableList.Create(_child1, _child2, _child3)
        );
    public ThreeChildren SetChildren(Children<ThreeChildren> newChildren)
        => new ThreeChildren(
            newChildren.Many[0],
            newChildren.Many[1],
            newChildren.Many[2]
        );
}
```

When Sawmill traverses a tree it typically calls `GetChildren()` for every node in the tree. That's a lot of throwaway `ImmutableList`s!

One way to avoid creating all this garbage might be to redesign `IRewritable` to be buffer-oriented.

```csharp
interface IRewritable<T> where T : IRewritable<T>
{
    int CountChildren();
    void GetChildren(T[] buffer);
    T SetChildren(T[] newChildren);
}
```

With this design, Sawmill passes an array into `GetChildren` (after calling `CountChildren` to find out how big the array needs to be) and asks the object to copy its children into the array. Implementations of `IRewritable` no longer have to allocate memory for their return value. The memory is allocated (and hopefully reused) by the library. But this API is less safe --- if an `IRewritable` implementation stores a reference to the buffer then it could get unexpectedly mutated.

```csharp
class Bad : IRewritable<Bad>
{
    private readonly Bad[] _children;

    public int CountChildren() => _children.Length;
    public void GetChildren(Bad[] buffer) => _children.CopyTo(buffer);
    public Bad SetChildren(Bad[] newChildren) => new Bad(newChildren);  // BUG
}
```

This is not a "pit of success" API --- `SetChildren` looks sensible but could go wrong if anyone else has a reference to the array. Sawmill would have to allocate a new array for each `GetChildren`/`SetChildren` call in order to be safe. So this design would end up allocating just as much as the `ImmutableList` version.

(Keep the buffer idea in your head, though, because we'll be coming back to it in a minute.)


### Wart 3: `RewriteChildren`

`RewriteChildren`, which applies a transformation function to the object's immediate children (in other words, a one-level `Rewrite`), has a sensible implementation in terms of the other two methods: get the children, transform them, and put them back.

```csharp
public static T DefaultRewriteChildren<T>(this T value, Func<T, T> transformer)
    where T : IRewritable<T>
{
    var children = value.GetChildren();
    var newChildren = children.Select(transformer);
    return value.SetChildren(newChildren);
}
```

([The real `DefaultRewriteChildren`](https://github.com/benjamin-hodgson/Sawmill/blob/87aea1e5757360b99457c8e2e6a7993fc2176f23/Sawmill/Rewriter.DefaultRewriteChildren.cs) was a bit more complex than this, because it tried to avoid calling `SetChildren` if `transformer` didn't actually change anything.)

In fact, I expect that most `IRewritable`s would just delegate `RewriteChildren` to `DefaultRewriteChildren`. So why did I put `RewriteChildren` on the interface? It's because of Wart 2 --- `GetChildren` can be expensive because it might have to create an `ImmutableList`. (And applying `transformer` to that `ImmutableList` using eg [`ConvertAll`](https://docs.microsoft.com/en-us/dotnet/api/system.collections.immutable.immutablelist-1.convertall?view=netcore-3.0) would have to create another one.) So `DefaultRewriteChildren` is slow; the idea of `RewriteChildren` was for objects to transform their children directly, without going via an `ImmutableList`.

```csharp
class ThreeChildren : IRewritable<ThreeChildren>
{
    // ...
    public ThreeChildren RewriteChildren(Func<ThreeChildren, ThreeChildren> f)
        => new ThreeChildren(f(_child1), f(_child2), f(_child3));  // no ImmutableList
}
```

The extra method makes `IRewritable` harder to understand and harder to implement. Implementing `RewriteChildren` (without using `DefaultRewriteChildren`) can be tricky; if the transformer function doesn't change anything you should avoid recreating the object.


## The new way

I'm pleased to say that `IRewritable`'s new design addresses all three of these warts. Remember the "buffer" idea from earlier?

```csharp
interface IRewritable<T> where T : IRewritable<T>
{
    int CountChildren();
    void GetChildren(Span<T> buffer);
    T SetChildren(ReadOnlySpan<T> newChildren);
}
```

This design is almost identical to the one I outlined earlier --- it uses [`Span`](https://docs.microsoft.com/en-us/dotnet/api/system.span-1?view=netcore-3.0)s instead of arrays. (For the uninitiated, a `Span` is basically a "slice" of an array. It can also be backed by _unmanaged_ memory, though, making it more flexible than `ArraySegment`.)

* `CountChildren` tells Sawmill how much space is needed to copy the children.
* `GetChildren` copies the current object's immediate children into `buffer`.
* `SetChildren` creates a copy of the current object with its immediate children replaced.

Here's the important difference. Unlike an array, a `Span` _can't be stored on the heap_. `Span` is defined using [the `ref` keyword](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/ref?view=netcore-3.0#ref-struct-types), which tells the compiler to check that `Span`s are confined to the stack. If you pass a `Span` into a method as an argument, you can be confident that the `Span` won't leave the scope of that method's stack frame (just like `ref`).

So `Span` thoroughly solves the safety issue with the array-oriented API. I don't have to worry about mutating a `Span` which got stored inside a client object, because the `Span` can't be stored! This allows Sawmill to safely reuse the memory behind a `Span`, rather than allocating a new array for each `GetChildren`/`SetChildren` call.

So Sawmill's methods like `RewriteChildren` can be implemented without allocating memory. In this example I'm using [`ArrayPool`](https://docs.microsoft.com/en-us/dotnet/api/system.buffers.arraypool-1?view=netcore-3.0) to avoid creating a new array for every `RewriteChildren` call.

```csharp
public static T RewriteChildren<T>(this T value, Func<T, T> transformer)
    where T : IRewritable<T>
{
    var count = value.CountChildren();
    var array = ArrayPool<T>.Shared.Rent(count);
    // ArrayPool can return arrays bigger than you asked for
    var span = array.AsSpan().Slice(count);

    value.GetChildren(span);
    
    for (var i = 0; i < span.Length; i++)
    {
        span[i] = transformer(span[i]);
    }

    var result = value.SetChildren(newChildren);
    ArrayPool<T>.Shared.Return(array);
    return result;
}
```

The main reason for having `RewriteChildren` be a method on the interface was to avoid allocating memory (for the `GetChildren` calls). So we don't need it on the interface any more --- this extension method serves as a single universal implementation. Likewise, `Children<T>`'s purpose was also to avoid allocating memory, so we can do away with it too.


## Relieving `ArrayPool` pressure

There's one operational problem with this implementation: it can end up renting a large number of arrays from the pool. `Rewrite`, which applies a transformation function to every node in a tree (not just one layer), is implemented something like this:

```csharp
public static T Rewrite(this T value, Func<T, T> transformer) where T : IRewritable<T>
    => transformer(t.RewriteChildren(child => child.Rewrite(transformer)));
```

The lambda which is passed to `RewriteChildren` contains a recursive call to `Rewrite`. Let's think through the operational behaviour of `Rewrite`:

1. `Rewrite` calls `RewriteChildren`
2. `RewriteChildren` rents an array from the array pool and calls `GetChildren`
3. `RewriteChildren` calls the `child => child.Rewrite(transformer)` lambda function for each child
4. The lambda function recursively calls `Rewrite`; steps 1-3 are repeated until you encounter a node with no children
5. `RewriteChildren` calls `SetChildren` and returns its array to the array pool
6. `RewriteChildren` returns and step 5 is repeated as you return up the call stack

Since steps 1-3 are repeated before step 5 happens, you can end up renting many arrays (a number equal to the height of the tree) before returning any of them to the pool. So the array pool could run out of arrays!

To fix this problem, we want to rent a small number of large arrays from the array pool, rather than a large number of small ones. We can lean on the fact that each array only lives as long as a single method --- the array is `Rent`ed at the start of `RewriteChildren` and then `Return`ed at the end. The memory usage is stack-shaped.

So here's the plan. We're going to rent a large array from the pool at `Rewrite`'s beginning, and `RewriteChildren` will take a chunk from that array each time it's called. Each chunk will be freed up before any previously-allocated chunks are freed.

```csharp
public static T Rewrite<T>(this T value, Func<T, T> transformer)
    where T : IRewritable<T>
{
    using (var chunks = new ChunkStack<T>())
    {
        T Go(T x)
            => transformer(t.RewriteChildrenInternal(Go, chunks));
        return Go(value);
    }
}

public static T RewriteChildren<T>(this T value, Func<T, T> transformer)
    where T : IRewritable<T>
{
    using (var chunks = new ChunkStack<T>())
    {
        return RewriteChildrenInternal(t, transformer, chunks);
    }
}

private static T RewriteChildrenInternal<T>(
    this T value,
    Func<T, T> transformer,
    ChunkStack<T> chunks
) where T : IRewritable<T>
{
    var count = value.CountChildren();
    var span = chunks.Allocate(count);

    value.GetChildren(span);
    
    for (var i = 0; i < span.Length; i++)
    {
        span[i] = transformer(span[i]);
    }

    var result = value.SetChildren(newChildren);
    chunks.Free(span);
    return result;
}
```

`ChunkStack` contains an array and a count of how much of that array is in use. `Allocate` and `Free` increase and decrease that count.

```csharp
class ChunkStack<T> : IDisposable
{
    private T[] _array = ArrayPool<T>.Shared.Rent(512);
    private int _used = 0;

    public Span<T> Allocate(int count)
    {
        var span = _array.Slice(_used, count);
        _used += count;
        return span;
    }
    public void Free(Span<T> span)
    {
        _used -= span.Length;
    }
    public void Dispose()
    {
        if (_array != null)
        {
            ArrayPool<T>.Shared.Return(_array);
            _array = null;
        }
    }
}
```

I've glossed over an important part of `ChunkStack`'s implementation: what happens when the array fills up? The [real implementation](https://github.com/benjamin-hodgson/Sawmill/blob/bf652359023a76d3ea395d7d743db1a8d6559ec2/Sawmill/ChunkStack.cs) manages a collection of "regions", taking a new region from the array pool when existing regions fill up.

In the pathological case of a _very_ large tree, this version of `Rewrite` can still exhaust the array pool, but it'll happen much less quickly.


## Hackalloc

I mentioned earlier that a `Span` is not necessarily backed by an array. `Span` represents _a contiguous block of memory_, with no assumptions about where that memory is or how it's managed. A `Span` could be a slice of an array, or it could be a chunk of unmanaged memory, or it could be an area of the stack. Under the hood it's _just a pointer_; `Span` doesn't care exactly where the pointer points.

In fact, C# has built in support for that last case. The `stackalloc` keyword works like C's `alloca`: it carves out a chunk of memory directly in the current stack frame, which becomes invalid when the current method returns. Until recently, `stackalloc` was only available in an `unsafe` context, but today it's available in safe code thanks to `Span`.

```csharp
public void StackallocExample()
{
    // allocate space for three ints in the current stack frame
    Span<int> myInts = stackalloc int[3];
    myInts[0] = 123;
    myInts[1] = 456;
    myInts[2] = myInts[0] + myInts[1];
    Console.WriteLine(myInts[2]);  // prints 579
}
```

I want to use `stackalloc` to avoid taking memory from the `ChunkStack` when an object has a small number (say, 4) of children. This is fairly common in practice. Stack memory tends to be marginally faster than heap memory because it's more likely to be in the processor cache, so this may have a modest performance benefit as well as relieving pressure on the array pool. If there are more than 4 children we can just fall back on the `ChunkStack`.

Here's the new `RewriteChildrenInternal`:

```csharp
private static T RewriteChildrenInternal<T>(
    this T value,
    Func<T, T> transformer,
    ChunkStack<T> chunks
) where T : IRewritable<T>
{
    var count = value.CountChildren();

    Span<T> span = stackalloc T[4];
    if (count > 4)
    {
        span = chunks.Allocate(count);
    }

    value.GetChildren(span);
    
    for (var i = 0; i < span.Length; i++)
    {
        span[i] = transformer(span[i]);
    }

    var result = value.SetChildren(newChildren);

    if (count > 4)
    {
        chunks.Free(span);
    }
    return result;
}
```

Sadly this doesn't work. The compiler complains about the `stackalloc T` line: "Cannot take the address of, get the size of, or declare a pointer to a managed type ('T')". Basically, the CLR doesn't support `stackalloc` with reference types --- you can only use `stackalloc` with primitives or structs containing primitives. (A type parameter `T` _might_ be a reference type, so you still can't use it with `stackalloc`.) Under the hood, `stackalloc` is untyped; the garbage collector doesn't know how to follow pointers that are stored in `stackalloc`ed memory because it doesn't even know there are pointers there.

I still think the idea's a good one, though. Can we unsafely hack it up?

I'm going to use the following `struct` as a "poor man's `stackalloc[4]`":

```csharp
struct Four<T>
{
    public T First;
    public T Second;
    public T Third;
    public T Fourth;
}
```

A variable of type `Four<T>` has enough room for four `T`s --- so when the variable is a local variable (in an ordinary method) it's functionally equivalent to a `stackalloc T[4]`. We won't be using the `First`, `Second`, `Third` and `Fourth` properties directly --- we'll be (unsafely) addressing them relative to the start of the struct. In this example I'm using [`System.Runtime.CompilerServices.Unsafe`](https://docs.microsoft.com/en-us/dotnet/api/system.runtime.compilerservices.unsafe?view=netcore-3.0) to address `Third` by looking 2 elements beyond `First`:

```csharp
var four = new Four<T>();
ref T third = ref Unsafe.Add(ref four.First, 2);
Assert.True(Unsafe.AreSame(ref four.Third, ref third));
```

The plan is to create a `Span` whose pointer refers to the start of a `Four<T>` on the stack. `span[0]` will address `four.First`, `span[1]` will address `Second`, and so on. My first idea to implement this was to use `System.Runtime.CompilerServices.Unsafe` to coerce a `ref Four<T>` to an unmanaged pointer, and then put that in a `Span`:

```csharp
unsafe
{
    var four = new Four<T>();
    void* ptr = Unsafe.AsPointer(ref four);
    var span = new Span<T>(ptr, 4);
}
```

Sadly the `Span` constructor throws an exception when `T` is a reference type. At this point I went for a poke around in the .NET source code. I wanted to know how `array.AsSpan()` works. I found [an internal constructor](https://github.com/dotnet/corefx/blob/7e9a177824cbefaee8985a9b517ebb0ea2e17a81/src/Common/src/CoreLib/System/Span.Fast.cs#L123) which takes a `ref T`. We can illictly call that constructor using reflection, although of course we want to avoid the performance costs of reflection. So the actual plan is to use runtime code generation to call the internal `Span` constructor. Ordinarily I'd use `Expression` to do this runtime code generation, but `Expression` doesn't support `ref` parameters, so we have to write the IL by hand.

```csharp
private static class SpanFactory<T>
{
    private delegate Span<T> SpanCtor(ref T value, int length);
    private static readonly SpanCtor _spanCtor;

    static SpanFactory()
    {
        var ctor = typeof(Span<T>)
            .GetConstructors(BindingFlags.NonPublic | BindingFlags.Instance)
            .Single(c =>
                c.GetParameters().Length == 2
                && c.GetParameters()[0].ParameterType.IsByRef
            );

        var method = new DynamicMethod(
            "",
            typeof(Span<T>),
            new[] { typeof(T).MakeByRefType(), typeof(int) }
        );

        var il = method.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Newobj, ctor);
        il.Emit(OpCodes.Ret);

        _spanCtor = (SpanCtor)method.CreateDelegate(typeof(SpanCtor));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Span<T> Create(ref T value, int length)
        => _spanCtor(ref value, length);
}
```

Obviously relying on BCL internals like this is risky. The internal constructor could be removed, or changed to work differently, in which case my code could stop working or even segfault. That said, I think the likelihood of the internal constructor changing is quite low in this case.

> **Update**: On .NET Core, there is an officially-supported API to do this: [`MemoryMarshal.CreateSpan`](https://docs.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.memorymarshal.createspan?view=netcore-3.0). I didn't know it existed at the time that I wrote this.

There are also risks associated with mixing pointers and references like this. You have to be very careful that the `Span` doesn't live longer than the `Four` it points to. That means the `Four` has to be discarded at the end of the method along with the `Span`, and it has to be stored in a "real" local variable, not in temporary storage on the evaluation stack. I'll address that by mentioning the variable (as a parameter to a non-inlined "keep-alive" method) at the end of the method.

You also need to be certain that the `Four` is stored on the stack and not the heap. Data stored on the heap is liable to get moved by the garbage collector, which would invalidate the pointer inside the `Span`. Beware that local variables are not always safe from being moved! Methods containing `await`s, `yield`s, and lambdas are liable to store their local variables on the heap, so if `RewriteChildrenInternal` were not an ordinary method this hack would not be safe.

Here's the final implementation of `RewriteChildrenInternal`. `var four = new Four<T>();` allocates space for four `T`s in `RewriteChildrenInternal`'s stack frame. Then, when I call `GetSpan`, I'm passing in the address of the start of that `Four` using the `ref` keyword. `GetSpan` returns a `Span` which either points at the start of `four` or at a chunk taken from the `ChunkStack`, depending on how many children we need to store. `ReleaseSpan` returns the `Span` to the `ChunkStack` if it came from there, and the `KeepAlive` call ensures the `four` isn't deallocated too early.

```csharp
private static T RewriteChildrenInternal<T>(
    this T value,
    Func<T, T> transformer,
    ChunkStack<T> chunks
) where T : IRewritable<T>
{
    var count = value.CountChildren();

    var four = new Four<T>();
    var span = GetSpan(count, chunks, ref four);

    value.GetChildren(span);
    
    for (var i = 0; i < span.Length; i++)
    {
        span[i] = transformer(span[i]);
    }

    var result = value.SetChildren(newChildren);

    ReleaseSpan(span, chunks);
    KeepAlive(ref four);
    return result;
}
private static Span<T> GetSpan<T>(int count, ChunkStack<T> chunks, ref Four<T> four)
{
    if (count == 0)
    {
        return new Span<T>();
    }
    else if (count <= 4)
    {
        return SpanFactory<T>.Create(ref four.First, count);
    }
    else
    {
        return chunks.Allocate(count);
    }
}
private static void ReleaseSpan<T>(Span<T> span, ChunkStack<T> chunks)
{
    if (span.Length > 4)
    {
        chunks.Free(span);
    }
}
[MethodImpl(MethodImplOptions.NoInlining)]
private static void KeepAlive<T>(ref Four<T> four)
{
}
```

As far as I know, the designers of `Span` were thinking primarily about applications such as serialisation and parsing --- the sort of low-level code you'd find in a [high performance web server](https://github.com/aspnet/AspNetCore). But `Span` also really shines in this high-level library of recursion patterns. Its guarantees about storage proved crucial to the safety of my `IRewritable` abstraction, but I'm also leaning on its flexibility to implement that abstraction as efficiently as possible.

Sawmill version 3.0 is now available [on Nuget](https://www.nuget.org/packages/Sawmill), and you can read all of this code in [the GitHub repo](https://github.com/benjamin-hodgson/Sawmill).
