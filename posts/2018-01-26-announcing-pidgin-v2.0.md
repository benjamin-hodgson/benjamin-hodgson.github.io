---
title: Announcing Pidgin v2.0
subtitle: Errors, `Span`, and performance
---

I'm pleased to announce that version 2.0 of my functional parsing library, [Pidgin](https://github.com/benjamin-hodgson/Pidgin), is now available [on Nuget](https://www.nuget.org/packages/Pidgin). In this release I've focused on error messages, performance, and `Span` support.


Parser Combinators
------------------

I haven't written about Pidgin before, so allow me to briefly introduce it. Pidgin is a [_parser combinator_](https://en.wikipedia.org/wiki/Parser_combinator) library, meaning that it consists of three main concepts:

* A type `Parser` which models a _parsing process_
* A collection of _primitive_ `Parser` objects which perform some simple individual parsing task
* A collection of _combinator_ functions which can build complex `Parser`s out of simpler ones

Taken together, we have an object model allowing you to write code resembling a high-level description of a parsing process. As a brief taste, here is a simple example parser which parses an identifier in a typical programming language.

```csharp
Parser<char, string> Identifier =
    Letter.Then(
        LetterOrDigit.ManyString(),
        (first, rest) => first + rest
    );

Assert.Equal("abc1", Identifier.ParseOrThrow("abc1"));
```

* `Identifier` is a `Parser<char, string>`, meaning it's a process which consumes a sequence of `char`s and produces a `string`.
* `Letter` is a primitive parser which consumes and returns a single character from the input stream, moving it on to the next character. (If the character is not a letter, the parser fails and doesn't change the state of the input stream.)
* `LetterOrDigit` is like `Letter` but for alphanumeric characters.
* `ManyString` is a combinator method which runs a parser in a loop until it fails. It takes all of the smaller parser's results from the loop and packs them into a `string`. So `LetterOrDigit.ManyString()` is a parser which consumes and returns a sequence of alphanumeric characters.
* `Then` is another combinator which runs two parsers in sequence and applies a function to the result. So, reading the parser as a whole, we can see that an `Identifier` consists of a single letter followed by a sequence of letters or digits.

Parser combinators' power comes from their composability. The library comprises a small number of building blocks, which you can put together in rich and varied ways to build a parser which does what you need. The library's level of abstraction is a good fit for small-to-medium sized parsing tasks: it's not as high-level as a full-blown parser generator like Antlr, but it's much simpler to integrate. Rewriting our hand-written [JQL](https://www.benjamin.pizza/posts/2017-11-13-recursion-without-recursion.html) parser in Pidgin took around four times less code.

That's the overview --- I won't dive into a full tutorial on parser combinators because there are already [plenty](http://www.lihaoyi.com/post/EasyParsingwithParserCombinators.html) [of](http://webdoc.sub.gwdg.de/ebook/serien/ah/UU-CS/2008-044.pdf) [those](https://news.ycombinator.com/item?id=14600079) a mere Google away.

### How it works

Pidgin's `Parser<TToken, T>` type represents a process which pulls `TToken`s one at a time from a (stateful) input stream, and either successfully returns a `T` or fails with an error message.

```csharp
public abstract class Parser<TToken, T>
{
    internal abstract Result<T> Parse(IParseState<TToken> input);
}
internal interface IParseState<TToken>
{
    TToken? Peek();  // null if at end of file
    void Advance();

    // ... plus a few methods to facilitate backtracking etc
}
internal struct Result<T>
{
    // if Success is true, Value contains the parsed value and Error is null
    // if Success is false, Value is null and Error contains error info
    public bool Success { get; }
    public T Value { get; }
    public ParseError Error { get; }
}
```

`IParseState` represents an iterator over the parser's input (a sequence of `TToken`s). `Peek` returns the token at the current location in the input (or `null` if the parser has reached the end of the input) and `Advance` moves the stream on to the next token.

Primitive parsers typically manipulate the `IParseState` directly. This parser matches a specific string:

```csharp
class StringParser : Parser<char, string>
{
    private readonly string _expected;

    internal override Result<string> Parse(IParseState<char> input)
    {
        foreach (var c in _expected)
        {
            var token = input.Peek();
            if (!token.HasValue)
            {
                return Result.Error(ParseError.EndOfFile);
            }
            if (token.Value != c)
            {
                return Result.Error(ParseError.UnexpectedToken);
            }
            input.Advance();
        }
        return Result.Success(_expected);
    }
}
```

Higher-order parsers typically compose one or more smaller parsers, delegating to their `Parse` methods in some useful way. `Then` returns a parser which sequences two parsers and applies a mapping function to their results:

```csharp
class ThenParser<TToken, T, U, R> : Parser<TToken, R>
{
    private readonly Parser<TToken, T> _first;
    private readonly Parser<TToken, U> _second;
    private readonly Func<T, U, R> _resultSelector;

    internal override Result<R> Parse(IParseState<TToken> input)
    {
        var result1 = _first.Parse(input);
        if (!result1.Success)
        {
            return Result.Error(result1.Error);
        }
        var result2 = _second.Parse(input);
        if (!result2.Success)
        {
            return Result.Error(result2.Error);
        }
        return Result.Success(_resultSelector(result1.Value, result2.Value));
    }
}
```


Error messages
--------------

Part of Pidgin's job is to report when you gave a parser bad input. For example, the top level of a C# file must contain `class`es and `namespace`s, so feeding a C# parser the text `Console.WriteLine("foo");` (without an enclosing `class`) should fail with some information about the problem:

```
Parse error.
    unexpected 'C'
    expected "class" or "namespace"
    at line 1, col 1
```

A mistake I made early on in Pidgin's development nearly two years ago(!) was trying to pre-compute the content of these error messages. Under the assumption that parsers are typically built once and then run repeatedly, I wrote some code to examine your parser upon construction and try to predict the ways it could fail on unexpected input, in order to avoid constructing error messages at runtime. This code calculated a set of "expected" input strings, accounting for `Then`s and `Or`s by concatenating the strings in the set and by unioning the sets respectively. (The idea was that a parser like `Keyword("public").Optional().Then(Keyword("class"))` would report that it expected `"class" or "public class"`.)

This went catastrophically wrong for complex parsers. Here's a sketch of some code which parses left-associative mathematical operators with precedence, so `3^2 + 4 * 3^5 * 5` is parsed as `(3^2) + ((4 * (3^5)) * 5)`:

```csharp
var topPrecedence = Number.Then(Char('^').Then(Number).Many());
var midPrecedence = topPrecedence.Then(Char('*').Then(topPrecedence).Many());
var lowPrecedence = midPrecedence.SeparatedBy(Char('+').Then(midPrecedence).Many());
```

`topPrecedence` expects a number, or a number followed by a sequence of exponents --- two possibilities. `midPrecedence` therefore expects a number, or a number followed by a sequence of exponents, or a number followed by a sequence of multiplications, or a number followed by a sequence of exponents followed by a sequence of multiplications --- four possibilities. `lowPrecedence` has eight possible expected inputs. The set of expected inputs blows up exponentially! Calculating the error messages in advance means you need to explore all of the (exponentially large number of) expected inputs in advance, which led to cosmologically long parser build times. And in any case, reporting a huge number of expected inputs does not make for a very good error message.

I made some attempts to optimise this by using more efficient data structures, but I realised this was a losing proposition and decided to throw it out altogether in v2.0. Error messages are now computed at runtime, when the error actually occurs. This means I can be more precise about what the parser was expecting at that particular point in the input: once a parsing process has committed to a branch I can report expected inputs _from that branch_, rather than reporting all possible expected inputs.

Implementing this efficiently was a challenge. Parse errors actually occur quite frequently in a parser combinator library, even in the happy path, because of the way the _prioritised choice_ operator `Or` works --- `String("foo").Or(String("bar"))` only tries `bar` if an attempt to parse `foo` failed. So I tried to implement this change without allocating heap memory every time a parser fails. When a parser fails, it saves its expected inputs in a stack implemented on top of pooled memory (using `ArrayPool`), which are then popped if the error gets discarded. (The way that certain parsers manipulate their children's error messages adds some interesting complications here, which I've described in a [long comment](https://github.com/benjamin-hodgson/Pidgin/blob/60c7734393719d11714158b201c99976ec48ffb9/Pidgin/ParseState.Error.cs#L36-L74).)


`Span`
------

> **Note**: You can watch [my esteemed colleague Gervasio](https://twitter.com/g3rv4) and me carrying out the work I describe here in [a live-stream on YouTube](https://www.youtube.com/watch?v=O23OLkQtiS4). It was pretty fun!

I wanted to add support for parsing input stored in a `Span`. (`Span` is a new part of the BCL representing a reference to a contiguous block of memory such as a chunk of an array.) I already had functions which applied a `Parser` to a `string`, a `T[]`, a `Stream`, etc; I'd abstracted over these various input types using the aforementioned `IParseState` interface.

```csharp
internal interface IParseState<TToken>
{
    TToken? Peek();
    void Advance();
}
```

So adding `Span` support basically means implementing `IParseState` on top of a `Span`. In theory this should be quite straightforward --- a class which has a `Span` and keeps track of its current position:

```csharp
class SpanTokenStream<TToken> : ITokenStream<TToken>
{
    private readonly Span<TToken> _span;
    private int _current;

    TToken? Peek() => _current >= _span.Length ? null : _span[_current];
    
    void Advance()
    {
        _current++;
    }
}
```

But if you try and write this class, you'll find that the compiler turns you away. `Span` is a `ref struct`, which means that it can only be stored on the stack. You can't use a `Span` as a field of a `class`, or put it in an array, or box it (by upcasting it to an `interface` or `object`), or use it as a local variable in an `async` or `yield` method (because behind the scenes such methods copy their stack frame to the heap). (There are good reasons for this restriction, pertaining to memory safety.)

How to implement `IParseState` without storing a `Span` on the heap? Because `IParseState` is an internal interface, I can make certain guarantees about its usage. Instances of `IParseState` have a limited life-span --- each `IParseState` instance becomes garbage before the call to `Parse` returns, and `IParseState` instances are never accessed from multiple threads. So I can store the `Span` in `Parse`'s stack frame and put a pointer to that stack frame in the `IParseState`.

This idea is complicated by the fact that `Span` is a [managed type](https://stackoverflow.com/questions/42154908), so you're not allowed to declare a `Span<T>*`. (This restriction ensures that a managed object can't accidentally become garbage while it's still being referred to by an unmanaged pointer; this needn't trouble us here as the `Span` is guaranteed to be reachable because it's on the stack.)

Fortunately, [the `System.Runtime.CompilerServices.Unsafe` package](https://www.nuget.org/packages/System.Runtime.CompilerServices.Unsafe/) contains some dangerous tools to get around this memory safety restriction: [`Unsafe.AsPointer`](https://docs.microsoft.com/en-us/dotnet/api/system.runtime.compilerservices.unsafe.aspointer) coerces a `ref` to an untyped `void*` and [`Unsafe.AsRef`](https://docs.microsoft.com/en-us/dotnet/api/system.runtime.compilerservices.unsafe.asref) coerces it back. So my plan was to store a `ref Span<T>` as a `void*` on the heap, and coerce it back to a `ref Span<T>` using  when I need to address it as a `Span`. (This is a similar hack to one that I used in [my HTML generation library](/posts/2018-03-16-eighty.html).)

Unfortunately this doesn't work either, because `Unsafe.AsPointer` is generic and you can't use a `ref struct` as a type parameter! The compiler can't be sure that `Unsafe.AsPointer` doesn't box its argument, which of course is forbidden when the argument is a `Span`.

I wound up implementing a type-specialised copy of those two `Unsafe` methods:

```csharp
static class Unsafe
{
    public static void* AsPointer<T>(ref Span<T> span);
    public static ref Span<T> AsRef(void* ptr);
}
```

`AsPointer` and `AsRef` are not expressible in C# (even `unsafe` C#); `System.Runtime.CompilerServices.Unsafe` is implemented in raw IL. My copies of those methods have stubs in C# which are filled in by rewriting the DLL's IL in a post-compile step.


Assorted Performance Improvements
---------------------------------

Performance has always been one of Pidgin's priorities --- I'm proud that Pidgin is C#'s fastest parser combinator library (that I know of!) --- but there's always room for improvement. In my tests Pidgin still runs somewhat slower than F#'s FParsec library, for example. In this release I made some architectural changes to `IParseState` to help close that gap.

### Buffering Uniformly

As I mentioned earlier, Pidgin has several internal implementations of `IParseState`, each of which implements a streaming abstraction on top of a different type of input. Parsers may need to _backtrack_ on failure (using the `Try` combinator), so you can't always discard a token as soon as you've seen it. Some `IParseState` implementations --- specifically the ones that are built on top of streaming storage (like `Stream`) --- therefore _buffer_ their input into an array. The ones that are backed by in-memory storage like `string` don't need to buffer because their data is already in memory.

I decided to move the buffering logic to a shared part of the code. Now _all_ `IParseState` implementations buffer their input, even the in-memory ones. On its own this should make the code slower (it's more expensive to copy a `string` into an array than not to!), but it enables all of the optimisations I'm about to describe.


### De-Virtualisation & Inlining

Virtual method calls are comparatively expensive in .NET, and they happen frequently in Pidgin's implementation because of `IParseState`'s design: a parser which consumes _n_ tokens makes a minimum of _n_ virtual calls to `Advance`.

With the buffering code extracted into a single place, there was no longer any need to keep it behind an interface. I moved all the buffering and error handling code into a mutable struct which is stored on the stack and passed to the `Parser`s by reference. Now `Advance` and `Peek` are non-virtual.

```csharp
struct ParseState<TToken>
{
    private TToken[] _buffer;
    private int _currentPosition;
    private ITokenStream<TToken> _stream;

    public TToken? Peek() =>
        _currentPosition < 0 || _currentPosition >= _buffer.Length
            ? null
            : _buffer[_currentPosition];
    
    public void Advance()
    {
        _currentPosition++;

        if (_currentPosition >= _buffer.Length)
        {
            ReadStreamIntoBuffer();
        }
    }
}

public abstract class Parser<TToken, T>
{
    internal abstract Result<T> Parse(ref ParseState<TToken> state);
}
```

That's a fairly general C# performance trick: put shared state in a mutable struct, store it on the stack, and pass it around as a `ref` parameter. This both reduces GC pressure and improves the CPU cache's hit rate, because the execution stack is likely to be in cache.

I also noticed that `Peek` was not _inlining_ well. Inlining is an important optimisation carried out by the JIT compiler: if you call a method that's short and simple enough, the compiler will just copy the method's body into the current method instead of emitting code to call it. This can often enable further optimisations such as erasing array bounds checks.

I split `Peek` up into a pair of properties:

```csharp
struct ParseState<TToken>
{
    private TToken[] _buffer;
    private int _currentPosition;

    public bool HasCurrent => _currentPos >= 0 && _currentPos < _buffer.Length;
    public TToken Current => _buffer[_currentPosition];
}
```

These properties are short and non-virtual, so they are good inlining candidates. After inlining them, the JIT sees simple array-manipulating code, which it can easily optimise in the context of the containing method. This simple change resulted in a 15-20% performance improvement across the board.


### Chunking

The `Stream` version of `Advance` called `stream.Read()`, which reads and returns a single byte from the stream. This is not as efficient as `stream.Read(byte[])`, which reads a chunk of bytes from the stream and copies them into an array. (The former requires _n_ virtual method calls to read _n_ bytes, whereas the latter requires only _n / `array.Length`_.)

I replaced `ITokenStream`'s `Advance` and `Peek` methods with a method which reads a chunk of tokens into the `ParseState`'s buffer.

```csharp
interface ITokenStream<TToken>
{
    void ReadInto(TToken[] buffer, int start, int count);
}
```

Why not design this signature as `void ReadInto(Span<TToken> span)`? I'd love to! Sadly that would preclude some implementations of `ITokenStream`. Methods like `stream.Read(Span<byte> span)` are available only on .NET Core, and as far as I'm aware Microsoft has no plans to backport them, so for compatibility with the desktop framework I'm stuck using arrays. This is irksome, as [I've said before](/posts/2018-12-06-zooming-in-on-field-accessors.html) --- what's the point of designing a feature for library authors if you're not going to support it properly?

`ReadInto` allows the `ParseState` to fill its buffer in chunks. But `ParseState`'s interface can also be chunk-ified --- some `Parser`s (like `String`) can predict how many characters they'll pull from the input. I added a `Peek` method to `ParseState` which returns a view into the `ParseState`'s buffer. (This makes `Peek` a little tricky to use correctly --- you have to be careful not to continue using the `Span` after a call to `Advance`, which may mutate the buffer.)

```csharp
public ReadOnlySpan<TToken> Peek(int count);
```

Using `Peek`, a parser like `String("foo")` can now look at three characters from the input to see if they match `foo`, rather than one at a time.

```csharp
class StringParser : Parser<char, string>
{
    private readonly string _expected;
    internal override Result<string> Parse(ref ParseState<char> state)
    {
        var span = state.Peek(_expected.Length);
        if (!_expected.AsSpan().SequenceEqual(span))
        {
            return Result.Failure();
        }
        return Result.Success(_expected);
    }
}
```

This runs about 20% faster for long strings.


Make Your Own Opportunities
---------------------------

Indulge me for a moment while I dispense some unsolicited career advice. For a long time, performance engineering was something that [other](https://twitter.com/marcgravell/), [cleverer](https://twitter.com/davidfowl) [people](https://mattwarren.org/) did while I observed from a distance with awe. Why is ASP.NET fast? Because the guys that wrote it are Actual Wizards.

I've been lucky enough to work with some of those clever people for a few years now, and actually they're not Actual Wizards. They just have more experience than me --- they know how to use and interpret profiles and benchmarks, they've seen enough to know what does and doesn't work, and they've had practice coming up with ideas to improve performance.

The good news is that performance optimisation usually doesn't involve fiddly low-level programming like this --- simply being aware of when your code performs IO makes a big difference --- and it's never too early or late to start building your experience level, no matter where you are in your career. For me, that meant setting myself the goal of making my open source libraries as fast as I could. For you, it could mean picking a slow part of your codebase at work and trying to make it faster, or pair-programming with a colleague, or thoroughly reading through some unfamiliar code. Making your own opportunities to learn new things is the best way to get better.
