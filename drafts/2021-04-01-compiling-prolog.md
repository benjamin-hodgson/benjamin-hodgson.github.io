---
title: Compiling Prolog
---

> **Work in progress**: This article is gonna be lo-o-ong. I initially thought about splitting it into separate posts, but instead I decided to write a single article and post it section by section.

Since we last saw each other, I've outfitted [my miniature Prolog interpreter](/posts/2019-12-01-write-you-a-prolog.html) with [a virtual machine and an x64 code generator](https://github.com/benjamin-hodgson/Amateurlog/tree/master/Machine). I wanted to document my work in guide form.

Why write another introductory build-a-compiler guide? There are lots of examples out there of how to make a compiler, but they tend to skew towards simple languages with a straightforward execution model (imperative languages, or simple functional ones). Prolog's core is a simple language, but it features an unusual execution model which doesn't map straightforwardly into low-level code, so I figured it'd make an interesting example.

This article leads on from [my earlier series of four posts](/posts/2019-12-01-write-you-a-prolog.html) about modelling Prolog in C#. I'd recommend reading those before you read this so as not to get lost. In order to get the code working, though, you'll only need [the AST](https://github.com/benjamin-hodgson/Amateurlog/blob/master/Syntax.cs) from [the first post](/posts/2019-12-01-write-you-a-prolog.html) and [the parser](https://github.com/benjamin-hodgson/Amateurlog/blob/master/Parser.cs) from [the second one](/posts/2019-12-08-parsing-prolog-with-pidgin.html).

I did a lot of reading while working on this code, especially [_Warren's Abstract Machine: A Tutorial Reconstruction_](http://wambook.sourceforge.net/). I'll be covering similar topics in a similar order to that book, but my design is a bit different than Warren's --- just because I was trying to write code which fits the way I think. (I'm certainly not claiming my system is better --- the WAM is a mature design used in production-quality Prolog systems!)


## Abstract Machines

We’re going to be spending much of this article developing an *abstract machine*, a model of a mechanistic system designed for running Prolog. We’ll compile high-level Prolog into a sequence of *instructions* which cause the machine to update its internal state. The `Machine` class is going to look like this:

```csharp
class Machine
{
    void Exec(Instruction instruction);
}
```

It’s important when designing an abstract machine to make something which looks roughly like a real CPU. Our `Machine` class is going to consume programs in the form of a linear stream of instructions, and manipulate word-sized data in a flat address space. We won’t directly use high-level constructs like expressions or objects - instead we’ll model those from scratch using low-level tools.

Why use an abstract machine at all? In the previous post series, I described Prolog’s semantics in terms of high-level C# constructs like dictionaries, enumerables, `from...select`... That’s all very well if you already have access to those language features, but it doesn’t really suffice as a recipe for getting a machine to run Prolog. I want to generate x64 assembly, so my abstract machine will serve as an intermediate step --- a thinking tool to help go from high-level Prolog to low-level x64.

Many production compilers have a similar design: source code is converted into an *intermediate representation* which is then optimised and eventually translated into assembly. LLVM, for example, is an off-the-shelf intermediate representation meant to serve as a stable compilation target for all sorts of language implementations. (I initially wanted to use LLVM in this project, but Prolog’s backtracking system doesn’t map cleanly onto LLVM, so it was easier for me to cut out the middle man.)


## Anatomy of a Heap Object

A compiler's job, to a large extent, is to flatten things. The code you type into your editor often has a nested structure, with curly braces and parentheses and whatnot, because that's a convenient way for humans to think. But it's not convenient for a CPU! Your CPU consumes a linear sequence of instructions and manipulates memory through a flat address space. When you work with a graph of objects with references to each other, the compiler has to figure out a way to represent that graph of objects in that flat address space.

Like most high-level languages, Prolog gives you transparent access to a memory area called the *heap*. Prolog’s heap is where terms and variables are stored during the execution of a program. Programmers happily manipulate objects in the heap without thinking about how they’re represented, but under the hood the heap is just a flat memory area like any other.

We’ll model the heap as a large array and pretend that it won’t get full. We also need to track how much of the array is occupied, so that we know where to put new objects without overwriting old ones.

```csharp
class Machine
{
    // 640k ought to be enough for anybody
    private readonly int[] _heap = new int[160000];
    private int _topOfHeap = 0;
}
```

So we have access to a big flat slab of words, and we need to think of a way to represent nested Prolog terms like `foo(bar(X), Y, baz(X), quux())`.

Let's start simple. How would we represent a structure term with no arguments such as `quux()`? We could store the name of the structure on the heap as a string, but for efficiency's sake let's instead give it an ID --- say, `123`. (These IDs will be generated by the compiler and the corresponding strings will be stored in a static data structure. More on that later.)

```
address: 0     1
         -------
         | 123 |
         -------
           ID
```

Simple enough! How about a term with arguments like `foo(bar(), quux())`? 
