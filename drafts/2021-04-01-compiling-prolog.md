---
title: Compiling Prolog
---

> **Work in progress**: This article is gonna be lo-o-ong. I initially thought about splitting it into separate posts, but instead I decided to write a single article and post it section by section.

Since we last saw each other, I've outfitted [my miniature Prolog interpreter](/posts/2019-12-01-write-you-a-prolog.html) with [a virtual machine and an x64 code generator](https://github.com/benjamin-hodgson/Amateurlog/tree/master/Machine). I wanted to document my work in guide form.

Why write another introductory build-a-compiler guide? There are lots of examples out there of how to make a compiler, but they tend to skew towards simple languages with a straightforward execution model (imperative languages, or simple functional ones). Prolog's core is a simple language, but it features an unusual execution model which doesn't map straightforwardly into low-level code, so I figured it'd make an interesting example.

This article leads on from [my earlier series of four posts](/posts/2019-12-01-write-you-a-prolog.html) about modelling Prolog in C#. I'd recommend reading those before you read this so as not to get lost. In order to get the code working, though, you'll only need [the AST](https://github.com/benjamin-hodgson/Amateurlog/blob/master/Syntax.cs) from [the first post](/posts/2019-12-01-write-you-a-prolog.html) and [the parser](https://github.com/benjamin-hodgson/Amateurlog/blob/master/Parser.cs) from [the second one](/posts/2019-12-08-parsing-prolog-with-pidgin.html).

I did a lot of reading while working on this code, especially [_Warren's Abstract Machine: A Tutorial Reconstruction_](http://wambook.sourceforge.net/). I'll be covering topics in a similar order to that book, but my design is a bit different than Warren's. I was trying to write simple code which fits the way I think, not efficient code suitable for a real language implementation. Throughout this article I’ll point out times when I chose a simple-and-inefficient option over a more sophisticated design.


## Abstract Machines

We’re going to spend much of this article developing an *abstract machine*, a model of a mechanistic system designed for running Prolog. We’ll compile high-level Prolog into a sequence of *instructions* which cause the machine to update its internal state. The `Machine` class is going to look like this:

```csharp
class Machine
{
    void Exec(Instruction instruction);
}
```

It’s important when designing an abstract machine to make something which looks roughly like a real CPU. Our `Machine` class is going to consume programs in the form of a linear stream of instructions, and manipulate word-sized data in a flat address space. We won’t directly use high-level constructs like expressions or objects — instead we’ll model those from scratch using low-level tools.

Why use an abstract machine at all? In the previous post series, I described Prolog’s semantics in terms of high-level C# constructs like dictionaries, lazy enumerables, `from...select`... That’s all very well if you already have access to those language features, but it doesn’t suffice as a recipe for getting a machine to run Prolog. I want to generate x64 assembly, without building all those high-level and inefficient tools myself. My abstract machine will serve as an intermediate step — a thinking tool to help go from high-level Prolog to low-level x64.

Many production compilers have a similar design: source code is converted into an *intermediate representation* which is then optimised and eventually translated into assembly. LLVM, for example, is an off-the-shelf intermediate representation meant to serve as a stable compilation target for all sorts of language implementations. (I initially thought of using LLVM in this project, but Prolog’s unusual control flow doesn’t map cleanly onto LLVM, so it was easier for me to cut out the middle man.)


## Anatomy of a Heap Object

A compiler's job, to a large extent, is to flatten things. The code you type into your editor typically has a nested structure, with curly braces and parentheses and whatnot, because that's a convenient way for humans to think. But it's not convenient for a CPU! Your CPU consumes a linear sequence of instructions and manipulates memory through a flat address space. When you work with a graph of objects with references to each other, the compiler has to figure out a way to represent that graph of objects in that flat address space.

Like most high-level languages, Prolog gives you transparent access to a memory area called the *heap*. Prolog’s heap is where terms and variables are stored during the execution of a program. Programmers happily manipulate objects in the heap without thinking about how they’re represented, but under the hood the heap is a flat memory area like any other and it’s the compiler’s job to manage the way objects are represented in that flat area.

We’ll model the heap as a large array and pretend that it won’t get full. We need to track how much of the array is occupied, so that we know where to put new objects without overwriting old ones.

```csharp
class Machine
{
    // 640k ought to be enough for anybody
    private readonly int[] _heap = new int[160000];
    private int _topOfHeap = 0;
}
```

So we have access to a big flat slab of words, and we need to think of a way to represent nested Prolog terms like `wibble(wobble(X), wubble(X, Y))`.

Let's start simple. How would we represent a **structure** term with no arguments such as `wibble()`? We could store the name of the structure on the heap as a string, but for efficiency's sake let's instead give it an integer ID. (These IDs will be generated by the compiler. The corresponding strings will be stored in a static data structure called a _symbol table_. More on that later.)

```
wibble()
↓
0     1
-------
| 789 |
-------
  ID
```

Simple enough! How about a structure with **arguments** like `wibble(wobble(), wubble())`? We'll want to store the number of arguments; we can put it right after the structure ID. The arguments will each themselves be objects stored elsewhere on the heap. We'll store the address of each of `wibble`'s arguments immediately after the `length` field. (I'll update `wubble()`'s representation to match this format.)

```
wibble(..., ...)        wobble()    wubble()
↓                       ↓           ↓
0     1     2     3     4     5     6     7     8
-------------------------------------------------
| 123 |  2  |  4  |  6  | 456 |  0  | 789 |  0  |
-------------------------------------------------
  ID    len   arg1  arg2   ID   len    ID   len
               ↑     ↑
address of wobble()  address of wubble()
```

Finally let's think about **variables** like `X` and `Y`. We need to choose a representation which supports *binding* — the act of setting a variable equal to another term during unification. One simple way of binding a variable to a term is with a pointer. Our variables will contain a pointer to another heap object; *unbound* variables will point to themselves by convention. (I could’ve used the null pointer for this purpose, but I didn’t. No real reason.)

Variables in this system are mutable. We bind a variable to a term by updating it in place. (Already-bound variables don’t get re-bound, though. It’s a write-once system.) This is in contrast with [my high-level model](/posts/2019-12-15-generic-unification-with-sawmill.html) which explicitly manipulated an immutable substitution object. Using mutation like this will cause some complications later on when we get around to backtracking.

We also need a way to tell variables apart from structures, so all of our heap objects will start with a *tag* word — `0` for variables and `1` for structures. I’m going to sprinkle tag words over structures’ fields too, needlessly and wastefully, because it’ll make some tasks simpler down the line. (Specifically, it's convenient for structure fields to have the same representation as bound variables.)

Here's how the `wibble(wobble(X), wubble(X, Y))` example looks in the finalised version of its heap representation.

```
wibble(..., ...)        
↓
0     1     2     3     4     5     6      7
--------------------------------------------
|  1  | 123 |  2  |  0  |  7  |  0  |  12  | ...
--------------------------------------------
 tag    ID    len   tag  arg1   tag   arg2

    wobble(...)
    ↓
    7     8     9     10    11    12
    --------------------------------
... |  1  | 456 |  1  |  0  |  19  | ...
    --------------------------------
      tag   ID    len   tag   arg1
                               ↑
                         address of X

    wubble(..., ...)
    ↓
    12    13    14    15    16     17    18     19
    ---------------------------------------------
... |  1  | 789 |  2  |  0  |  19  |  0  |  21  | ...
    ---------------------------------------------
      tag   ID    len   tag   arg1   tag   arg2

    X            Y
    ↓            ↓
    19    20     21    22     23
    ---------------------------
... |  0  |  19  |  0  |  21  |
    ---------------------------
      tag   ptr    tag   ptr
             ↑            ↑
    X and Y are both unbound variables
    (they contain their own addresses)
```

In the end, structures have a three-word header consisting of a tag, an ID for the name of the structure, and a length. Then come the fields, each of which consists of a `0` tag followed by the address of a child object. Variables have a one-word header — the tag — and a single field.

Overall this design for heap objects is not so different than that of many industrial-strength languages. In .NET, for example, objects consist of a two-word header followed by a sequence of fields. The header contains information about the object’s type, its virtual methods, and other .NET runtime features like locking. My system is much simpler (and less efficient) in the details, though — my data cells take up a whole word each (I didn’t want to worry about alignment) and I’m not doing any bit-packing tricks to save space.


## Traversing the Heap

Here are a few functions which process data on the heap.

First up, `Bind`. `Bind` takes two heap objects, at least one of which must be an unbound variable, and sets it to refer to the other object. (If they’re both unbound variables, it prefers to bind the more recently created one — that is, the one with the higher address.)

```csharp
private void Bind(int addr1, int addr2)
{
    (addr1, addr2) = (Math.Max(addr1, addr2), Math.Min(addr1, addr2));

    if (_heap[addr1] == 0 && _heap[addr1 + 1] == addr1)
    {
        // addr1 is the address of an unbound variable
        // no-op: (addr1, addr2) = (addr1, addr2);
    }
    else if (_heap[addr2] == 0 && _heap[addr2 + 1] == addr2)
    {
        // addr2 is the address of an unbound variable
        (addr1, addr2) = (addr2, addr1);
    }
    else
    {
        throw new Exception("Bind needs an unbound variable");
    }

    _heap[addr1 + 1] = addr2;
}
```

Once a variable has been bound, `Deref` finds the object it’s bound to. There may have been a chain of bindings, so `Deref` loops as long as `address` is a bound variable. `Deref` will always return the address of either a structure or an unbound variable. In general, when pulling data from the heap, the first order of business is to `Deref` it.

```csharp
private int Deref(int address)
{
    while (_heap[address] == 0 && _heap[address + 1] != address)
    {
        address = _heap[address + 1];
    }
    return address;
}
```

You may know `Bind` and `Deref` better by the names `Union` and `Find`. Prolog’s variables are an application of [the classic Union-Find algorithm](https://en.m.wikipedia.org/wiki/Disjoint-set_data_structure)! (A real Prolog implementation would incorporate [path compression](https://en.m.wikipedia.org/wiki/Disjoint-set_data_structure#Finding_set_representatives) to minimise the amount of time spent dereferencing chains of bindings. However, Prolog’s semantics mean path compression is not always safe! More on that later.)

With `Bind` and `Deref` in hand, we can `Unify` two heap objects. Now, [the `Unify` I wrote back in 2019](/posts/2019-12-15-generic-unification-with-sawmill.html) was a recursive function which called itself on the children of the current term. I could write this `Unify` recursively, but I’m expecting to translate this code to assembly, which doesn’t natively support recursion. To make that task simpler I’m going to write `Unify` to explicitly manipulate a stack.

```csharp
class Machine
{
    // ...
    private readonly int[] _stack = new int[160000];
    private int _topOfStack = -1;
    private int _frameBase = -1;
    
    private void Push(int data)
    {
        _topOfStack++;
        _stack[_topOfStack] = data;
    }
    private int Pop()
    {
        var result = _stack[_topOfStack];
        _topOfStack--;
        return result;
    }
}
```

I have it in mind to use the stack for other things, so `Unify` can’t simply keep going until the stack is empty — it needs to remember how tall it was previously. That’s what the (suggestively-named) `_frameBase` field is for.

As far as `Unify` is concerned, the stack will contain (the addresses of) pairs of terms to be unified.

```csharp
private bool Unify(int left, int right)
{
    // save _frameBase so it can be restored when returning
    Push(_frameBase);
    _frameBase = _topOfStack;
    Push(left);
    Push(right);
    
    // invariant: the stack contains an even number of items
    while (_topOfStack > _frameBase)
    {
        // the top two items are the next pair of terms to unify
        var address1 = Deref(Pop());
        var address2 = Deref(Pop());

        if (_heap[address1] == 0 || _heap[address2] == 0)
        {
            // unbound variable
            Bind(address1, address2);
        }
        else  // both terms are structures
        {
            // match the structures’ IDs
            if (_heap[address1 + 1] != _heap[address2 + 1])
            {
                // different atoms
                _topOfStack = _frameBase;
                _frameBase = Pop();
                return false;
            }
            // match their lengths
            var length1 =_heap[address1 + 2];
            var length2 = _heap[address2 + 2];
            if (length1 != length2)
            {
                _topOfStack = _frameBase;
                _frameBase = Pop();
                return false;
            }
            // repeat with each pair of children
            for (var i = 0; i < length1; i++)
            {
                // Push the address of each pair of fields.
                // The data at this address has the form of a variable
                Push(address1 + 3 + i * 2);
                Push(address2 + 3 + i * 2);
            }
        }
    }
    _topOfStack = _frameBase;
    _frameBase = Pop();
    return true;
}
```

The code is complicated by the explicit stack and the index arithmetic for accessing the heap, but the algorithm itself is recognisable.
1. If either term is a variable, bind it to the other term.
2. If neither is, compare the structures’ heads and fail if they don’t match.
3. Pair up the children and repeat.

The code also features [a function named `Dump`](https://github.com/benjamin-hodgson/Amateurlog/blob/55f3a5b9eda9f6e889d0cb55d1e12de644317a84/Machine/Machine.cs#L366) which writes a string representation of a term out to the console. In the interest of space I won't go into detail about its implementation, except to mention that it uses a _symbol table_ to look up structures' names. I'll describe the symbol table when we get on to talking about the compiler.

That's everything about the representation of terms on the heap. In the next part, I'll introduce the instructions which build heap objects, and the compiler which generates those instructions from high-level Prolog.
