---
layout: docs
title: Functions
permalink: docs/tutorial/functions
---

# Functions

For defining a function, you may use similar syntax know from other languages such as Scala:

```
def fib(n: Int): Int =
  if (n == 0 || n == 1) 1
  else fib(n - 1) + fib(n - 2)
```

Since `fib` is a recursive function, you need to explicitly annotate its return type. Parameters like `n` always need to be
annotated with their type.

Calling functions works as you might expect:

```effekt:repl
fib(5)
```

Perhaps unusual, you can also call `fib` using Effekt's implementation of [Uniform Function Call Syntax
](https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax#cite_note-4):

```effekt:repl
5.fib()
```
Here, the receiver (before the `.`) is simply passed as first argument to function `fib`. If there are no additional arguments, you can also omit the parenthesis:

```effekt:repl
5.fib.show.println
```

To define a type polymorphic (generic) function, like the identity function, you need to introduce a type parameter enclosed in square brackets:

```
def identity[A](x: A): A = x
val a = identity(42)
val b = identity[Int](42)
```

When calling the function, you may explicitly pass a type argument for each type parameter or let type inference do its job.

There are also nested function definitions. Thus, we also could have defined `fib` like this:

```
def fibNested(n: Int): Int = {
  def inner(last: Int, current: Int, count: Int): Int =
    if (count == 1) current
    else inner(current, last + current, count - 1)

  inner(0, 1, n)
}
```

```effekt:repl
fibNested(5)
```
Only on the toplevel, functions can be mutually recursive.

```effekt
def even(n: Int): Bool = if (n <= 0) true else odd(n - 1)
def odd(n: Int): Bool = if (n <= 0) false else even(n - 1)
```
```effekt:repl
7.even
```
