---
layout: docs
title: Blocks
permalink: docs/tutorial/blocks
---

# Blocks

```effekt:hidden
interface Yield[A] {
  def yield(x: A): Unit
}
```
## Values vs. Computation
Following Paul Levy's [Call-By-Push-Value](https://www.cs.bham.ac.uk/~pbl/papers/thesisqmwphd.pdf) Effekt distinguishes between **values** (such as `42`, `true`, or instances of datatypes) and **computation**.

Examples of "computation" in Effekt are:
- blocks (this is what we call functions),
- instances of interfaces (objects), and
- regions

Functions (and all other computation) are _second-class_ in Effekt. To make this difference explicit, we pass values in parenthesis (e.g. `f(42)`) and computation in braces (e.g. `f { x => println(x) }`).

```
def myMap[A, B](xs: List[A]) { f: A => B }: List[B] =
  xs match {
    case Nil() => Nil()
    case Cons(x, xs) => Cons(f(x), myMap(xs) { f })
  }
```

The block parameter `f` is enclosed in curly braces and syntactically separated from the value parameters. 
While multiple value parameters are separated by comma, multiple block parameters are expressed enclosing each in curly braces.

```effekt:repl
[1, 2, 3].myMap { x => x * 2 }
```

In the previous example, the block passed to `myMap` did not use any effect operations and the signature of `f` in `myMap` did not mention any as well. What happens if
the argument passed for `f` is effectful?

```
def example(): List[Int] / Yield[Int] = [1, 2, 3].map { x => do yield[Int](x); x * 2 }
```

We can apply a "contract"-based reading to understand effects. The expanded type of `myMap`

```effekt
myMap[A, B](xs: List[A]) { f: A => B / {} }: List[B] / {}
``
has an empty effect set on `f` and an empty effect set on `myMap`. 
In general, effects in covariant positions (like those on `myMap`) are part of the precondition and considered as _requirements_ for the calling context.
In contrast, effects in contravariant positions (like those on `f`) are part of the postcondition and can be read as provided (that is, "myMap provides these capabilities to `f`").

This reading is consistent with something we call _effect parametricity_: since the signature of `f` does not mention any effects, the implementation of `myMap` cannot handle any effects occurring in `f`. 

The effects used in the passed block thus need to be handled exactly where the block is defined, that is, at the call-site of `myMap`. 
