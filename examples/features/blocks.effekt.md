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

Functions are second-class in Effekt. While you cannot pass them as first-class values to other functions as argument, you can pass them as second-class blocks.

```
def myMap[A, B](xs: List[A]) { f: A => B }: List[B] =
  xs match {
    case Nil() => Nil()
    case Cons(x, xs) => Cons(f(x), myMap(xs) { f })
  }
```

The block parameter `f` is enclosed by curly braces and is syntactically separated from the value paramters. Of course, there may also be multiple block parameters
each enclosed by curly braces.

```effekt:repl
[1, 2, 3].myMap { x => x * 2 }
```

In the previous example, the block passed to `myMap` did not use any effect operations and the signature of `f` in `myMap` did not mention any as well. What happens if
the argument passed for `f` is effectful?

```
def example(): List[Int] / Yield[Int] = [1, 2, 3].map { x => do yield[Int](x); x * 2 }
```

Since effects are considered as requirements for the calling context and the signature of `f` does not mention any, the implementation of `myMap` cannot handle any effects
occuring in `f`. The effects used in the passed block need to be handled exactly where the block is defined, that is, at the call-site of `myMap`.
