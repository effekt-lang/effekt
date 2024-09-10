---
layout: docs
title: Regions
permalink: docs/tutorial/regions
---

# Regions

## State and continuations

Consider the following handle for non-determinism:

```
interface Amb { def flip(): Boolean }

def allChoices { prog: () => Unit / Amb }: Unit = {
  try { prog() }
  with Amb {
    def flip() = {
      resume(true); resume(false)
    }
  }
}
```

Multiple resumptions and mutable state need to be handled with care such that the state is appropiately restored upon each resumption.

```
def example1() = {
  var x = 1
  allChoices {
    if (do flip()) { x = 2 }
    else { () }
    println(x)
  }
}
```

In this example, `x` is defined outside of the handler `allChoices`. Thus, when resuming for the second time, the changes made to `x` carry over:

```effekt:repl
example1()
```

However, if we define `x` within the handler, the behaviour is a different one.

```
def example2() = {
  allChoices {
    var x = 1
    if (do flip()) { x = 2 }
    else { () }
    println(x)
  }
}
```

Opposed to the previous example, the state of `x` actually backtracks:

```effekt:repl
example2()
```

This is because the state is allocated on the stack and thus, the captured continuation includes the initial state of `x` and is therefore restored when resumed.

## Explicit region-based allocation

All mutable variables are allocated in regions. These become visible when trying to close over references to mutable variables:

```
def example3() = {
  var x = 1
  val closure = box { () => x }
  () // try returning closure here
}
```

Here, the type of `closure` is `() => Int at {x}`. The capture set makes closing over `x` explicit. Each mutable variable is allocated into a equally named region.
We can also explicitly instantiate a new region and allocate `x` into it:

```
def example4() =
  region r {
    var x in r = 1
    val closure = box { () => x }
    ()
  }
```

By doing so, the type of `closure` becomes `() => Int at {r}` and thereby signifying that `closure` closes over something allocated in the region `r`.
`closure` may only ever be unboxed where region `r` is still in scope. Otherwise, accessing `closure` outside of `r` would exceed the lifetime given to `x` by allocating it in `r`.

Additionally, regions are second-class like blocks and objects and thus can be passed to functions. We use this for rewriting the opening example from the previous section:

```
def prog {r: Region} = {
  var x in r = 1
  if (do flip()) { x = 2 }
  else { () }
  println(x)
}

def example1Region() =
  region r {
    allChoices {
      exampleProgram {r}
    }
  }

def example2Region() =
  allChoices {
    region r {
      exampleProgram {r}
    }
  }
```

Running it will give us the same result:

```effekt:repl
example1Region();
```

## Global

It is also possible to allocate a variable globally by allocating it into the built-in region `global`. With this, it is possible to write a program which is normally not possible:

```
def example5() = {
  var x in global = 1
  val closure = box { () => x }
  closure
}
```

We can return a closure that closes over a variable. This is only possible because `x` is allocated into the `global` region and therefore has a static lifetime.

## References

- [Region-based Resource Management and Lexical Exception Handlers in Continuation-Passing Style](https://link.springer.com/chapter/10.1007/978-3-030-99336-8_18)
- [Effects, capabilities, and boxes: from scope-based reasoning to type-based reasoning and back](https://dl.acm.org/doi/10.1145/3527320)
