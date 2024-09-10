---
layout: docs
title: Captures
permalink: docs/tutorial/captures
---

# Captures

## Notes (TO BE IGNORED)

Boxing is almost never needed. The cases where you need it are complex yet interesting. While it appears to be complicated, it guarantees safety and helps writing correct programs.

Aspects

- disallowing effects (only `{ io, global }` since these can be run without a handler or pure `{}`)
  - FFI on click (event-handler)
  ```
  do promise(box { println("hello") })
  ```
- storing the resumption/continuation (scheduler, reified generator)
- classes-pattern
  ```
  interface Counter {
    def tick(): Unit
    def current(): Int
  }

  def newCounter(init: Int): Counter at {global} = {
    var state in global = init // or pass a region
    box new Counter {
      def tick() = state = state + 1
      def current() = state
    }
  }
  /*
  class GlobalCounter(init: Int): Counter {
    var state in global = init // or pass a region
    def tick() = state = state + 1
    def current() = state
  }
  */

  def myCounter = unbox newCounter(0)
  ```
  alternative we can write it in "CPS" to avoid boxing
  ```
  def counter[R](init: Int) { prog: {Counter} => R }: R = {
    var state = init // NO region needed here
    def c = new Counter {
      def tick() = state = state + 1
      def current() = state
    }
    prog {c}
  }

  // ...
  with def myCounter = counter(0);
  ...
  ```
  - on term level unbox, box
  - `at` at type level
  - capability, resource, region, lifetime, coeffect

## Captures

Functions are second-class in Effekt, but for a good reason. Functions closing over capabilities must not be returned, otherwise it can not be guaranteed that these capabilities are still in scope.
Thus, we explicitly have to keep track of these captures and ensure they are in scope upon envoking.

Consider the following example where `divide` is being passed a explicit capability:

```
def divide(n: Int, m: Int) {exc: Exception}: Int =
  if (m == 0) { exc.throw("uhoh") }
  else { n / m }
```

```effekt:repl
try { 
  val res = divide(10, 0) {exc}
  println(res)
} with exc: Exception {  
  def throw(msg) = println("ERROR: " ++ show(msg))
}
```

What happens if we return the capability?

```
def example() = try {
  exc
} with exc: Exception { 
  def throw(msg) = panic("ohno")
}
```

By directly returning `exc` from the `try` expression, we break the lexical effect handling reasoning since `exc` is only defined within the `try` body.
For the same reason, returning a closure is not an option either.

For preventing capabilities escaping their scope, Effekt uses captures. In case of the previous example, the return type of `example` is `Exception at {exc}`. This is called
a boxed computation. A boxed computation may only be used after unboxing it, which is only permitted when all its captures are in scope.

## Boxing

While this may seem like a restriction, this also has upsides. For example, using boxing, you can have first-class functions. You can even ensure that these first-class functions
only use a certain set of effects.

Consider a simple scheduler that runs two functions in parallel:

```
def parallelWrong { f: () => Unit } { g: () => Unit }: Unit = <>
```

Since `parallelWrong` expects two arbitrary blocks as arguments, these blocks may capture arbitrary capabilities.
Running both in parallel can have non-deterministic side-effects or introduce data races.
Thus, we need to enforce that both `f` and `g` are pure, that is, do not capture any capabilities.

```
def parallel (f: () => Unit at {}, g: () => Unit at {}): Unit = <>
```

Converting second-class computations to first-class values can be done either explicitly by `box`ing them or implicitly when
- they occur in return position,
- on the right-hand side of assignments,
- or being passed as arguments where first-class values are expected.

```effekt:repl
parallel(
  box { () => println("Hello, ") },
  box { () => println("world!") }
)
```

Here both arguments have the capture set `{io}` while the expected capture set is expected to be empty `{}`. Hence, this call does not type-check.

## Built-in 

The capability `{io}` is actually built-in and is handled by the runtime, similar to `IO` in Haskell.

## References

- [Effects, capabilities, and boxes: from scope-based reasoning to type-based reasoning and back](https://dl.acm.org/doi/10.1145/3527320)