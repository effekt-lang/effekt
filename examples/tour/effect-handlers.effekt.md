---
title: Effects
permalink: tour/effects
---

# Effects and handlers

Let's start with a very general definition that helps guide our intuition

> While _pure_ expressions like `1 + 2` can be locally reduced to a value (e.g. `3`),
> an _effectful_ expression depends on or modifies its context.

Effect handlers in Effekt allow us to decouple the mere mentioning of an effect operation from its meaning. In short:

- **Effect Signatures** declare which effect operations are available and their types
- **Effect Operations** are a request to the context
- **Effect Handlers** handle these requests and provide meaning to effect operations

## Non-resumptive effects

For getting started with effects and handlers, we define the popular exception effect, consisting just of one effect operation `throw`:

```
interface Exception {
  def throw(msg: String): Nothing
}
```
We declare the effect signature as an _interface_ (like those that [objects](./objects) implement).
An interface starts with the `interface` keyword, followed by its name.
Enclosed in curly braces, _operations_ belonging to this interface are declared with their signature.

While calling a method on an object would require us to know the receiver (e.g. `exc.throw("boom!")`),
every operation can also be used as an effect by omitting the receiver and prepending the keyword `do`:

```
def div(a: Double, b: Double) =
  if (b == 0.0) { do throw("division by zero") }
  else { a / b }
```
Using `throw` as an _effect operation_ means that we do not know (and maybe do not care) who the receiver (and thus implementor) is. We simply express that we want to `throw` and leave it to the context to decide what to do.

The return type of non-recursive functions can be left out and is inferred.
In our example, it is `Double / { Exception }`, that is, the return type `Double` is followed by a slash and the effects that need to be handled by the calling context.

Whereas operations introduce effects, handlers offer a way of discharging them:

```
def unsafeDiv(a: Double, b: Double): Double / {} =
  try {
    div(a, b)
  } with Exception {
    def throw(msg) = {
      panic(msg)
    }
  }
```
```effekt:repl
unsafeDiv(42.0, 0.0)
```

Each handler consists of a `try` block, followed by one or more handlers, starting with `with`,
followed by the name of the effect and definitions for each of the effect's declared operations.

Notice that the handler for `throw` does *not* call `resume`. This is intentional:
not resuming is how you abort -- the rest of the computation is simply discarded.

## Resumptive effects

Besides exceptions, we can also emulate other useful mechanisms with effects. For example, a generator function using the `Yield` effect:

```
interface Yield[A] {
  def yield(x: A): Unit
}
```

When invoking the `yield` operation, we pass a value to be yielded to the lexically nearest handler, discharging the
`Yield` effect. Recall our `fib` functions from earlier. We may also write it as a generator that runs infinitely while
yielding each Fibonacci number in the process.

```
// Infinite fibonacci sequence as a generator function
def fib(): Unit / { Yield[Int] } = {
  def inner(a: Int, b: Int): Unit = {
    do yield(a)
    inner(b, a + b)
  }
  inner(0, 1)
}

// generate all fibonacci numbers up until the given limit.
def genFibs(limit: Int): List[Int] / {} = {
  var count = 0
  var fibs = []
  try {
    fib()
  } with Yield[Int] {
    def yield(x) =
      if (count < limit) {
        count = count + 1
        fibs = Cons(x, fibs)
        resume(()) // <- we resume the computation where yield was invoked
      }
  }
  fibs
}
```

```effekt:repl
genFibs(15).foreach { x => println(x) }
```

When an effect operation fires, the handler receives the *rest of the program* --
everything that would have run after `do yield(a)` -- as an implicit argument called `resume`.

For an effect `yield(x: A): Unit` handled in a `try` block of type `T`, `resume` has type `Unit => T`.
You can hover over `resume` in the editor to always see its exact type.

There are three ways of using `resume`:
- Not calling `resume` aborts the producer, and the continuation is discarded (as with `throw` above).
- Calling `resume` once is the ordinary case seen in generators and state (as with `yield` above). The producer continues with the value passed to `resume`.
- Calling `resume` multiple times runs the rest of the computation multiple times: this is how nondeterminism and backtracking work.

For more details on effect handlers as a concept, see the [Effect Handlers](../docs/concepts/effect-handlers) page.

## Singleton operation

Often, we want to declare an interface that is entirely defined by just one operation(like a "single-abstract-method" in Java). In this case, you can declare it as a _singleton operation_ using the `effect` keyword:

```
effect tell(): Int
```

This is syntactic sugar: a singleton `effect` declaration is exactly equivalent to an `interface` with a single operation of the same name,
and the concise handler syntax `with tell { () => ... }` is shorthand for `with tell { def tell() = ... }`:

```effekt:sketch
// These two are equivalent:
effect tell(): Int

interface tell {
  def tell(): Int
}
```

The handler for a singleton effect uses the same concise syntax:
```effekt
import bench

def tellTime[A] { prog: () => A / tell }: A =
  try { prog() }
  with tell { () =>
    resume(bench::timestamp())
  }
```

Here, `prog` is expected to be passed a computation (block) and not as a value.
You can read more about their distinction in the section about [computations](computation).

Using the function `tellTime` that _handles_ the effect `tell` of `prog`, we can write the following program for measuring the execution time for computing the first 1500 Fibonacci numbers:

```effekt:repl
tellTime {
  val start = do tell()
  val fibs = genFibs(1500)
  val end = do tell()
  println("took " ++ show(end - start) ++ "ns to generate " ++ fibs.size.show ++ " numbers")
}
```

## Chaining handlers

We can decompose `genFibs` into a more general helper function `collect` that collects the
yielded values by abstracting over the generating program:

```
def collect[A](n: Int) { prog: () => Unit / Yield[A] }: List[A] = {
  var yielded: List[A] = []
  var count = 0
  try {
    prog()
  } with Yield[A] {
    def yield(x) =
      if (count < n) {
        yielded = Cons(x, yielded)
        count = count + 1
        resume(())
      }
  }
  yielded
}
```

Let us define another handler for `Yield` to filter values:

```
def filter[A] { keep: A => Bool } { prog: () => Unit / Yield[A] }: Unit / Yield[A] =
  try prog()
  with Yield[A] {
    def yield(x) = if (keep(x)) { do yield(x); resume(()) } else { resume(()) }
  }
```

We can avoid the nesting of higher-order function calls like `collect[Int](limit) { filter { x => x.mod(2) == 0 } { fib() } }` and write:

```
def evenFibs(limit: Int): List[Int] = {
  with collect(limit)
  with filter { x => x.mod(2) == 0 }
  fib()
}
```

```effekt:repl
evenFibs(15).foreach { x => println(x) }
```

## `with` syntax

`with f(args);` is syntactic sugar: it passes the remainder of the current block as the last block argument to `f`.
This lets deeply nested calls (such as handler calls) be written as a flat, non-nested sequence.
The following two forms are exactly equivalent:

```effekt:sketch
// Nested form:
collect(limit) {
  filter { x => x.mod(2) == 0 } {
    fib()
  }
}

// Flat form using `with`:
with collect(limit)
with filter { x => x.mod(2) == 0 }
fib()
```
