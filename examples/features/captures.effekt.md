---
layout: docs
title: Captures
permalink: docs/tutorial/captures
---

# Captures

Functions are second-class in Effekt, but for a good reason. Functions closing over capabilities must not be returned, otherwise it can not be guaranteed that these capabilities are still in scope.
Thus, we have to keep track of these captures and ensure they are in scope upon invoking.

Consider the following example where `divide` is being passed an explicit capability:

```
interface Exception {
  def throw(msg: String): Nothing
}

def divide(n: Int, m: Int) {exc: Exception}: Int =
  if (m == 0) { exc.throw("uhoh") }
  else { n / m }
```
Handlers (implicitly or explicitly) introduce capabilities that are then passed to the position where an effect is used.
```effekt:repl
try {
  val res = divide(10, 0) {exc} // here we pass it explicitly
  println(res)
} with exc: Exception {
  def throw(msg) = println("ERROR: " ++ show(msg))
}
```
What happens if we try to return the capability?

```effekt:repl
try { exc } with exc: Exception { def throw(msg) = panic(msg) }
```
By directly returning `exc` from the `try` expression, we break the lexical effect handling reasoning since `exc` is only defined within the `try` body.
For the same reason, we must not return a function that closes over such a capability.

To prevent capabilities from escaping their scope, Effekt tracks captures.
In the case of the previous example, the return type of the handler is `Exception at {exc}`.
This is called a boxed computation. A boxed computation may only be used after unboxing it, which is only permitted when all its captures are in scope. In fact, the example above does not typecheck, since the type `Exception at {exc}` would not be well-formed outside of the handler that introduces (and binds!) the capability `exc`.

## Boxing

Effekt distinguishes between _values_ (that can be returned and stored) and _computation_ (that can be passed, but not returned). Computation can (invisibly) close over arbitrary capabilities, while values make this capture explicit in their type. In the below example we use the term-level construct `box`, to explicitly convert a computation to a value.

```
def example() =
  try {
    // Computation
    //
    //  () => Nothing / {}
    //  vvvvvvvvvvvv
    def secondClass() = exc.throw("ohno")

    // Value
    //
    // (() => Nothing / {}) at {exc}
    //  vvvvvvvvvv
    val firstClass = box secondClass;

    ()
  } with exc: Exception { def throw(msg) = panic("ohno") }
```
Computations are bound using `def` and the type doesn't mention captures, while values are bound using `val` and the inferred type mentions the capture `{exc}`.

Converting second-class computations to first-class values can be done either explicitly by `box`ing them or implicitly when
- they occur in return position,
- on the right-hand side of value assignments,
- or being passed as arguments where first-class values are expected.

## Restricting Capture
Boxed computations can be extremely useful to restrict their capture to a certain set of capabilities.
Consider a simple scheduler that runs two functions in parallel:

```
def parallelWrong { f: () => Unit } { g: () => Unit }: Unit = <>
```
Since `parallelWrong` expects two arbitrary blocks as arguments, these blocks may capture arbitrary capabilities.
Running both in parallel can have non-deterministic side effects or introduce data races.
Thus, we need to enforce that both `f` and `g` are pure, that is, do not capture any capabilities.

```
def parallel (f: () => Unit at {}, g: () => Unit at {}): Unit = <>
```
The type-checker will reject calls, where we try to pass functions that to close over capabilities (or builtin resources):
```effekt:repl
parallel(
  box { () => println("Hello, ") },
  box { () => println("world!") }
)
```

Here, by using `println` both arguments have the capture set `{io}` while the expected capture set is expected to be empty `{}`. Hence, this call does not type-check.

## Effects vs. Captures
When programming Effekt, it can be very confusing that there are two different aspects of the type system that ensure type-and-effect safety:

- **Effects** express a requirement to the context (for instance the caller of a function) that certain capabilities still need to be provided.
- **Captures** express a restriction where a computation can be used.

```
effect get(): Int
def effectsVsCaptures() =
  try {
    def closure(): Unit = println(x.get())
    def free(): Unit / {get} = println(do get())

    val boxedClosure = box closure
    val boxedFree = box free

    try {
      closure() // prints 1
      free()    // prints 2
    } with y: get { resume(2) }
  } with x: get { resume(1) }
```
```effekt:repl
effectsVsCaptures()
```
Try hovering over `boxedClosure` and `boxedFree` to see the difference in their effects and capture.

## Built-in Resources
Capture sets can contain (at least) three different kinds of elements:

- capabilities introduced by handlers
- memory regions (see [regions](./regions))
- builtin resources (see [ffi](./ffi) and [IO](./IO))

An example of a builtin resource is `io`, which is handled by the runtime. Other builtin resources include `global` (for global mutable state) and `async` (for asynchronicity).

## References

- [Effects, capabilities, and boxes: from scope-based reasoning to type-based reasoning and back](https://dl.acm.org/doi/10.1145/3527320)
