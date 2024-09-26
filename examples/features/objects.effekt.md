---
layout: docs
title: Objects
permalink: tour/objects
---

# Objects

Effekt features a simple object-system. Let us assume the following interface:

```
interface Counter {
  def increment(): Unit
  def get(): Int
}
```

Interfaces are [_computation types_](./computation) and we can write a program that expects a counter to work on:

```
def useCounter {c: Counter} = {
  println(c.get())
  c.increment()
  c.increment()
  println(c.get())
}
```
Here `c.increment()` is the syntax for calling the operation `increment` on the object `c` of type `Counter`.

We can create an instance using `new`:
```
def runWithCounter() = {
  var count = 0;
  def counter = new Counter {
    def increment() = { count = count + 1 }
    def get() = count
  }

  useCounter {counter}
}
```
```effekt:repl
runWithCounter()
```
In the above example, `new Counter { ... }` creates a new `Counter` instance by providing implementations for each operation. Note how we use `def` to bind `counter`: it is a computation, not a value.
In the call to `useCounter`, we can see how we pass computation, such as `counter` in braces.

## Objects and Handlers
The instantiation of an object might be reminiscent of how handlers implement effect signatures.
This is no coincidence: there is a close correspondence between interfaces and effect signatures, as well as capabilities and objects.

In fact, we can also implement the `Counter` interface with an effect handler:

```
def counterAsEffect() = {
  var count = 0;
  try {
    println(do get())
    do increment()
    do increment()
    println(do get())
  } with Counter {
    def increment() = { count = count + 1; resume(()) } // note the resume
    def get() = resume(count) // note the resume here as well
  }
}
```
```effekt:repl
counterAsEffect()
```
Explicitly binding the capability makes the correspondence very clear:

```
def counterAsEffect2() = {
  var count = 0;
  try {
    useCounter {counter}
  } with counter: Counter {
    def increment() = { count = count + 1; resume(()) }
    def get() = resume(count)
  }
}
```

```effekt:repl
counterAsEffect2()
```
As part of its compilation pipeline, and guided by the type-and-effect system,
the Effekt compiler performs this translation from implicitly handled effects to explicitly passed capabilities [Brachthäuser et al., 2020](https://dl.acm.org/doi/10.1145/3428194).
