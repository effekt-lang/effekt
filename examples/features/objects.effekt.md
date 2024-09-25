---
layout: docs
title: Objects
permalink: docs/tutorial/objects
---

# Objects

Given an effect interface;

```
interface Exception {
  def throw(msg: String): Nothing
}
```

besides implementing a handler for it, there is also another way of defining the semantics of an effect:

```
def excImpl: Exception = new Exception {
  def throw(msg) = panic(msg)
}
```

`excImpl` is an object, a concrete implementation of the `Exception` effect. You can access these implementations by referencing the object, followed by a `.` and the name of the operation:

```effekt:repl
excImpl.throw("hello")
```

Objects can also be passed as computation types to functions:

```
def div(a: Double, b: Double) { exc: Exception }: Double / {} = {
  if (b == 0) exc.throw("division by zero")
  else a / b
}
```

Notice that even though we are using the `Exception` effect, the set of to be handled effects in the return signature is empty.

```effekt:repl
div(42, 0) { excImpl }
```

You may also explicitly bind the capability defined by the implementation of the handler and pass it to `div` as a computation:

```effekt:repl
try { div(42, 0) { exc } }
with exc: Exception { def throw(msg) = panic(msg) }
```

This capability-passing-style transformation [Schuster et al., 2020](https://doi.org/10.1145/3408975) is normally done implicitly and ensures that every effect is handled.
