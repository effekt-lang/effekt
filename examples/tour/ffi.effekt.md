---
title: FFI
permalink: tour/ffi
---

# FFI (Foreign Function Interface)

Not all functions of the standard library can be defined in Effekt itself (for example, what does it mean to print?).
For some functions, we need to reach out to the host platform. This is what the FFI is for.

When defining FFI, try to follow the following principle:

> **keep the FFI surface minimal**

The reasoning for this is:

1. less foreign types and less foreign functions means more compatibility across backends (JS and LLVM are the most important)
2. foreign types and functions are opaque for the Effekt optimizer and thus cannot be optimized

## `extern def`

Effekt allows communicating with the host platform (for instance, JavaScript) by defining extern functions via FFI.
Let's start with the JavaScript backend by defining a function that adds two numbers:

```
extern def add(n: Int, m: Int): Int =
  js "${n} + ${m}"
```

Every call of `add(n, m)` is replaced with `n + m` in the JavaScript backend.

## `extern type`

It's often useful to define opaque extern types to encapsulate a foreign type:

```
extern type MyRef[T]

extern def myRef[T](init: T): MyRef[T] =
  js "{ value: ${init} }"

extern def get[T](ref: MyRef[T]): T =
  js "${ref}.value"
```

## `extern include`

Sometimes, you want to include a whole JavaScript file containing your favorite JavaScript library
so that you can write bindings for it later.
You can do that in Effekt with the following code:

```effekt:sketch
extern include js "./mycoollibrary.js"
```

## `extern` strings

Do you want to quickly define some inline JavaScript function instead of creating a whole `.js` file for it?
Use `extern` strings!

```
extern js """
  function set$impl(ref, value) {
    ref.value = value;
    return $effekt.unit; // this is the Unit-value of Effekt
  }
"""

extern def set[T](ref: MyRef[T], value: T): Unit =
  js "set$impl(${ref}, ${value})"
```

Now you can call `set` on your reference!
```
def example() = {
  val r = myRef(42)
  r.set(r.get + 1)
  r.get
}
```
```effekt:repl
example()
```

## Captures

Not all extern definitions are pure.
Since Effekt doesn't know anything about externs, you should manually annotate the [capture](./captures) of your extern function as `extern <capture> def ...`

- no capture, usually denoted as `pure`, also writable as `{}`. If an extern definition is annotated as `pure`, it will be considered for inlining by the compiler.
- if you want to allocate into the [global region](./regions) (e.g. the JavaScript heap), use `global`
- if your function performs I/O (like `println`), use `io` (this is the default capture)
- if your function captures the continuation, [use `async`](./io) (in the JS backend, control definitions should return a monadic value of type `async`)
- of course, you can mix and match: `extern def {global, io, control} ...`

For example, the following function gets the current timestamp via the JavaScript FFI, which is an operation that is not pure (it has a side effect):

```
extern io def now(): Int =
  js "Date.now()"
```

For example, the `myRef` and `set` functions above should have been annotated as `extern global def` as they allocate globally.

If you want to add your own tracked resource `foo` of type `MyResource`, Effekt supports the following:

```
extern interface MyResource
extern resource foo: MyResource
```

## Other backends

Effekt has a lot of different backends. One `extern def` can support multiple backends.
Here's how to `mul`tiply two numbers on different backends:

```
extern pure def mul(x: Int, y: Int): Int =
  js "(${x} * ${y})"
  chez "(* ${x} ${y})"
  llvm "%z = mul %Int ${x}, ${y} ret %Int %z"
```

You can also use multiline strings if you need to splice in a lot of code and `default { ... }` to specify a default implementation in Effekt:

```
// used as a fallback impl for `not`
def primNot(b: Bool) = if (b) false else true

extern pure def not(b: Bool): Bool =
  js "!${b}"
  llvm """
    %p = extractvalue %Pos ${b}, 0
    %q = xor i64 1, %p
    %adt_q = insertvalue %Pos zeroinitializer, i64 %q, 0
    ret %Pos %adt_q
  """
  default { primNot(b) }
```

```effekt:repl
now()
```

## Reference

[`effekt.effekt`](https://github.com/effekt-lang/effekt/blob/master/libraries/common/effekt.effekt) is the main stdlib file defining things like primitive addition for each backend.
We highly recommend reading through the source code of the standard library to see how FFI is used.
