---
layout: docs
title: FFI
permalink: docs/tutorial/ffi
---

# FFI (Foreign Function Interface)

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
extern type Ref[T]

extern def ref[T](init: T): Ref[T] =
  js "{ value: ${init} }"
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
    return $effekt.unit;
  }
"""

extern def set[T](ref: Ref[T], value: T): Unit =
  js "set$impl(${ref}, ${value})"
```

Now you can call `set` on your reference!

## Captures

> NOTE: Link captures/boxing/regions, etc.

You can and should annotate the capture of your extern function as `extern <capture> def ...`

- no capture, usually denoted as `pure`, also writable as `{}`. If an extern definition is annotated as `pure`, it will be considered for inlining by the compiler.
- if you want to allocate into a global scope (e.g. the JavaScript heap), use `global`
- if your function performs I/O (like `println`), use `io` (this is the default capture)
- if your function captures the continuation, use `async` (in the JS backend, control definitions should return a monadic value of type `async`)
- of course, you can mix and match: `extern def {global, io, control} ...`

For example, the following function gets the current timestamp via the JavaScript FFI, which is an operation that is not pure (it has a side effect):

```
extern io def now(): Int =
  js "Date.now()"
```

For example, the `ref` and `set` functions above should have been annotated as `extern global def` as they allocate globally.

If you want to add your own tracked resource `foo` of type `MyResource`, Effekt supports the following:

```
extern interface MyResource
extern resource foo: MyResource
```

## Other backends

Effekt has a lot of different backends. One `extern def` can support multiple backends, so here's how to `mul`tiply two numbers on different backends:

```
extern pure def mul(x: Int, y: Int): Int =
  js "(${x} * ${y})"
  chez "(* ${x} ${y})"
  ml "(${x}: int) * ${y}"
  llvm "%z = mul %Int ${x}, ${y} ret %Int %z"
```

You can also use multiline strings if you need to splice in a lot of code and `default { ... }` to specify a default implementation in Effekt:

```
// used as a fallback impl for `not`
def primNot(b: Bool) = if (b) false else true

extern pure def not(b: Bool): Bool =
  js "!${b}"
  llvm """
    %p = extractvalue %Pos %b, 0
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
