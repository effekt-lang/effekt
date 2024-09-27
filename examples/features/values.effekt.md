---
title: Values
permalink: tour/values
---

# Values

Values in Effekt are terms that are _pure_ in the sense that they cannot have any side-effecting behavior. This includes
the usual `String`, `Bool`, `Int`, `Double` and `Char` literals, including unicode literals, but also user-custom data types, which we will be presented in a separate chapter.

```
val pi = 3.14
val int = 42 * 2
val bool = true && false
val str = "hello" ++ ", " ++ "world"
val c = 'c'
val unicode = \u039E // Ξ
```

Most values of built-in types can be printed with `println`, however, `println` is not defined for others, e.g., `Char` or user-defined data types.

```effekt:repl
println(str)
```
It is common to define a `show` function that renders a value as a string and then print this value:

```effekt:repl
println(show(bool))
```

These `show` functions are predefined for many types in the standard library.

For quick introspection of arbitrary types, the standard library also provides an `inspect` function that works on arbitrary values (only available for certain backends). This outputs a pretty-printed version of a compiler-internal representation and is not meant to be used for actual "production" code.

```effekt:repl
inspect(unicode)
```
