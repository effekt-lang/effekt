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
val unicode = '\u039E' // Ξ
```

Most values of built-in types can be printed with `println`, however, `println` is not defined for others, e.g., `Char` or user-defined data types.

```effekt:repl
println(str)
```

Manual definitions of `show` for any specific type can be written and will take priority, but
Effekt will generate a `show` function for types that do not have one, which renders the value as a string and then print this value:

```effekt:repl
println(show(bool))
```
