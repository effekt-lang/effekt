# Values

Values in Effekt are terms that are pure in the sense that they cannot have any side-effecting behaviour. This includes
the usual `String`, `Bool`, `Int`, `Double` and `Char` literals, including unicode literals, but also user-custom data types which we will be presented in a separate chapter.

```
val pi = 3.14
val int = 42 * 2
val bool = true && false
val str = "hello" ++ ", " ++ "world"
val c = 'c'
val unicode = \u039E // Ξ
```

Most values can be printed with `println`, however, `println` is not defined for others, e.g. `Char` or user-defined datatypes. 

```effekt:repl
println(str)
```

Though, everything can be transformed into a string representation via the `inspect` function. This outputs a pretty-printed version of an compiler internal representation and is not meant to be used for actual "production" code.

```effekt:repl
inspect(unicode)
```
