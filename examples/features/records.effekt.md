# Tuples & records

A tuple is written as comma-separated list delimited by parentheses:

```
val t = ("a", 42)
```

For destructing a tuple, you may use pattern matching:

```effekt:repl
t match { case (first, second) => first }
```

Before creating a record, you first need to declare it:

```
record Vec2d(x: Int, y: Int)
val vec = Vec2d(1, 2)
```

A record can either again be destructed by employing pattern matching

```effekt:repl
vec match { case Vec2d(a, b) => a }
```

or by using the declared field names:

```effekt:repl
vec.x
```
