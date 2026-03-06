---
title: Records
permalink: tour/records
---

# Tuples & records
Multiple values can be aggregated in a tuple or record.

A _tuple_ is written as comma-separated list delimited by parentheses:

```
val t: (String, Int) = ("a", 42)
```

For destructing a tuple, you may use pattern matching with `match`:

```effekt:repl
t match { case (first, second) => first }
```

or irrefutable pattern matching in the left-hand side of `val` bindings:

```effekt:sketch
val (first, second) = t
```

A _record_, on the other hand, is named and needs to be declared first:

```
record Vec2d(x: Int, y: Int)
val vec: Vec2d = Vec2d(1, 2)
```

Like in the case of tuples, records can be destructed by employing pattern matching:

```effekt:repl
vec match { case Vec2d(a, b) => a }
```

or by using irrefutable pattern matching in the left-hand side of `val` bindings:

```effekt:sketch
val Vec2d(a, b) = vec
println(a) // `a` and `b` are in scope here
```

or by using the declared field names as accessors:

```effekt:repl
vec.x
```
