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

For destructing a tuple, you may use pattern matching:

```effekt:repl
t match { case (first, second) => first }
```

```effekt:sketch
val (first, second) = t
```

A _record_, on the other hand, is named and needs to be declared first:

```
record Vec2d(x: Int, y: Int)
val vec: Vec2d = Vec2d(1, 2)
```
Again, a record can either again be destructed by employing pattern matching:

```effekt:repl
vec match { case Vec2d(a, b) => a }
```
or by using the declared field names as accessors:

```effekt:repl
vec.x
```
