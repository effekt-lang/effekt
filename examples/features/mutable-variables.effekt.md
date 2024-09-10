---
layout: docs
title: Variables
permalink: docs/tutorial/variables
---

# Mutable Variables

Like in many other languages, there are also mutable variables in Effekt. Whereas values are defined using the `val` keyword, mutable variables use the `var` keyword.

```
def sum(xs: List[Int]): Int = {
  var sum = 0
  xs.foreach { x =>
    sum = sum + x
  }
  sum
}
```

It is not permitted to have unintialized variables, that is, there are no mere declarations of variables. Therefore, the following is not allowed:

```effekt:repl
var x: Int
```