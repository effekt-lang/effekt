---
layout: docs
title: Variables
permalink: tour/variables
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

It is not permitted to have uninitialized variables, that is, there are no mere declarations of variables. Therefore, the following is **not** allowed:

```effekt:sketch
def main() = {
  var x: Int; // ERROR: Expected = but got ;
  ()
}

```

At runtime, mutable variables are allocated on the stack -- see also [regions](./regions) and [captures](./captures).
