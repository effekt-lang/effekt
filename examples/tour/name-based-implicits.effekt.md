---
title: Name-based implicits
permalink: tour/name-based-implicits
---

# Name-Based Implicit Parameters

Some functions need additional parameters that can be easily inferred based on
their name and type. For example, a sort function might want to get
a order as its argument:

```effekt
def mySort1[A](x: List[A]){ compare: (A, A) => Ordering }: List[A] =
  // we use the standard library sort here, which gets a less than or equal
  x.sortBy{ (x, y) =>
    compare(x,y) match {
      case Greater() => false
      case _ => true
    }
  }

def compare(x: Int, y: Int): Ordering = compareInt(x, y)
```
```effekt:repl
mySort1([3,1,2]){ (x, y) => compare(x, y) }
```

However, often there will be a function `compare` in scope, and we could
simplify the call if this was used automatically. In Effekt, we can
instruct the compiler to generate the block parameter like above by
putting a `?` in front of the parameter:
```
def mySort2[A](x: List[A]){ ?compare: (A, A) => Ordering }: List[A] =
  // we use the standard library sort here, which gets a less than or equal
  x.sortBy{ (x, y) =>
    compare(x,y) match {
      case Greater() => false
      case _ => true
    }
  }
```

Then we can just call it as:
```effekt:repl
mySort2([3,1,2])
```

This generates something like the following, which we are still allowed to write explicitly, too:
```effekt:repl
mySort2([3,1,2]){ (x, y) => compare(x, y) }
```

Name-based implicit parameters also will be passed
to the functions that get called implicitly like this, so if we define
```effekt
def compare[A](x: Option[A], y: Option[A]){ ?compare: (A,A) => Ordering }: Ordering =
  (x, y) match {
    case (None(), None()) => Equal()
    case (Some(_), None()) => Greater()
    case (None(), Some(_)) => Less()
    case (Some(x), Some(y)) => compare(x, y)
  }
```

we can now also call `mySort2` with lists of options (or options of options, ...):
```effekt:repl
mySort2([Some(12), None(), Some(2)])
```

This will expand to code equivalent to:
```effekt:repl
mySort2([Some(12), None(), Some(2)]){ (x,y) => compare(x, y){ (u, v) => compare(u, v) } }
```

## Values

We can also pass value parameters implicitly when they are marked with a `?`:

```effekt
def foo(?context: String): Unit =
  println(s"Called from ${context}")

def example1() = {
  val context = "example1"
  foo()
}

def example2(context: String) = {
  example1()
  foo()
}
```
```effekt:repl
example2("ex2")
```

That is, implicit value parameters use any value binding of the correct name
at the call site.
For some special cases, they work differently, though. We will discuss those now.

### Boxing

If the value parameter has a boxed type (something like `A => B at {}`),
this is expanded to `box {...}` and expanded like block parameters, so
the following works:
```
def mySort3[A](x: List[A], ?compare: (A, A) => Ordering at {}): List[A] =
  mySort2(x){ (x,y) => (unbox compare)(x, y) }
```
```effekt:repl
mySort3([12,3,42])
```

### Source Positions

When a function takes an implicit parameter with the name `sourcePosition`,
it will get the result of calling `SourcePosition` with the filename,
start line, start column, end line, and end column of the source position of the call:
```
def printSourcePos(?sourcePosition: SourcePosition): Unit =
  println(sourcePosition.show)
```
```effekt:repl
printSourcePos()
```

### Call IDs

Sometimes we would use source positions just to get a unique id for each
call in the source code. Instead, we can take a parameter `callId: Int`
which will get a unique integer ID directly:

```
def here(?callId: Int): Int = callId
```
```effekt:repl
here() == here()
```