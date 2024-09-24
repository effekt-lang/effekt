---
layout: docs
title: Effects
permalink: docs/tutorial/effects
---

# Effects and handlers

TODO mention effect safety

## Non-resumptive effects

For getting started with effects and handlers, we define the popular exception effect, consisting just of one effect operation `throw`:

```
interface Exception {
  def throw(msg: String): Nothing
}
```

Each effect declaration starts with the `interface` keyword, followed by the name of the effect. Enclosed in curly braces, effect operations belonging to this effect are declared with their signature.

For envoking an effect, the respective effect operations can be called with the usual function call syntax, prepended by `do`:

```
def div(a: Double, b: Double) = 
  if (b == 0.0) { do throw("division by zero") }
  else { a / b }
```

The return type of non-recursive functions can be left out and is inferred. In this it is `Double / { Exception }`, that is, the return type `Double` followed by a slash and the effects that need to be handled by the calling context.

Whereas operations introduce effects, handlers offer a way of discharging them:

```
def unsafeDiv(a: Double, b: Double): Double / {} =
  try {
    div(a, b)
  } with Exception {
    def throw(msg) = {
      panic(msg)
    }
  }
```

```effekt:repl
unsafeDiv(42, 0)
```

Each handler consists of a `try` block, followed by one one more handlers, starting with `with`, followed by the name of the effect and definitions for each of the effect's declared operations.

## Resumptive effects

Besides exceptions, we can also emulate other useful mechanism with effects. For example generator functions using the 
`Yield` effect.

```
interface Yield[A] {
  def yield(x: A): Unit
}
```

When envoking the `yield` operation, we pass a value to be yield to the lexically nearest handler discharging the 
`Yield` effect. Recall our `fib` functions from earlier. We may also write it as a generator that runs infinitely while 
yielding each fibonacci number in the process.

```
// Fibonacci sequence as a generator function
def fib(): Unit / { Yield[Int] } = {
  def inner(a: Int, b: Int): Unit / { Yield[Int] } = {
    do yield(a)
    inner(b, a + b)
  }
  inner(0, 1)
}

// generate all fibonacci numbers up until the given limit.
def genFibs(limit: Int): List[Int] / {} = {
  var iters = 0
  var fibs = []
  try {
    fib()
  } with Yield[Int] {
    def yield(x) = 
      if (iters < limit) {
        iters = iters + 1
        fibs = Cons(x, fibs)
        resume(()) // <- we resume the computation where yield was invoked
      } else {
        ()
      }
  }
  fibs
}
```

```effekt:repl
genFibs(15)
```

## Singleton operation

Often you want to declare an interface that is entirely defined by just one operation. In this case you can declare it as a singleton operation:

```effekt:hide
extern def time(): Int =
  js "Date.now()"
```

```
effect tell(): Int
```

The handler for this interface uses a slightly different more concise syntax like it is used in the literature:

```
def tellTime[A] { prog: () => A / tell }: A =
  try { prog() }
  with tell { () =>
    resume(time())
  }
```

```effekt:repl
tellTime {
  val start = do tell()
  val end = do tell()
  println("took " ++ show(end - start))
}
```

## `with` handler

```
def limited[A](n: Int) { prog: () => Unit / Yield[A] }: List[A] = {
  var buff: List[A] = []
  var yields = 0
  try { 
    prog()
    buff
  } with Yield[A] {
    def yield(x) =
      if (yields >= n) { buff }
      else {
        buff = Cons(x, buff)
        yields = yields + 1
        resume(())
      }
  }
}

def run() = {
  with limited[Int](1)
  do yield(1)
  do yield(2)
}
```

```effekt:repl
run()
```