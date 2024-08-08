# Loops

In Effekt, there is only a `while` loop expression built-in (that is completely standard).

```
def fib(n: Int): Int = {
  var last = 0
  var current = 1
  var iters = 0
  while (iters < n) {
    val next = last + current
    last = current
    current = next
    iters = iters + 1
  } else { println("ran for " ++ show(iters) ++ " iterations") }
  last
}
```

```repl
fib(10)
```

A `while` loop consists a condition on which to continue the loop and an optional `else` branch that is run when the
`while` loop ended.

Furthermore, there is a `loop` function defined in the standard library, you can use for infinite, breakable loops.

```effekt:hidden
interface Event[A] {
  def receive(): Option[A]
}
record Job()
```

```
def worker() = loop {
  val packet = do receive[Job]()
  packet match {
    case Some(p) => println("processing request")
    case None => do break()
  }
}
```

The operations `break` and `continue` have their usual semantics, except that they are handled by `loop` as operations of the algebraic effect `Control`.
