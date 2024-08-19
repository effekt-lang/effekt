# Loops

Effekt includes a (completely standard) built-in `while` loop expression as its sole native looping construct.


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

Additionally, the standard library also includes the fucntions `each` and `repeat`.

```effekt:repl
each(1, 11) { n => if (n.mod(2) == 0) println(n) else do continue() }
```

`each` expects two value arguments: an inclusive starting index and an exclusive end index. At each iteration, the current index is passed to the block argument of `each`.
You may also use the operations `break` and `continue` of the effect `Control` within this block argument.

```effekt:repl
repeat(2) { println("Effekt!") }
```

`repeat` just expects one value argument that controls, how often the passed block argument is to be executed. You may also use the operations of the `Control` effect here.
