# Effects and handlers

Up until this point, all things shown are pretty standard and also known from many other languages. Effects and 
handlers, however, are not quite as well known but nonetheless are at the core of the Effekt language.

One particular handler and effect, though, is very wide spread and often integrated into the other languages themselve 
-- the exception effect. First, we declare the `Exception` effect:

```
interface Exception {
  def throw(msg: String): Nothing
}
```

Each effect is equiped with at least one operation, like `throw`. Operations offer a way of invoking an effect.

```
def div(a: Double, b: Double) = 
  if (b == 0.0) do throw("division by zero")
  else a / b
```

In the definition of `div`, we throw an exception when dividing by zero. The return type has purposefully been left 
out. In other languages such as Java, the return type would simply be `Double`. However, in Effekt the return type is 
actually `Double / { Exception }`. The right-hand side of the slash consists of a set of
effects the calling context needs to handle. Thus, they are requirements passed onto the calling context.

Whereas operations introduce effects, handlers offer a way of discharging them.

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

Notice how the return type of `unsafeDiv` changed in comparison with `div`. Since the `Exception` effect has been 
handled, there are no other effects (the empty set `{}`) the calling context needs to handle. One may easily mistake 
the absence of effects in the signature as proof for the purity of the function, however, this is not the case in 
Effekt as `unsafeDiv` obviously invokes the `Exception` effect.

Besides exceptions, we can also emulate other usefull mechanism with effects. For example generator functions using the 
`Yield` effect.

```
interface Yield[A] {
  def yield(x: A): Unit
}
```

When envoking the `yield` operation, we pass a value to be yield to the lexically nearest handler discharging the 
`Yield` effect. Recall our `fib` functions from earlier. We may also write it as a generator that runs infinitely while 
yielding each fibonacci number in the process.

````
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
        resume(()) // <- we resume the computation where yield was envoked
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
