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
Effekt as `unsafeDiv` obviously invokes an effect: it is merely contextually pure.

Now that we have seen this basic example, you may ask yourself why you would even use effects if they simply emulate 
exceptions? While this is partly true, that is, effects can be used for exceptions, there are also many other, more 
elaborate use-cases. Effects are in fact strictly more powerful since they can capture the state of a computation as a 
continuation and optionally resume this computation at a later point in time.

```
interface Flip {
  def flip(): Boolean
}
```

```
def imp(): List[Boolean] / Flip = { 
  val a = flip()
  val b = flip()
  [not(a) || b]
}
def truthTableImp(): List[Boolean] =
  try { 
    imp()
  } with Flip {
    def flip() = resume(true).append(resume(false))
  }
```

