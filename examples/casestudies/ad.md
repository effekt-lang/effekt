# Automatic Differentiation
In this case study we reimplement automatic differentiation as presented in

> Demystifying Differentiable Programming: Shift/Reset the Penultimate Backpropagator
> Fei Wang et al. ICFP 2019

Instead of using the control operators `shift` and `reset`, we are using effects and
handlers.
```
module examples/casestudies/ad

import immutable/list
import mutable/heap
```

## Representing Differentiable Expressions
Like Wang et al. we start by defining our own representation of differentiable
expressions. We represent the language of differentiable expressions by an
effect `AD`, which operates on `Num`. The effect `AD` can be thought of as an
embedded DSL.
```
record Num(value: Double, d: Ref[Double])

effect AD {
  def num(x: Double): Num
  def add(x: Num, y: Num): Num
  def mul(x: Num, y: Num): Num
}
```
If Effekt would support polymorphic effects (or abstract type members on
effects) we could abstract over the domain `Num`, which is hard-coded above to
contain a double value and a mutable reference.
We make use of operator overloading by defining `infixAdd` and `infixMul`, whch are
special-cased in the Effekt compiler.
```
def infixAdd(x: Num, y: Num) = add(x, y)
def infixMul(x: Num, y: Num) = mul(x, y)
```
We can use the number DSL to express an example program.
```
// d = 3 + 3x^2
def prog(x: Num): Num / AD =
  (num(3.0) * x) + (x * x * x)
```
This program uses effect operations for multiplication, addition, and for embedding
constant literals.

## Forwards Propagation
To compute the derivative of an expression like `prog`, we start with
forwards propagation. A differentiable function has type `Num => Num / AD`
and computing the derivative is expressed as a handler.

For forwards propagation, we do not use the fact that the derivative stored
in `Num` is a mutable box. This is only necessary for backwards propagation.
```
def forwards(in: Double) { prog: Num => Num / AD }: Double =
  try { prog(Num(in, fresh(1.0))).d.get } with AD {
    def num(v)    = resume(Num(v, fresh(0.0)))
    def add(x, y) = resume(Num(
      x.value + y.value,
      fresh(x.d.get + y.d.get)))
    def mul(x, y) = resume(Num(
      x.value * y.value,
      fresh(x.d.get * y.value + y.d.get * x.value)))
  }
```
Except for the wrapping and unwrapping of the references, the definition
of `add` and `mul` are exactly the ones we would expect from a text book.

## Backwards Propagation
We can use the same differentiable expression and compute its derivative
by using _backwards propagation_. Since we modeled the DSL as an effect,
we automatically get access to the continuation in the implementation of
`add` and `mul`. We thus do not have to use `shift` and `reset`.
Otherwise the implementation closely follows the one by Wang et al.
```
def backwards(in: Double) { prog: Num => Num / AD }: Double = {
  // the representation of our input
  val input = Num(in, fresh(0.0))

  // a helper function to update the derivative of a given number by adding v
  def push(n: Num)(v: Double): Unit = n.d.put(n.d.get + v)

  try { prog(input).push(1.0) } with AD {
    def num(v) = resume(Num(v, fresh(0.0)))
    def add(x, y) = {
      val z = Num(x.value + y.value, fresh(0.0))
      resume(z)
      x.push(z.d.get);
      y.push(z.d.get)
    }
    def mul(x, y) = {
      val z = Num(x.value * y.value, fresh(0.0))
      resume(z)
      x.push(y.value * z.d.get);
      y.push(x.value * z.d.get)
    }
  }
  // the derivative of `prog` at `in` is stored in the mutable reference
  input.d.get
}
```

## Example Usages
We can use forwards and backwards propagation to compute derivatives of a few
examples.
```
def main() = {
  println(forwards(2.0) { x => prog(x) })
  println(backwards(2.0) { x => prog(x) })

  println(forwards(3.0) { x => prog(x) })
  println(backwards(3.0) { x => prog(x) })

  println(forwards(0.0) { x => prog(x) })
  println(backwards(0.0) { x => prog(x) })


  // we have the same pertubation confusion as in Lantern
  val result = forwards(1.0) { x =>
    val shouldBeOne = forwards(1.0) { y => x + y }
    val z = num(shouldBeOne)
    x * z
  }
  println(result)

  val result2 = backwards(1.0) { x =>
    val shouldBeOne = backwards(1.0) { y => x + y }
    val z = num(shouldBeOne)
    x * z
  }
  println(result2)

  // this is proposed by Wang et al. as a solution to pertubation confusion
  val result3 = backwards(1.0) { x =>
    val shouldBeOne = forwards(1.0) { y => x + y }
    val z = num(shouldBeOne)
    x * z
  }
  println(result3)
}
```
