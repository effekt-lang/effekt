---
layout: docs
title: Automatic Differentiation
permalink: docs/casestudies/ad
---

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
def mathExp(d: Double): Double = exp(d)

effect AD[Num] {
  def num(x: Double): Num
  def add(x: Num, y: Num): Num
  def mul(x: Num, y: Num): Num
  def exp(x: Num): Num
}
```
If Effekt would support polymorphic effects (or abstract type members on
effects) we could abstract over the domain `Num`, which is hard-coded above to
contain a double value and a mutable reference.

We can use the number DSL to express two example programs.
```
// d = 3 + 3x^2
def prog[Num](x: Num): Num / AD[Num] =
  add(mul(num(3.0), x), mul(mul(x, x), x))

// d = exp(1 + 2x) + 2x*exp(x^2)
def progExp[Num](x: Num): Num / AD[Num] =
  add(mul(num(0.5), exp(add(num(1.0), mul(num(2.0), x)))), exp(mul(x, x)))
```
These programs use effect operations for multiplication, addition, the exponential function, and for embedding
constant literals.

## Forwards Propagation
To compute the derivative of an expression like `prog`, we start with
forwards propagation. A differentiable function has type `Num => Num / AD`
and computing the derivative is expressed as a handler.

For forwards propagation, we do not use the fact that the derivative stored
in `Num` is a mutable box. This is only necessary for backwards propagation.
```
record NumF(value: Double, d: Double)
def forwards(in: Double) { prog: NumF => NumF / AD[NumF] }: Double =
  try { prog(NumF(in, 1.0)).d } with AD[NumF] {
    def num(v)    = resume(NumF(v, 0.0))
    def add(x, y) = resume(NumF(x.value + y.value, x.d + y.d))
    def mul(x, y) = resume(NumF(x.value * y.value, x.d * y.value + y.d * x.value))
    def exp(x) = resume(NumF(mathExp(x.value), x.d * mathExp(x.value)))
  }


record NumH[N](value: N, d: N)
def forwardsHigher[N](in: N) { prog: NumH[N] => NumH[N] / AD[NumH[N]] }: N / AD[N] =
  try { prog(NumH(in, num(1.0))).d } with AD[NumH[N]] {
    def num(v)    = resume(NumH(num(v), num(0.0)))
    def add(x, y) = resume(NumH(add(x.value, y.value), add(x.d, y.d)))
    def mul(x, y) = resume(NumH(mul(x.value, y.value), add(mul(x.d, y.value), mul(y.d, x.value))))
    def exp(x) = resume(NumH(exp(x.value), mul(x.d, exp(x.value))))
  }

def showString { prog: String => String / AD[String] } =
  try { prog("x") } with AD[String] {
    def num(v)    = resume(v.show)
    def add(x, y) =
      if (x == "0") resume(y) else if (y == "0") resume(x) else resume("(" ++ x ++ " + " ++ y ++ ")")
    def mul(x, y) =
      if (x == "0" || y == "0") { resume("0") } else if (x == "1") { resume(y) } else { resume("(" ++ x ++ " * " ++ y ++ ")") }
    def exp(x) = resume("exp(" ++ x ++ ")")
  }
```
Except for the wrapping and unwrapping of the references, the definition
of `add`, `mul` and `exp` are exactly the ones we would expect from a text book.

## Backwards Propagation
We can use the same differentiable expression and compute its derivative
by using _backwards propagation_. Since we modeled the DSL as an effect,
we automatically get access to the continuation in the implementation of
`add`, `mul` and `exp`. We thus do not have to use `shift` and `reset`.
Otherwise the implementation closely follows the one by Wang et al.
```
record NumB(value: Double, d: Ref[Double])
def backwards(in: Double) { prog: NumB => NumB / AD[NumB] }: Double = {
  // the representation of our input
  val input = NumB(in, fresh(0.0))

  // a helper function to update the derivative of a given number by adding v
  def push(n: NumB)(v: Double): Unit = n.d.put(n.d.get + v)

  try { prog(input).push(1.0) } with AD[NumB] {
    def num(v) = resume(NumB(v, fresh(0.0)))
    def add(x, y) = {
      val z = NumB(x.value + y.value, fresh(0.0))
      resume(z)
      x.push(z.d.get);
      y.push(z.d.get)
    }
    def mul(x, y) = {
      val z = NumB(x.value * y.value, fresh(0.0))
      resume(z)
      x.push(y.value * z.d.get);
      y.push(x.value * z.d.get)
    }
    def exp(x) = {
      val z = NumB(mathExp(x.value), fresh(0.0))
      resume(z)
      x.push(mathExp(x.value) * z.d.get)
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


  println(forwards(1.0) { x => progExp(x) })
  println(backwards(1.0) { x => progExp(x) })

  println(forwards(0.0) { x => progExp(x) })
  println(backwards(0.0) { x => progExp(x) })

  println(showString { x => forwardsHigher(x) { x => forwardsHigher(x) { y => forwardsHigher(y) { z => prog(z) } }} })

  // we have the same pertubation confusion as in Lantern
  val result = forwards(1.0) { x =>
    val shouldBeOne = forwards(1.0) { y => add(x, y) }
    val z = num[NumF](shouldBeOne)
    mul(x, z)
  }
  println(result)

  val result2 = backwards(1.0) { x =>
    val shouldBeOne = backwards(1.0) { y => add(x, y) }
    val z = num[NumB](shouldBeOne)
    mul(x, z)
  }
  println(result2)

  // this is proposed by Wang et al. as a solution to pertubation confusion
  val result3 = backwards(1.0) { x =>
    val shouldBeOne = forwards(1.0) { y => add(num(x.value), y) }
    mul(x, num(shouldBeOne))
  }
  println(result3)
}
```
