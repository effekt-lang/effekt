package effekt.core.optimizer.normalizer.theories

import effekt.core.Block.BlockVar
import effekt.core.{Expr, Id, Type}
import effekt.core.optimizer.normalizer.semantics.Neutral

/**
 * Theory for integers with neutral variables: multivariate Laurent polynomials with 64-bit signed integer coefficients.
 * Compare https://en.wikipedia.org/wiki/Laurent_polynomial
 *
 * KNOWN LIMITATION: This implementation assumes 64-bit signed integers.
 * Unfortunately, this is unsound for the JavaScript backend, which uses JavaScript numbers that are IEEE-754 doubles.
 */
object integers {
  case class IntegerRep(value: Long, addends: Addends) {
    val free: Set[Id] = addends.flatMap { case (factors, _) => factors.keys.flatMap(_.free) }.toSet
    def isLiteral: Boolean = addends.isEmpty
    def show: String = {
      val IntegerRep(v, a) = this
      val terms = a.map { case (factors, n) =>
        val factorStr = if (factors.isEmpty) "1" else {
          factors.map { case (n, exp) =>
            if (exp == 1) "<neutral>"
            else s"<neutral>^$exp"
          }.mkString("*")
        }
        if (n == 1) s"$factorStr"
        else s"$n*$factorStr"
      }.toList

      val constPart = if (v != 0) List(v.toString) else Nil
      (constPart ++ terms).mkString(" + ")
    }
  }

  enum Operation {
    case Add, Sub, Mul, Div
  }
  import Operation._

  def embed(value: Long): integers.IntegerRep = IntegerRep(value, Map.empty)
  def embed(n: Neutral): integers.IntegerRep = IntegerRep(0, Map(Map(n -> 1) -> 1))

  def reify(value: IntegerRep, embedBuiltinName: String => BlockVar, embedNeutral: Neutral => Expr): Expr =
    Reify(embedBuiltinName, embedNeutral).reify(value)

  // 3 * x * x / y   =  Addend(3, Map(x -> 2, y -> -1))
  type Addends = Map[Factors, Long]
  type Factors = Map[Neutral, Int]

  def normalize(n: IntegerRep): IntegerRep = normalized(n.value, n.addends)

  def normalized(value: Long, addends: Addends): IntegerRep =
    val (const, norm) = normalizeAddends(addends)
    IntegerRep(value + const, norm)

  def add(l: IntegerRep, r: IntegerRep): IntegerRep = (l, r) match {
    // 2 + (3 * x)  +   4 + (5 * y)   =    6 + (3 * x) + (5 * y)
    case (IntegerRep(x, xs), IntegerRep(y, ys)) =>
      normalized(x + y, add(xs, ys))
  }

  def add(xs: Addends, ys: Addends): Addends = {
    var addends = xs
    ys.foreach { case (factors, n) =>
      val m: Long = addends.getOrElse(factors, 0)
      addends = addends.updated(factors, n + m)
    }
    addends
  }

  // 3 * x1^2  +  2 * EMPTY  + 0 * x2^3  =  2  +  3 * x1^2
  def normalizeAddends(xs: Addends): (Long, Addends) = {
    var constant: Long = 0
    var filtered: Addends = Map.empty
    xs.foreach { case (factors, n) =>
      if (factors.isEmpty) {
        constant += n
      }
      if (n != 0) {
        filtered = filtered.updated(factors, n)
      }
    }
    (constant, filtered)
  }

  def neg(l: IntegerRep): IntegerRep = mul(l, -1)

  // (42 + 3*x + y) - (42 + 3*x + y) =  (42 + 3*x + y) + (-1*42 + -1*3*x + -1*y)
  def sub(l: IntegerRep, r: IntegerRep): IntegerRep =
    add(l, neg(r))

  def mul(l: IntegerRep, factor: Long): IntegerRep = l match {
    case IntegerRep(value, addends) =>
      IntegerRep(value * factor, addends.map { case (f, n) => f -> n * factor })
  }

  def mul(l: IntegerRep, factor: Factors): IntegerRep = l match {
    case IntegerRep(value, addends) =>
      IntegerRep(0, Map(factor -> value) ++ addends.map { case (f, n) =>
        mul(f, factor) -> n
      })
  }

  // (x * x * y) * (x * y * z) = x^3 + y^2 + z^1
  def mul(l: Factors, r: Factors): Factors = {
    var factors = l
    r.foreach { case (f, n) =>
      val m = factors.getOrElse(f, 0)
      factors = factors.updated(f, n + m)
    }
    normalizeFactors(factors)
  }

  // x1^2 * x2^0 * x3^3   =   x1^2 * x3^3
  def normalizeFactors(f: Factors): Factors =
    f.filterNot { case (n, exp) => exp == 0 }

  // (42 + 3*x + y) * (42 + 3*x + y)
  //    =
  // (42 + 3*x + y) * 42   +  (42 + 3*x + y) * 3*x   +    (42 + 3*x + y) * y
  def mul(l: IntegerRep, r: IntegerRep): IntegerRep = r match {
    case IntegerRep(y, ys) =>
      var sum: IntegerRep = mul(l, y)
      ys.foreach { case (f, n) => sum = add(sum, mul(mul(l, n), f)) }
      normalize(sum)
  }

  case class Reify(embedBuiltinName: String => BlockVar, embedNeutral: Neutral => Expr) {
    def reifyVar(n: Neutral): Expr = embedNeutral(n)

    def reifyInt(v: Long): Expr = Expr.Literal(v, Type.TInt)

    def reifyOp(l: Expr, op: Operation, r: Expr): Expr = op match {
      case Add => Expr.PureApp(embedBuiltinName("effekt::infixAdd(Int, Int)"), List(), List(l, r))
      case Sub => Expr.PureApp(embedBuiltinName("effekt::infixSub(Int, Int)"), List(), List(l, r))
      case Mul => Expr.PureApp(embedBuiltinName("effekt::infixMul(Int, Int)"), List(), List(l, r))
      case Div => Expr.PureApp(embedBuiltinName("effekt::infixDiv(Int, Int)"), List(), List(l, r))
    }

    def reify(v: IntegerRep): Expr =
      val IntegerRep(const, addends) = normalize(v)

      val adds = addends.toList.map { case (factors, n) =>
        if (n == 1) reifyFactors(factors)
        else reifyOp(reifyInt(n), Mul, reifyFactors(factors))
      }.reduceOption { case (l, r) => reifyOp(l, Add, r) }

      adds.map { a =>
        if (const != 0) reifyOp(reifyInt(const), Add, a)
        else a
      }.getOrElse {
        reifyInt(const)
      }

    def reifyFactor(x: Neutral, n: Int): Expr =
      if (n <= 0) sys error "Should not happen"
      else if (n == 1) reifyVar(x)
      else reifyOp(reifyVar(x), Mul, reifyFactor(x, n - 1))

    def reifyFactors(ys: Factors): Expr = {
      val factors = ys.toList.filterNot { case (_, n) => n == 0 }
      val positive = factors.filter { case (_, n) => n > 0 }
      val negative = factors.filter { case (_, n) => n < 0 }

      val pos = positive.map { case (x, n) => reifyFactor(x, n) }.reduceOption { case (l, r) => reifyOp(l, Mul, r) }
      val neg = negative.map { case (x, n) => reifyFactor(x, n * -1) }.reduceOption { case (l, r) => reifyOp(l, Mul, r) }
      val numerator = pos.getOrElse {
        reifyInt(1)
      }

      neg.map { denominator =>
        reifyOp(numerator, Div, denominator)
      }.getOrElse {
        numerator
      }
    }
  }
}
