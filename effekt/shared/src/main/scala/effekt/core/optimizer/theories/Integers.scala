package effekt.core.optimizer.theories

import effekt.core.Block.BlockVar
import effekt.core.{Expr, Id, Type}

/**
 * Theory for integers with neutral variables: multivariate Laurent polynomials with 64-bit signed integer coefficients.
 *
 * KNOWN LIMITATION: This implementation assumes 64-bit signed integers.
 * Unfortunately, this is unsound for the JavaScript backend, which uses JavaScript numbers that are IEEE-754 doubles.
 */
object Integers {
  case class Integer(value: Long, addends: Addends) {
    val free: Set[Id] = addends.flatMap { case (factors, _) => factors.keys }.toSet

    def show: String = {
      val Integer(v, a) = this
      val terms = a.map { case (factors, n) =>
        val factorStr = if (factors.isEmpty) "1" else {
          factors.map { case (id, exp) =>
            if (exp == 1) s"${id.name}"
            else s"${id.name}^$exp"
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

  def embed(value: Long): Integers.Integer = Integer(value, Map.empty)
  def embed(id: Id): Integers.Integer = Integer(0, Map(Map(id -> 1) -> 1))

  def reify(value: Integer, embedBuiltinName: String => BlockVar): Expr = Reify(embedBuiltinName).reify(value)

  // 3 * x * x / y   =  Addend(3, Map(x -> 2, y -> -1))
  type Addends = Map[Factors, Long]
  type Factors = Map[Id, Int]

  def normalize(n: Integer): Integer = normalized(n.value, n.addends)

  def normalized(value: Long, addends: Addends): Integer =
    val (const, norm) = normalizeAddends(addends)
    Integer(value + const, norm)

  def add(l: Integer, r: Integer): Integer = (l, r) match {
    // 2 + (3 * x)  +   4 + (5 * y)   =    6 + (3 * x) + (5 * y)
    case (Integer(x, xs), Integer(y, ys)) =>
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

  def neg(l: Integer): Integer = mul(l, -1)

  // (42 + 3*x + y) - (42 + 3*x + y) =  (42 + 3*x + y) + (-1*42 + -1*3*x + -1*y)
  def sub(l: Integer, r: Integer): Integer =
    add(l, neg(r))

  def mul(l: Integer, factor: Long): Integer = l match {
    case Integer(value, addends) =>
      Integer(value * factor, addends.map { case (f, n) => f -> n * factor })
  }

  def mul(l: Integer, factor: Factors): Integer = l match {
    case Integer(value, addends) =>
      Integer(0, Map(factor -> value) ++ addends.map { case (f, n) =>
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
    f.filterNot { case (id, exp) => exp == 0 }

  // (42 + 3*x + y) * (42 + 3*x + y)
  //    =
  // (42 + 3*x + y) * 42   +  (42 + 3*x + y) * 3*x   +    (42 + 3*x + y) * y
  def mul(l: Integer, r: Integer): Integer = r match {
    case Integer(y, ys) =>
      var sum: Integer = mul(l, y)
      ys.foreach { case (f, n) => sum = add(sum, mul(mul(l, n), f)) }
      normalize(sum)
  }

  case class Reify(embedBuiltinName: String => BlockVar) {
    def reifyVar(id: Id): Expr = Expr.ValueVar(id, Type.TInt)

    def reifyInt(v: Long): Expr = Expr.Literal(v, Type.TInt)

    def reifyOp(l: Expr, op: Operation, r: Expr): Expr = op match {
      case Add => Expr.PureApp(embedBuiltinName("effekt::infixAdd(Int, Int)"), List(), List(l, r))
      case Sub => Expr.PureApp(embedBuiltinName("effekt::infixSub(Int, Int)"), List(), List(l, r))
      case Mul => Expr.PureApp(embedBuiltinName("effekt::infixMul(Int, Int)"), List(), List(l, r))
      case Div => Expr.PureApp(embedBuiltinName("effekt::infixDiv(Int, Int)"), List(), List(l, r))
    }

    def reify(v: Integer): Expr =
      val Integer(const, addends) = normalize(v)

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

    def reifyFactor(x: Id, n: Int): Expr =
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
