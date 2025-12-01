package effekt
package core
package optimizer
package normalizer

type ~>[-A, +B] = PartialFunction[A, B]

type BuiltinImpl = List[semantics.Value] ~> semantics.Value

def builtin(name: String)(impl: List[semantics.Value] ~> semantics.Value): (String, BuiltinImpl) = name -> impl

type Builtins = Map[String, BuiltinImpl]

given Conversion[Long, semantics.Value] with
  def apply(n: Long): semantics.Value =
    semantics.Value.Integer(theories.integers.embed(n))

given Conversion[Boolean, semantics.Value] with
  def apply(b: Boolean): semantics.Value =
    semantics.Value.Literal(b, Type.TBoolean)

given Conversion[String, semantics.Value] with
  def apply(s: String): semantics.Value =
    semantics.Value.Literal(s, Type.TString)

given Conversion[Double, semantics.Value] with
  def apply(d: Double): semantics.Value =
    semantics.Value.Literal(d, Type.TDouble)

lazy val supportedBuiltins: Builtins = integers ++ doubles ++ booleans ++ strings ++ chars

lazy val integers: Builtins = Map(
  // Integer arithmetic operations with symbolic simplification support
  // ----------
  builtin("effekt::infixAdd(Int, Int)") {
    case As.IntRep(x) :: As.IntRep(y) :: Nil => semantics.Value.Integer(theories.integers.add(x, y))
  },
  builtin("effekt::infixSub(Int, Int)") {
    case As.IntRep(x) :: As.IntRep(y) :: Nil => semantics.Value.Integer(theories.integers.sub(x, y))
  },
  builtin("effekt::infixMul(Int, Int)") {
    case As.IntRep(x) :: As.IntRep(y) :: Nil => semantics.Value.Integer(theories.integers.mul(x, y))
  },
  builtin("effekt::infixEq(Int, Int)") {
    case As.IntRep(x) :: As.IntRep(y) :: Nil if x == y => true
    case As.Int(x) :: As.Int(y) :: Nil => x == y
  },
  // Integer arithmetic operations only evaluated for literals
  // ----------
  builtin("effekt::infixDiv(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil if y != 0 => x / y
  },
  builtin("effekt::mod(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil if y != 0 => x % y
  },
  builtin("effekt::bitwiseShl(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => x << y
  },
  builtin("effekt::bitwiseShr(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => x >> y
  },
  builtin("effekt::bitwiseAnd(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => x & y
  },
  builtin("effekt::bitwiseOr(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => x | y
  },
  builtin("effekt::bitwiseXor(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => x ^ y
  },
  // Comparison
  // ----------
  builtin("effekt::infixNeq(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => x != y
  },
  builtin("effekt::infixLt(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => x < y
  },
  builtin("effekt::infixGt(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => x > y
  },
  builtin("effekt::infixLte(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => x <= y
  },
  builtin("effekt::infixGte(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => x >= y
  },
  // Conversion
  // ----------
  builtin("effekt::toDouble(Int)") {
    case As.Int(x) :: Nil => x.toDouble
  },

  builtin("effekt::show(Int)") {
    case As.Int(n) :: Nil => n.toString
  },
)

lazy val doubles: Builtins = Map(
  // Arithmetic
  // ----------
  builtin("effekt::infixAdd(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => x + y
  },
  builtin("effekt::infixSub(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => x - y
  },
  builtin("effekt::infixMul(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => x * y
  },
  builtin("effekt::infixDiv(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => x / y
  },
  builtin("effekt::sqrt(Double)") {
    case As.Double(x) :: Nil => Math.sqrt(x)
  },
  builtin("effekt::exp(Double)") {
    case As.Double(x) :: Nil => Math.exp(x)
  },
  builtin("effekt::log(Double)") {
    case As.Double(x) :: Nil => Math.log(x)
  },
  builtin("effekt::cos(Double)") {
    case As.Double(x) :: Nil => Math.cos(x)
  },
  builtin("effekt::sin(Double)") {
    case As.Double(x) :: Nil => Math.sin(x)
  },
  builtin("effekt::tan(Double)") {
    case As.Double(x) :: Nil => Math.tan(x)
  },
  builtin("effekt::atan(Double)") {
    case As.Double(x) :: Nil => Math.atan(x)
  },
  builtin("effekt::round(Double)") {
    case As.Double(x) :: Nil => Math.round(x)
  },
  builtin("effekt::pow(Double, Double)") {
    case As.Double(base) :: As.Double(exp) :: Nil => Math.pow(base, exp)
  },
  builtin("effekt::pi()") {
    case Nil => Math.PI
  },
  // Comparison
  // ----------
  builtin("effekt::infixEq(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => x == y
  },
  builtin("effekt::infixNeq(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => x != y
  },
  builtin("effekt::infixLt(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => x < y
  },
  builtin("effekt::infixGt(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => x > y
  },
  builtin("effekt::infixLte(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => x <= y
  },
  builtin("effekt::infixGte(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => x >= y
  },

  // Conversion
  // ----------
  builtin("effekt::toInt(Double)") {
    case As.Double(x) :: Nil => x.toLong
  },
  builtin("effekt::show(Double)") {
    // TODO globally define show on double in a decent way... this mimicks JS
    case As.Double(n) :: Nil =>
      if (n == n.toInt.toDouble) n.toInt.toString // Handle integers like 15.0 → 15
      else {
        val formatted = BigDecimal(n)
          .setScale(15, BigDecimal.RoundingMode.DOWN) // Truncate to 15 decimal places without rounding up
          .bigDecimal
          .stripTrailingZeros()
          .toPlainString

        formatted
      }
  },
)

lazy val booleans: Builtins = Map(
  builtin("effekt::not(Bool)") {
    case As.Bool(x) :: Nil => !x
  },
)

lazy val strings: Builtins = Map(
  builtin("effekt::infixConcat(String, String)") {
    case As.StringRep(x) :: As.StringRep(y) :: Nil => semantics.Value.String(theories.strings.concat(x, y))
  },

  builtin("effekt::infixEq(String, String)") {
    case As.String(x) :: As.String(y) :: Nil => x == y
  },

  builtin("effekt::length(String)") {
    case As.String(x) :: Nil => x.length.toLong
  },

  builtin("effekt::substring(String, Int, Int)") {
    case As.String(x) :: As.Int(from) :: As.Int(to) :: Nil => x.substring(from.toInt, to.toInt)
  },

  builtin("string::unsafeCharAt(String, Int)") {
    case As.String(x) :: As.Int(at) :: Nil => x.charAt(at.toInt).toLong
  },

  builtin("string::toInt(Char)") {
    case As.Int(n) :: Nil => n
  },

  builtin("string::toChar(Int)") {
    case As.Int(n) :: Nil => n
  },

  builtin("string::infixLte(Char, Char)") {
    case As.Int(x) :: As.Int(y) :: Nil => x <= y
  },

  builtin("string::infixLt(Char, Char)") {
    case As.Int(x) :: As.Int(y) :: Nil => x < y
  },

  builtin("string::infixGt(Char, Char)") {
    case As.Int(x) :: As.Int(y) :: Nil => x > y
  },

  builtin("string::infixGte(Char, Char)") {
    case As.Int(x) :: As.Int(y) :: Nil => x >= y
  },
)

lazy val chars: Builtins = Map(
  builtin("effekt::infixEq(Char, Char)") {
    case As.Int(x) :: As.Int(y) :: Nil => x == y
  },
)

protected object As {
  object Int {
    def unapply(v: semantics.Value): Option[scala.Long] = v match {
      case semantics.Value.Literal(value: scala.Long, _) => Some(value)
      case semantics.Value.Literal(value: scala.Int, _) => Some(value.toLong)
      case semantics.Value.Literal(value: java.lang.Integer, _) => Some(value.toLong)
      case semantics.Value.Integer(value) if value.isLiteral => Some(value.value)
      case _ => None
    }
  }

  object IntRep {
    def unapply(v: semantics.Value): Option[theories.integers.IntegerRep] = v match {
      // Integer literals not yet embedded into the theory of integers
      case semantics.Value.Literal(value: scala.Long, _) => Some(theories.integers.embed(value))
      case semantics.Value.Literal(value: scala.Int, _) => Some(theories.integers.embed(value.toLong))
      case semantics.Value.Literal(value: java.lang.Integer, _) => Some(theories.integers.embed(value.toLong))
      // Neutrals (e.g. variables or extern calls)
      case n: semantics.Neutral => Some(theories.integers.embed(n))
      // Already embedded integers
      case semantics.Value.Integer(value) => Some(value)
      case _ => None
    }
  }

  object Double {
    def unapply(v: semantics.Value): Option[scala.Double] = v match {
      case semantics.Value.Literal(value: scala.Double, _) => Some(value)
      case _ => None
    }
  }

  object String {
    def unapply(v: semantics.Value): Option[java.lang.String] = v match {
      case semantics.Value.Literal(value: java.lang.String, _) => Some(value)
      case semantics.Value.String(value) if value.isLiteral => Some(value.value.head.asInstanceOf[java.lang.String])
      case _ => None
    }
  }

  object StringRep {
    def unapply(v: semantics.Value): Option[theories.strings.StringRep] = v match {
      case semantics.Value.Literal(value: java.lang.String, _) => Some(theories.strings.embed(value))
      case n: semantics.Neutral => Some(theories.strings.embed(n))
      case semantics.Value.String(value) => Some(value)
      case _ => None
    }
  }

  object Bool {
    def unapply(v: semantics.Value): Option[scala.Boolean] = v match {
      case semantics.Value.Literal(value: scala.Boolean, _) => Some(value)
      case _ => None
    }
  }
}
