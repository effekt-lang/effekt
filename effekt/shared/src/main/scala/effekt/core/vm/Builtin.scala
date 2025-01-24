package effekt
package core
package vm

import java.io.PrintStream
import scala.util.matching as regex
import scala.util.matching.Regex

trait Runtime {
  def out: PrintStream
}
def Runtime(using run: Runtime) = run

case class Builtin(name: String, impl: Runtime => List[Value] ~> Value)

def builtin(name: String)(impl: Runtime ?=> List[Value] ~> Value): (String, Builtin) =
  name -> Builtin(name, env => impl(using env))

type Builtins = Map[String, Builtin]

lazy val printing: Builtins = Map(
  builtin("effekt::println(String)") {
    case As.String(msg) :: Nil =>
      Runtime.out.println(msg)
      Value.Unit()
  },
)

lazy val doubles: Builtins = Map(
  // Arithmetic
  // ----------
  builtin("effekt::infixAdd(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => Value.Double(x + y)
  },
  builtin("effekt::infixSub(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => Value.Double(x - y)
  },
  builtin("effekt::infixMul(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => Value.Double(x * y)
  },
  builtin("effekt::infixDiv(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => Value.Double(x / y)
  },
  builtin("effekt::sqrt(Double)") {
    case As.Double(x) :: Nil => Value.Double(Math.sqrt(x))
  },
  builtin("effekt::exp(Double)") {
    case As.Double(x) :: Nil => Value.Double(Math.exp(x))
  },
  builtin("effekt::log(Double)") {
    case As.Double(x) :: Nil => Value.Double(Math.log(x))
  },
  builtin("effekt::cos(Double)") {
    case As.Double(x) :: Nil => Value.Double(Math.cos(x))
  },
  builtin("effekt::sin(Double)") {
    case As.Double(x) :: Nil => Value.Double(Math.sin(x))
  },
  builtin("effekt::tan(Double)") {
    case As.Double(x) :: Nil => Value.Double(Math.tan(x))
  },
  builtin("effekt::atan(Double)") {
    case As.Double(x) :: Nil => Value.Double(Math.atan(x))
  },
  builtin("effekt::round(Double)") {
    case As.Double(x) :: Nil => Value.Int(Math.round(x))
  },
  builtin("effekt::pow(Double, Double)") {
    case As.Double(base) :: As.Double(exp) :: Nil => Value.Double(Math.pow(base, exp))
  },
  builtin("effekt::pi()") {
    case Nil => Value.Double(Math.PI)
  },


  // Comparison
  // ----------
  builtin("effekt::infixEq(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => Value.Bool(x == y)
  },
  builtin("effekt::infixNeq(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => Value.Bool(x != y)
  },
  builtin("effekt::infixLt(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => Value.Bool(x < y)
  },
  builtin("effekt::infixGt(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => Value.Bool(x > y)
  },
  builtin("effekt::infixLte(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => Value.Bool(x <= y)
  },
  builtin("effekt::infixGte(Double, Double)") {
    case As.Double(x) :: As.Double(y) :: Nil => Value.Bool(x >= y)
  },

  // Conversion
  // ----------
  builtin("effekt::toInt(Double)") {
    case As.Double(x) :: Nil => Value.Int(x.toLong)
  },
  builtin("effekt::show(Double)") {
    // TODO globally define show on double in a decent way... this mimicks JS
    case As.Double(n) :: Nil =>
      Value.String(
        if (n == n.toInt.toDouble) n.toInt.toString // Handle integers like 15.0 → 15
        else {
          val formatted = BigDecimal(n)
            .setScale(15, BigDecimal.RoundingMode.DOWN) // Truncate to 15 decimal places without rounding up
            .bigDecimal
            .stripTrailingZeros()
            .toPlainString

          formatted
        }
      )
  },
)

lazy val integers: Builtins = Map(
  // Arithmetic
  // ----------
  builtin("effekt::infixAdd(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Int(x + y)
  },
  builtin("effekt::infixSub(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Int(x - y)
  },
  builtin("effekt::infixMul(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Int(x * y)
  },
  builtin("effekt::mod(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Int(x % y)
  },
  builtin("effekt::infixDiv(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Int(x / y)
  },
  builtin("effekt::bitwiseShl(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Int(x << y)
  },
  builtin("effekt::bitwiseShr(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Int(x >> y)
  },
  builtin("effekt::bitwiseAnd(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Int(x & y)
  },
  builtin("effekt::bitwiseOr(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Int(x | y)
  },
  builtin("effekt::bitwiseXor(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Int(x ^ y)
  },

  // Comparison
  // ----------
  builtin("effekt::infixEq(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x == y)
  },
  builtin("effekt::infixNeq(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x != y)
  },
  builtin("effekt::infixLt(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x < y)
  },
  builtin("effekt::infixGt(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x > y)
  },
  builtin("effekt::infixLte(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x <= y)
  },
  builtin("effekt::infixGte(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x >= y)
  },

  // Conversion
  // ----------
  builtin("effekt::toDouble(Int)") {
    case As.Int(x) :: Nil => Value.Double(x.toDouble)
  },

  builtin("effekt::show(Int)") {
    case As.Int(n) :: Nil => Value.String(n.toString)
  },
)

lazy val booleans: Builtins = Map(
  builtin("effekt::not(Bool)") {
    case As.Bool(x) :: Nil => Value.Bool(!x)
  },
)

lazy val strings: Builtins = Map(
  builtin("effekt::infixConcat(String, String)") {
    case As.String(x) :: As.String(y) :: Nil => Value.String(x + y)
  },

  builtin("effekt::inspect(Any)") {
    case any :: Nil =>
      Runtime.out.println(inspect(any))
      Value.Unit()
  },

  builtin("effekt::infixEq(String, String)") {
    case As.String(x) :: As.String(y) :: Nil => Value.Bool(x == y)
  },

  builtin("effekt::length(String)") {
    case As.String(x) :: Nil => Value.Int(x.length)
  },

  builtin("effekt::substring(String, Int, Int)") {
    case As.String(x) :: As.Int(from) :: As.Int(to) :: Nil => Value.String(x.substring(from.toInt, to.toInt))
  },

  builtin("string::unsafeCharAt(String, Int)") {
    case As.String(x) :: As.Int(at) :: Nil => Value.Int(x.charAt(at.toInt).toLong)
  },

  builtin("string::toInt(Char)") {
    case As.Int(n) :: Nil => Value.Int(n)
  },

  builtin("string::toChar(Int)") {
    case As.Int(n) :: Nil => Value.Int(n)
  },

  builtin("string::infixLte(Char, Char)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x <= y)
  },

  builtin("string::infixLt(Char, Char)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x < y)
  },

  builtin("string::infixGt(Char, Char)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x > y)
  },

  builtin("string::infixGte(Char, Char)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x >= y)
  },
)

lazy val chars: Builtins = Map(
  builtin("effekt::infixEq(Char, Char)") {
    case As.Int(x) :: As.Int(y) :: Nil => Value.Bool(x == y)
  },
)

lazy val arrays: Builtins = Map(
  builtin("array::allocate(Int)") {
    case As.Int(x) :: Nil => Value.Array(scala.Array.ofDim(x.toInt))
  },
  builtin("array::size[T](Array[T])") {
    case As.Array(arr) :: Nil => Value.Int(arr.length.toLong)
  },
  builtin("array::unsafeGet[T](Array[T], Int)") {
    case As.Array(arr) :: As.Int(index) :: Nil => arr(index.toInt)
  },
  builtin("array::unsafeSet[T](Array[T], Int, T)") {
    case As.Array(arr) :: As.Int(index) :: value :: Nil => arr.update(index.toInt, value); Value.Unit()
  },
)

lazy val undefined: Builtins = Map(
  builtin("effekt::isUndefined[A](A)") {
    case Value.Literal(m) :: Nil => Value.Bool(m == null)
  },
)

lazy val refs: Builtins = Map(
  builtin("ref::ref[T](T)") {
    case init :: Nil => Value.Ref(Reference(init))
  },
  builtin("ref::get[T](Ref[T])") {
    case As.Reference(ref) :: Nil => ref.value
  },
  builtin("ref::set[T](Ref[T], T)") {
    case As.Reference(ref) :: value :: Nil => ref.value = value; Value.Unit()
  },
)

lazy val regexes: Builtins = Map(
  builtin("regex::regex(String)") {
    case As.String(str) :: Nil => Value.Literal(new Regex(str))
  },
  builtin("regex::exec(Regex, String)") {
    case As.Regex(r) :: As.String(str) :: Nil => Value.Literal(r.findFirstMatchIn(str).orNull)
  },
  builtin("regex::matched(RegexMatch)") {
    case As.RegexMatch(m) :: Nil => Value.String(m.matched)
  },
  builtin("regex::index(RegexMatch)") {
    case As.RegexMatch(m) :: Nil => Value.Int(m.start)
  },
)

lazy val builtins: Builtins = printing ++ integers ++ doubles ++ booleans ++ strings ++ arrays ++ refs ++ chars ++ regexes ++ undefined

protected object As {
  object String {
    def unapply(v: Value): Option[java.lang.String] = v match {
      case Value.Literal(value: java.lang.String) => Some(value)
      case _ => None
    }
  }
  object Int {
    def unapply(v: Value): Option[scala.Long] = v match {
      case Value.Literal(value: scala.Long) => Some(value)
      case Value.Literal(value: scala.Int) => Some(value.toLong)
      case Value.Literal(value: java.lang.Integer) => Some(value.toLong)
      case _ => None
    }
  }
  object Bool {
    def unapply(v: Value): Option[scala.Boolean] = v match {
      case Value.Literal(value: scala.Boolean) => Some(value)
      case _ => None
    }
  }
  object Double {
    def unapply(v: Value): Option[scala.Double] = v match {
      case Value.Literal(value: scala.Double) => Some(value)
      case _ => None
    }
  }
  object Array {
    def unapply(v: Value): Option[scala.Array[Value]] = v match {
      case Value.Array(array) => Some(array)
      case _ => None
    }
  }
  object Reference {
    def unapply(v: Value): Option[Reference] = v match {
      case Value.Ref(ref) => Some(ref)
      case _ => None
    }
  }
  object Regex {
    def unapply(v: Value): Option[regex.Regex] = v match {
      case Value.Literal(v: regex.Regex) => Some(v)
      case _ => None
    }
  }
  object RegexMatch {
    def unapply(v: Value): Option[regex.Regex.Match | Null] = v match {
      case Value.Literal(null) => Some(null)
      case Value.Literal(v: regex.Regex.Match) => Some(v)
      case _ => None
    }
  }
}
