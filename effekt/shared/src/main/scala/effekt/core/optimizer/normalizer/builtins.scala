package effekt
package core
package optimizer
package normalizer

type ~>[-A, +B] = PartialFunction[A, B]

type BuiltinImpl = List[semantics.Value] ~> semantics.Value

def builtin(name: String)(impl: List[semantics.Value] ~> semantics.Value): (String, BuiltinImpl) = name -> impl

type Builtins = Map[String, BuiltinImpl]

lazy val integers: Builtins = Map(
  // Integer arithmetic operations with symbolic simplification support
  // ----------
  builtin("effekt::infixAdd(Int, Int)") {
    case As.IntExpr(x) :: As.IntExpr(y) :: Nil => semantics.Value.Integer(theories.Integers.add(x, y))
  },
  builtin("effekt::infixSub(Int, Int)") {
    case As.IntExpr(x) :: As.IntExpr(y) :: Nil => semantics.Value.Integer(theories.Integers.sub(x, y))
  },
  builtin("effekt::infixMul(Int, Int)") {
    case As.IntExpr(x) :: As.IntExpr(y) :: Nil => semantics.Value.Integer(theories.Integers.mul(x, y))
  },
  // Integer arithmetic operations only evaluated for literals
  // ----------
  builtin("effekt::infixDiv(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil if y != 0 => semantics.Value.Integer(theories.Integers.embed(x / y))
  },
  builtin("effekt::mod(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil if y != 0 => semantics.Value.Integer(theories.Integers.embed(x % y))
  },
  builtin("effekt::bitwiseShl(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Integer(theories.Integers.embed(x << y))
  },
  builtin("effekt::bitwiseShr(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Integer(theories.Integers.embed(x >> y))
  },
  builtin("effekt::bitwiseAnd(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Integer(theories.Integers.embed(x & y))
  },
  builtin("effekt::bitwiseOr(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Integer(theories.Integers.embed(x | y))
  },
  builtin("effekt::bitwiseXor(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Integer(theories.Integers.embed(x ^ y))
  },
  // Comparison
  // ----------
  builtin("effekt::infixEq(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Literal(x == y, Type.TBoolean)
  },
  builtin("effekt::infixNeq(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Literal(x != y, Type.TBoolean)
  },
  builtin("effekt::infixLt(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Literal(x < y, Type.TBoolean)
  },
  builtin("effekt::infixGt(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Literal(x > y, Type.TBoolean)
  },
  builtin("effekt::infixLte(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Literal(x <= y, Type.TBoolean)
  },
  builtin("effekt::infixGte(Int, Int)") {
    case As.Int(x) :: As.Int(y) :: Nil => semantics.Value.Literal(x >= y, Type.TBoolean)
  },
  // Conversion
  // ----------
  builtin("effekt::toDouble(Int)") {
    case As.Int(x) :: Nil => semantics.Value.Literal(x.toDouble, Type.TDouble)
  },

  builtin("effekt::show(Int)") {
    case As.Int(n) :: Nil => semantics.Value.Literal(n.toString, Type.TString)
  },
)

lazy val supportedBuiltins: Builtins = integers

protected object As {
  object Int {
    def unapply(v: semantics.Value): Option[scala.Long] = v match {
      case semantics.Value.Literal(value: scala.Long, _) => Some(value)
      case semantics.Value.Literal(value: scala.Int, _) => Some(value.toLong)
      case semantics.Value.Literal(value: java.lang.Integer, _) => Some(value.toLong)
      case _ => None
    }
  }

  object IntExpr {
    def unapply(v: semantics.Value): Option[theories.Integers.Integer] = v match {
      // Integer literals not yet embedded into the theory of integers
      case semantics.Value.Literal(value: scala.Long, _) => Some(theories.Integers.embed(value))
      case semantics.Value.Literal(value: scala.Int, _) => Some(theories.Integers.embed(value.toLong))
      case semantics.Value.Literal(value: java.lang.Integer, _) => Some(theories.Integers.embed(value.toLong))
      // Variables of type integer
      case semantics.Value.Var(id, tpe) if tpe == Type.TInt => Some(theories.Integers.embed(id))
      // Already embedded integers
      case semantics.Value.Integer(value) => Some(value)
      case _ => None
    }
  }
}