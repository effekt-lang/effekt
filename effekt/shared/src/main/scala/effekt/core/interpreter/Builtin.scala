package effekt
package core
package interpreter


case class Builtin(name: String, impl: List[Value] ~> Value)

def builtin(name: String)(impl: List[Value] ~> Value): (String, Builtin) = name -> Builtin(name, impl)

val printing = Map(
  builtin("effekt::println(String)") {
    case As.String(msg) :: Nil =>
      println(msg);
      Value.Unit()
  },
)

val doubles = Map(
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
)

val integers = Map(
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

val booleans = Map(
  builtin("effekt::not(Bool)") {
    case As.Bool(x) :: Nil => Value.Bool(!x)
  },
)

val strings = Map(
  builtin("effekt::infixConcat(String, String)") {
    case As.String(x) :: As.String(y) :: Nil => Value.String(x + y)
  },

  builtin("effekt::inspect(Any)") {
    case any :: Nil => Value.String(inspect(any))
  },
)

val arrays = Map(
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

val refs = Map(
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

val builtins = printing ++ integers ++ doubles ++ strings ++ arrays ++ refs

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
}
