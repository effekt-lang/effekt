package effekt
package core
package typed

import scala.annotation.targetName
import scala.reflect.ClassTag

// this is a draft...
// key observation: Stmt x Gamma == typed statement
//
// related post: https://brianmckenna.org/blog/type_annotation_cofree

enum Expr {
  case Literal(value: Any, annotatedTpe: ValueType)
  case ImpureApp(id: Id, args: List[Expr])
  case ValueVar(id: Id)
}

enum Stmt {
  case Val(id: Id, binding: Stmt, body: Stmt)
  case Return(expr: Expr)
}

case class Types(
  externs: Map[Id, BlockType.Function],
  values: Map[Id, ValueType]
) {
  def bind(id: Id, tpe: ValueType): Types = Types(externs, values.updated(id, tpe))
  def +(binding: (Id, ValueType)): Types = bind(binding._1, binding._2)

  def `:-`[T](value: T): Typed[T] = new `:-`(this, value)

  def lookup(id: Id): ValueType = values.getOrElse(id, sys error s"Cannot find type for ${id}")

  def extern(id: Id): BlockType.Function = externs.getOrElse(id, sys error s"Extern not found ${id}")

  def typeOf(stmt: Stmt): ValueType = (this :- stmt).tpe
  def typeOf(expr: Expr): ValueType = (this :- expr).tpe
}
object Types {
  val empty = Types(Map.empty, Map.empty)
}

// we Use :- instead of vdash to associate to the left
case class `:-`[+T](context: Types, extract: T)


type Typed[+T] = :-[T]

object Typed {
  // one layer (only used for pattern matching)
  enum Stmt {
    case Val(id: Id, binding: Typed[effekt.core.typed.Stmt], body: Typed[effekt.core.typed.Stmt])
    case Return(expr: Typed[effekt.core.typed.Expr])
  }
  enum Expr {
    case Literal(value: Any, annotatedTpe: ValueType)
    case ImpureApp(id: Id, args: List[Typed[effekt.core.typed.Expr]])
    case ValueVar(id: Id)
  }
}

extension (stmt: Typed[Stmt]) {
  @targetName("stmt_tpe")
  def tpe: ValueType = stmt.unroll match {
    case ctx :- Typed.Stmt.Val(id, binding, body) => body.tpe
    case ctx :- Typed.Stmt.Return(expr) => expr.tpe
  }

  @targetName("stmt_unroll")
  def unroll: Typed[Typed.Stmt] = stmt match {
    case ctx :- Stmt.Val(id, binding, body) =>
      ctx :- Typed.Stmt.Val(id, ctx :- binding, ctx + (id -> ctx.typeOf(binding)) :- body)
    case ctx :- Stmt.Return(expr) =>
      ctx :- Typed.Stmt.Return(ctx :- expr)
  }
}

extension (expr: Typed[Expr]) {
  @targetName("expr_tpe")
  def tpe: ValueType = expr.unroll match {
    case ctx :- Typed.Expr.Literal(value, annotatedTpe) => annotatedTpe
    case ctx :- Typed.Expr.ImpureApp(id, args) =>
      val funType = ctx extern id
      Type.instantiate(funType, Nil, Nil).result
    case ctx :- Typed.Expr.ValueVar(id) => ctx lookup id
  }

  @targetName("expr_unroll")
  def unroll: Typed[Typed.Expr] = expr match {
    case ctx :- Expr.Literal(id, tpe) =>
      ctx :- Typed.Expr.Literal(id, tpe)
    case ctx :- Expr.ImpureApp(id, args) =>
      ctx :- Typed.Expr.ImpureApp(id, args.map(arg => ctx :- arg))
    case ctx :- Expr.ValueVar(id) =>
      ctx :- Typed.Expr.ValueVar(id)
  }
}

// Usage
//
def transform(stmt: Typed[Stmt]): Stmt = stmt.unroll match {
  case ctx :- Typed.Stmt.Val(id, binding, body) =>
    if (binding.tpe == Type.TUnit) transform(body) else stmt.extract
  case ctx :- Typed.Stmt.Return(expr) => stmt.extract
}

object MyApp extends scala.App {

  val x = Id("x")
  val term = Stmt.Val(x, Stmt.Return(Expr.Literal(42, Type.TInt)),
    Stmt.Return(Expr.ValueVar(x)))

  val term2 = Stmt.Val(x, Stmt.Return(Expr.Literal((), Type.TUnit)),
    Stmt.Return(Expr.Literal(42, Type.TInt)))

  println(util.show(Types.empty typeOf term2))
  val transformed = transform(Types.empty :- term2)
  println(transformed)
  println(util.show(Types.empty typeOf transformed))
}


object implicitly {

  case class Types(
    externs: Map[Id, BlockType.Function],
    values: Map[Id, ValueType]
  ) {
    def bind(id: Id, tpe: ValueType): Types = Types(externs, values.updated(id, tpe))
    def +(binding: (Id, ValueType)): Types = bind(binding._1, binding._2)

    def `:-`[T](value: T): Typed[T] = new `:-`(this, value)

    def lookup(id: Id): ValueType = values.getOrElse(id, sys error s"Cannot find type for ${id}")

    def extern(id: Id): BlockType.Function = externs.getOrElse(id, sys error s"Extern not found ${id}")
  }
  object Types {
    val empty = Types(Map.empty, Map.empty)
  }

  type Γ = Types
  def Γ(using context: Types): Types = context

  def lookup(id: Id)(using context: Types): ValueType = context.lookup(id)

  // we Use :- instead of vdash to associate to the left
  case class `:-`[+T](context: Types, extract: T)



  type Typed[+T] = :-[T]

  object Typed {
    // one layer (only used for pattern matching)
    enum Stmt {
      case Val(id: Id, binding: Typed[effekt.core.typed.Stmt], body: Typed[effekt.core.typed.Stmt])
      case Return(expr: Typed[effekt.core.typed.Expr])
    }
    enum Expr {
      case Literal(value: Any, annotatedTpe: ValueType)
      case ImpureApp(id: Id, args: List[Typed[effekt.core.typed.Expr]])
      case ValueVar(id: Id)
    }
  }

  extension (stmt: Typed[Stmt]) {
    @targetName("stmt_tpe")
    def tpe: ValueType = stmt.extract.tpe(using stmt.context)

    @targetName("stmt_unroll")
    def unroll: Typed.Stmt = stmt match {
      case given Γ :- Stmt.Val(id, binding, body) =>
        Typed.Stmt.Val(id, Γ :- binding, Γ + (id -> binding.tpe) :- body)
      case given Γ :- Stmt.Return(expr) =>
        Typed.Stmt.Return(Γ :- expr)
    }

    def unrollTyped: Typed[Typed.Stmt] = stmt.context :- stmt.unroll
  }

  extension (expr: Typed[Expr]) {
    @targetName("expr_tpe")
    def tpe: ValueType = expr.extract.tpe(using expr.context)

    @targetName("expr_unroll")
    def unroll: Typed.Expr = expr match {
      case given Γ :- Expr.Literal(id, tpe) => Typed.Expr.Literal(id, tpe)
      case given Γ :- Expr.ImpureApp(id, args) => Typed.Expr.ImpureApp(id, args.map(arg => Γ :- arg))
      case given Γ :- Expr.ValueVar(id) => Typed.Expr.ValueVar(id)
    }
  }

  extension (stmt: Stmt) {
    def tpe(using Γ): ValueType = (Γ :- stmt).unroll match {
      case Typed.Stmt.Val(id, binding, body) => body.tpe
      case Typed.Stmt.Return(expr) => expr.tpe
    }
  }

  extension (expr: Expr) {
    def tpe(using Γ): ValueType = (Γ :- expr).unroll match {
      case Typed.Expr.Literal(value, annotatedTpe) => annotatedTpe
      case Typed.Expr.ImpureApp(id, args) =>
        val funType = Γ extern id
        Type.instantiate(funType, Nil, Nil).result
      case Typed.Expr.ValueVar(id) => Γ lookup id
    }
  }


  // Usage

  def transform(stmt: Typed[Stmt]): Stmt = stmt.unrollTyped match {
    case given Γ :- Typed.Stmt.Val(id, binding, body) =>
      if (binding.tpe == Type.TUnit) transform(body) else stmt.extract
    case given Γ :- Typed.Stmt.Return(expr) => stmt.extract
  }

  object MyApp extends scala.App {

    val x = Id("x")
    val term = Stmt.Val(x, Stmt.Return(Expr.Literal(42, Type.TInt)),
      Stmt.Return(Expr.ValueVar(x)))

    val term2 = Stmt.Val(x, Stmt.Return(Expr.Literal((), Type.TUnit)),
      Stmt.Return(Expr.Literal(42, Type.TInt)))

    given Types = Types.empty

    println(util.show(term2.tpe))
    val transformed = transform(Types.empty :- term2)
    println(transformed)
    println(util.show(transformed.tpe))
  }
}

// TODO maybe drop implicit contexts (but then we cannot use Γ as term-level name for contexts...)
