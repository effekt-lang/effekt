package effekt
package cps

import effekt.context.Context
import effekt.machine.Statement.Invoke
import effekt.symbols.{ Constructor, Name, Symbol }

import scala.collection.immutable

export effekt.core.Id

// notes:
// - we desugar If -> Match, Select into pure app + pure user-defined selector function
// - treat Hole as builtin [A]() => A

// The most important question we have to answer is:
// - should this IR still have lifts, or not?
//   If no, then we cannot use it without evidence mono.


// - what is the best way to express run (e.g. s(id))? Curried? As special application form?
// - should we distinguish recursive from non-recursive bindings (let vs. letrec)
//   syntactically?
// - do we need If in preceding IRs if we have match??? Codegens could still special case data with two variants


// We do not **syntactically** distinguish between control effecting terms or not.
// This is done ONLY on the typelevel. Applying functions that are stackshape monomorphic (that is,
// that have a fixed final answer type) are considered "expressions", while applying stackshape
// polymorphic functions is considered a "statement".


case class ModuleDecl(
  path: String,
  imports: List[String],
  decls: List[Declaration],
  externs: List[Extern],
  definitions: List[Definition],
  exports: List[Id],
  main: Expr
)

/**
 * Toplevel data and interface declarations
 */
enum Declaration {
  def id: Id

  case Data(id: Id, tparams: List[Id], constructors: List[Constructor])
  case Interface(id: Id, tparams: List[Id], properties: List[Method])
}
export Declaration.*

case class Constructor(id: Id, fields: List[Field])
case class Field(id: Id, tpe: Type)
case class Method(id: Id, tpe: Type.Function)

enum Extern {
  case Def(id: Id, tparams: List[Id], params: List[Id], ret: Type, body: String)
  case Include(contents: String)
}

enum Definition {
  def id: Id
  def binding: Expr

  // can be recursive (either function, or new)
  case Def(id: Id, binding: Expr)

  // cannot be recursive
  case Let(id: Id, binding: Expr)

  def pure: Boolean = binding.pure
}

// we give up the distinction between expressions, statements, and blocks
enum Type {
  case Var(name: Id)
  case Data(name: Id, targs: List[Type])
  // [T1, T2](Int) ->[#Bool,#Int] R
  case Function(tparams: List[Id], params: List[Type], shape: Shape)
  case Interface(name: Id, targs: List[Type])
}

// List of delimiters on the stack
//  [#A, #B, #C]
// corresponds to the shape
//
//  |  ...  |
//  |-- A --|
//  |  ...  |
//  |-- B --|
//  |  ...  |
//  |-- C --|
//  |  ...  |
//  |---?---|
//
// where ? is the final answer type.
enum Shape {
  def shape: List[Delim]

  case Mono(shape: List[Delim], tpe: Type)
  case Poly(shape: List[Delim])
}
object Shape {
  def Mono(tpe: Type): Shape.Mono = Mono(Nil, tpe)
  def Mono(delims: Delim*)(tpe: Type): Shape.Mono = Mono(delims.toList, tpe)
  def Poly(delims: Delim*): Shape.Poly = Poly(delims.toList)
}

enum Delim {
  // #Int
  case Reset(answer: Type)
  // !Int
  case State(state: Type)
}

case class Param(id: Id, tpe: Type)

enum Expr {

  // x: Int   or   k: (Int) ->[] R
  case Var(id: Id, annotatedType: Type)

  // 42: Int
  case Literal(value: Any, annotatedType: Type)

  // T(e..)
  case Make(constructor: Id, args: List[Expr], annotatedType: Type.Data)

  // e.g.  { [T..](x..| k..) => s }
  //
  // Examples:
  //   { (x|k) => k(x + 1) }
  //   { (x|) => x + 1 }
  //   ID   = [A]{ (x:A|) => x }
  //   PURE = [A]{ (a:A|k) => k(a) }
  //
  // Please note again: `answer` is a binding occurrence (conceptually, within shape AND body).
  // Invariant: sizeof(conts) = sizeof(shape)
  case Mono(tparams: List[Id], params: List[Param], conts: List[Id], shape: Shape.Mono, body: Expr)
  case Poly(answer: Id, tparams: List[Id], params: List[Param], conts: List[Id], shape: Shape.Poly, body: Expr)

  // f(e..|{ (x|) => s }..)
  case App(fun: Expr, targs: List[Type], args: List[Expr], conts: List[Expr])

  // let x = 4; def f = ...; e
  case Scope(defs: List[Definition], body: Expr)

  // e match { case t: (x) => x .. }
  case Match(scrutinee: Expr, cases: List[Clause], default: Option[Expr])

  // new I { m: b.. }
  case New(interface: Type.Interface, operations: List[Operation])

  // e.m(e..|k..)
  case Invoke(callee: Expr, method: Id, args: List[Expr], conts: List[Expr])


  def tpe: Type = ???
  // maybe we need to record the latent effect in the function type?
  // todo do we also need effect polymorphism here? That would be a bit annoying.
  def pure: Boolean = this match {
    case _: (Var | Literal | Make | Mono | Poly | New) => true

    // these are the difficult cases
    case App(f, targs, args, conts) => ???
    case Invoke(callee, method, args, conts) => ???

    // congruences
    case Match(sc, cases, default) => sc.pure && cases.forall {
      case Clause(_, body) => body.pure
    } && default.forall { _.pure }

    case Scope(defs, body) => defs.forall { _.pure } && body.pure
  }
}
case class Operation(name: Id, implementation: Lambda)
case class Clause(tag: Id, body: Expr.Mono)

type Lambda = Expr.Mono | Expr.Poly

object examples {
  implicit def autoVar(id: Id): Type.Var = Type.Var(id)

  import Expr.*

  val x = Id("x")
  val k = Id("k")
  val A = Id("A")
  val R = Id("R")

  def TInt: Type = ???

  def fType = Type.Function(Nil, Nil, Shape.Poly(List(Delim.Reset(TInt))))

  // def kType = Type.Function(Nil, List(TInt), Shape.Mono(Nil, alpha))

  // [A](x: A|) => x
  val ID = Mono(List(A), Param(x, A) :: Nil, Nil, Shape.Mono(A),
    Var(x, A))

  // [A](x: A|k) => k(x|)
  val RETURN = Poly(R, List(A), Param(x, A) :: Nil, k :: Nil, Shape.Poly(Delim.Reset(R)),
    App(Var(k, Type.Function(Nil, List(A), Shape.Mono(R))), Nil, Var(x, A) :: Nil, Nil))
}



// def f = [A|R](x: A|k: (A|) -> R) => k(x|)
// def g = [A|R](x: A|k1: (A| [](B|) -> R)) -> R, k2: (B|) -> R) => k1(x|k2)
//
// let's write
//   (A|) -> R
// as
//   (A) ->[] R
//
// and
//   (A| (B|) -> R)) -> R
// as
//   (A) ->[#B] R
//
// the type of g is
//   [A|R](A) ->[#A, #B] R
//
// we can instantiate R as B to obtain
//   g' : [A|](A) ->[#A, #B] B
//
// g[Int|B](42 | {(a: Int| k: (B) ->[] B) => k(intToB(a)|) }, { (b: B|) => b })



//   def f: [|R]() ->[#Int] R
// we use the abbreviation for polymorphic function types:
//   def f: () -> [#Int]
