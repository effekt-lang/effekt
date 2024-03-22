package effekt
package generator
package ml

import scala.annotation.tailrec

// TODO choose appropriate representation and apply conversions
class MLName(n: String) {
  private def fixName(nn: String): String = {
    val tmp = nn.replace("$", "Dollar")
    if (tmp.startsWith("_")) "f" + tmp else tmp
  }

  val name: String = fixName(n)

  override def equals(obj: Any): Boolean = obj match {
    case n: MLName =>
      this.name == n.name
    case _ => false
  }

  override def toString: String = n

  override def hashCode(): Int = this.name.hashCode
}

enum Type {
  case TApp(tpe: Type, args: List[Type])
  case Builtin(n: MLName)
  case Var(n: MLName)
  case Tuple(args: List[Type])
  case Unit
  case Integer
  case Real
  case String
  case Bool
  case Data(name: MLName)
  case Fun(args: List[Type], res: Type)
}

enum Param {
  case Named(name: MLName)
  case Patterned(pattern: Pattern)
}

enum Binding {
  case AnonBind(body: Expr)
  case ValBind(name: MLName, body: Expr)
  case FunBind(name: MLName, params: List[Param], body: Expr)
  case RawBind(raw: String)
  case DataBind(name: MLName, tparams: List[Type.Var], constructors: List[(MLName, Option[Type])])
  case TypeBind(name: MLName, tparams: List[Type.Var], tpe: Type)
}
export Binding.*

case class Toplevel(bindings: List[Binding], body: Expr)


/**
 * This file defines the syntax of ML as it is the image of our translation.
 */
enum Expr {

  // e.g. (<EXPR>(<EXPR>, ..., <EXPR>))
  case Call(callee: Expr, args: List[Expr])

  // e.g. "" <EXPR> " + " <EXPR>
  //   raw ml splices, always start with a prefix string, then interleaved with arguments
  case RawExpr(raw: List[String], args: List[Expr])

  // e.g. 42 (represented as Scala string "42") and inserted verbatim
  case RawValue(raw: String)

  // Sequential Scoping
  case Let(bindings: List[Binding], body: Expr)

  case Tuple(terms: List[Expr])

  case Sequence(exps: List[Expr], rest: Expr)

  //  // e.g. (let* ([x 42] [y x]) (+ x y))
  //  case Let_*(bindings: List[Binding], body: Block)

  // e.g. (fn (x y) => body)
  case Lambda(params: List[Param], body: Expr)

  // e.g. (if COND then THEN else ELSE)
  case If(cond: Expr, thn: Expr, els: Expr)

  // e.g x
  case Variable(name: MLName)

  case Make(tag: MLName, arg: Option[Expr])

  case Match(scrutinee: Expr, clauses: List[MatchClause], default: Option[Expr])

  case Ref(exp: Expr)

  case Deref(exp: Expr)

  case Assign(asignee: Expr, value: Expr)
}

export Expr.*

def RawExpr(str: String): ml.Expr = Expr.RawExpr(List(str), Nil)


case class MatchClause(pattern: Pattern, body: Expr)
enum Pattern {
  case Wild()
  case Named(name: MLName)
  case Datatype(tag: MLName, args: List[Pattern])
}

object Consts {

  val unitVal: Expr = RawValue("()")
  val trueVal: Expr = RawValue("true")
  val falseVal: Expr = RawValue("false")
  val id: Expr = ml.Lambda(ml.Param.Named(MLName("x")))(Variable(MLName("x")))

  // from effekt.sml
  val lift: Expr = Variable(MLName("lift"))
  val reset: Expr = Variable(MLName("reset"))
  val run: Expr = Variable(MLName("run"))
  val nested: Expr = Variable(MLName("nested"))
  def here: Expr =
    val a = freshName("a")
    ml.Lambda(a)(a)
  val withRegion: Expr = Variable(MLName("withRegion"))
  val fresh: Expr = Variable(MLName("fresh"))
  val backup: Expr = Variable(MLName("backup"))

}

// smart constructors
def Call(expr: Expr)(args: Expr*): Expr = Expr.Call(expr, args.toList)

def Lambda(params: Param*)(body: Expr): Lambda = Expr.Lambda(params.toList, body)

def MLString(mlString: String): Expr = RawValue(s"\"$mlString\"")

implicit def autoVar(n: MLName): Expr = Variable(n)
implicit def autoParam(n: MLName): Param = Param.Named(n)

@tailrec
def mkLet(bindings: List[Binding], body: Expr): Expr = body match {
  case Let(bindings1, body1) => mkLet(bindings ++ bindings1, body1)
  case _ => ml.Expr.Let(bindings, body)
}

object utils extends util.Structural {

  case class Substitution(terms: Map[MLName, Expr], types: Map[MLName, Type]) {
    def --(vars: Vars): Substitution = Substitution(terms.removedAll(vars.terms), types.removedAll(vars.types))
  }

  def substituteTerms(in: Expr)(subst: (MLName, Expr)*): Expr =
    substitute(in)(using Substitution(subst.toMap, Map.empty))

  def substituteTypes(in: Expr)(subst: (MLName, Type)*): Expr =
    substitute(in)(using Substitution(Map.empty, subst.toMap))

  def substitute(exp: Expr)(using s: Substitution): Expr = exp match {
    case Variable(n) => s.terms.getOrElse(n, exp)
    case Lambda(params, body) => Lambda(params.map(substitute), substitute(body)(using s -- params.map(Vars.bound)))
    case Let(bindings, body) => Let(bindings.map(substitute), substitute(body)(using s -- bindings.map(Vars.bound)))

    case e : (RawExpr | RawValue | Tuple | Sequence | If | Make | Ref | Deref | Assign | Call | Match) =>
      rewriteStructurally[Expr](e)
  }

  def substitute(p: Param)(using s: Substitution): Param = p match {
    case Param.Named(name) => p
    case Param.Patterned(pattern) => Param.Patterned(substitute(pattern))
  }

  def substitute(m: MatchClause)(using s: Substitution): MatchClause = m match {
    case MatchClause(pattern, body) =>
      MatchClause(substitute(pattern), substitute(body)(using s -- Vars.bound(pattern)))
  }

  def substitute(pattern: Pattern)(using s: Substitution): Pattern =
    rewriteStructurally(pattern)

  def substitute(b: Binding)(using s: Substitution): Binding = b match {
    case Binding.FunBind(name, params, body) =>
      val bound = Vars.term(name) ++ params.map(Vars.bound)
      Binding.FunBind(name, params.map(substitute), substitute(body)(using s -- bound))

    case Binding.DataBind(name, tparams, constructors) =>
      val bound = Vars.tpe(name) ++ Vars.types(tparams.map(_.n))
      Binding.DataBind(name, tparams, constructors map {
        case (n, tpe) => (n, tpe.map(substitute(_)(using s -- bound)))
      })

    case Binding.TypeBind(name, tparams, tpe) =>
      val bound = Vars.tpe(name) ++ Vars.types(tparams.map(_.n))
      Binding.TypeBind(name, tparams, substitute(tpe)(using s -- bound))

    // congruences
    case Binding.RawBind(raw) => b
    case Binding.AnonBind(body) => Binding.AnonBind(substitute(body))
    case Binding.ValBind(name, body) => Binding.ValBind(name, substitute(body))
  }

  def substitute(tpe: Type)(using s: Substitution): Type = tpe match {
    case Type.Builtin(n) => s.types.getOrElse(n, tpe)
    case Type.Var(n) => s.types.getOrElse(n, tpe)

    // congruence cases
    case Type.TApp(tpe, args) => Type.TApp(substitute(tpe), args.map(substitute))
    case Type.Tuple(args) => Type.Tuple(args.map(substitute))
    case Type.Data(name) => tpe
    case Type.Fun(args, res) => Type.Fun(args.map(substitute), substitute(res))
    case Type.Unit | Type.Integer | Type.Real | Type.String | Type.Bool => tpe
  }
}


case class Vars(terms: Set[MLName], types: Set[MLName]) {
  def ++(other: Vars): Vars =
    Vars(terms ++ other.terms, types ++ other.types)

  def --(other: Vars): Vars =
    Vars(terms -- other.terms, types -- other.types)
}
object Vars {
  def empty = Vars(Set.empty, Set.empty)
  def term(name: MLName) = Vars(Set(name), Set.empty)
  def tpe(name: MLName) = Vars(Set.empty, Set(name))
  def types(names: List[MLName]) = Vars(Set.empty, names.toSet)

  implicit def flatten(vars: List[Vars]): Vars = vars.fold(Vars.empty)(_ ++ _)
  implicit def flatten(vars: Option[Vars]): Vars = vars.getOrElse(Vars.empty)

  def bound(b: Binding): Vars = b match {
    case Binding.AnonBind(body) => Vars.empty
    case Binding.ValBind(name, body) => Vars.term(name)
    case Binding.FunBind(name, params, body) => Vars.term(name)
    case Binding.RawBind(raw) => Vars.empty
    case Binding.DataBind(name, tparams, constructors) =>
      Vars.tpe(name) ++ constructors.foldLeft(Vars.empty) {
        case (xs, (ctor, tpe)) => Vars.term(ctor) ++ xs
      }
    case Binding.TypeBind(name, tparams, tpe) => Vars.tpe(name)
  }
  def bound(p: List[Param]): Vars = p.map(bound)
  def bound(p: Param): Vars = p match {
    case Param.Named(name) => Vars.term(name)
    case Param.Patterned(p) => bound(p)
  }

  def bound(p: Pattern): Vars = p match {
    case Pattern.Wild() => Vars.empty
    case Pattern.Named(name) => Vars.term(name)
    case Pattern.Datatype(tag, args) => args.map(bound)
  }

  def free(p: Param): Vars = p match {
    case Param.Named(name) => Vars.empty
    case Param.Patterned(pattern) => free(pattern)
  }

  def free(p: Pattern): Vars = p match {
    case Pattern.Wild() => Vars.empty
    case Pattern.Named(name) => Vars.empty
    case Pattern.Datatype(tag, args) => Vars.tpe(tag) ++ args.map(free)
  }

  def free(p: Binding): Vars = p match {
    case Binding.FunBind(name, params, body) => free(body) -- params.map(bound) -- Vars.term(name)
    case Binding.DataBind(name, tparams, ctors) => ctors.map {
      case (ctor, tpe) => flatten(tpe.map(free))
    } -- Vars.types(tparams.map(_.n)) -- Vars.tpe(name)
    case Binding.TypeBind(name, tparams, tpe) => free(tpe) -- Vars.types(tparams.map(_.n)) -- Vars.tpe(name)

    // congruence cases
    case Binding.AnonBind(body) => free(body)
    case Binding.ValBind(name, body) => free(body)
    case Binding.RawBind(raw) => Vars.empty
  }

  def free(c: MatchClause): Vars = c match {
    case MatchClause(pattern, body) => (free(body) -- bound(pattern)) ++ free(pattern)
  }

  def free(exp: Expr): Vars = exp match {
    case Expr.Variable(name) => Vars.term(name)
    case Expr.Lambda(params, body) => (free(body) -- params.map(bound)) ++ params.map(free)
    case Expr.Let(bindings, body) => (free(body) -- bindings.map(bound)) ++ bindings.map(free)

    // congruence cases
    case Expr.Call(callee, args) => free(callee) ++ args.map(free)
    case Expr.RawExpr(raw, args) => args.map(free)
    case Expr.RawValue(raw) => Vars.empty // we don't know
    case Expr.Tuple(terms) => terms.map(free)
    case Expr.Sequence(exps, rest) => exps.map(free) ++ free(rest)
    case Expr.If(cond, thn, els) => free(cond) ++ free(thn) ++ free(els)
    case Expr.Make(tag, arg) => arg.map(free)
    case Expr.Match(scrutinee, clauses, default) => free(scrutinee) ++ clauses.map(free) ++ default.map(free)
    case Expr.Ref(exp) => free(exp)
    case Expr.Deref(exp) => free(exp)
    case Expr.Assign(assignee, value) => free(assignee) ++ free(value)
  }

  def free(tpe: Type): Vars = tpe match {
    case Type.Builtin(n) => Vars.tpe(n)
    case Type.Var(n) => Vars.tpe(n)

    // congruence cases
    case Type.Fun(args, res) => free(res) ++ args.map(free)
    case Type.TApp(tpe, args) => free(tpe) ++ args.map(free)
    case Type.Tuple(args) => args.map(free)
    case Type.Data(n) => Vars.empty
    case Type.Unit |  Type.Integer | Type.Real | Type.String | Type.Bool => Vars.empty
  }
}
