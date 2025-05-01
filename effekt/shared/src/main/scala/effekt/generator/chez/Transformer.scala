package effekt
package generator
package chez

import effekt.context.Context
import effekt.core.*
import effekt.symbols.{ Module, Symbol, TermSymbol, Wildcard }
import effekt.util.paths.*
import effekt.util.messages.ErrorReporter
import kiama.output.PrettyPrinterTypes.Document
import util.messages.{ INTERNAL_ERROR, NOT_SUPPORTED }

import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.collection.mutable

object TransformerMonadic extends Transformer {

  def run(expr: chez.Expr): chez.Expr =
    Builtin("run", expr)

  def pure(expr: chez.Expr): chez.Expr =
    Builtin("pure", expr)

  def bind(binding: chez.Expr, param: ChezName, body: chez.Block): chez.Expr =
    Builtin("then", binding, chez.Lambda(List(param), body))

  def runMain(main: ChezName): chez.Expr =
    chez.Builtin("run", chez.Call(main))
}

object TransformerCallCC extends Transformer {

  def run(expr: chez.Expr): chez.Expr = expr

  def pure(expr: chez.Expr): chez.Expr = expr

  def bind(binding: chez.Expr, param: ChezName, body: chez.Block): chez.Expr =
    chez.Let(List(Binding(param, binding)), body)

  def runMain(main: ChezName): chez.Expr =
    chez.Builtin("run", chez.Variable(main))
}

trait Transformer {

  val escapeSeqs: Map[Char, String] = Map('\'' -> raw"'", '\"' -> raw"\"", '\\' -> raw"\\", '\n' -> raw"\n", '\t' -> raw"\t", '\r' -> raw"\r")

  def run(expr: chez.Expr): chez.Expr
  def pure(expr: chez.Expr): chez.Expr
  def bind(binding: chez.Expr, param: ChezName, body: chez.Block): chez.Expr

  def runMain(main: ChezName): chez.Expr

  def state(id: ChezName, init: chez.Expr, body: chez.Block): chez.Expr =
    Builtin("state", init, chez.Lambda(List(id), body))

  def compilationUnit(mainSymbol: Symbol, mod: Module, core: ModuleDecl)(using ErrorReporter): chez.Block = {
    val definitions = toChez(core)
    chez.Block(generateStateAccessors(pure) ++ definitions, Nil, runMain(nameRef(mainSymbol)))
  }

  def toChez(p: ValueParam): ChezName = nameDef(p.id)
  def toChez(p: BlockParam): ChezName = nameDef(p.id)

  def toChez(module: ModuleDecl)(using ErrorReporter): List[chez.Def] = {
    val decls = module.declarations.flatMap(toChez)
    val externs = module.externs.map(toChez)
     // TODO FIXME, once there is a let _ = ... in there, we are doomed!
    val defns = module.definitions.map(toChez)
    decls ++ externs ++ defns
  }

  def toChezExpr(stmt: Stmt): chez.Expr = stmt match {
    case Return(e) => pure(toChez(e))
    case App(b, targs, vargs, bargs) => chez.Call(toChez(b), vargs.map(toChez) ++ bargs.map(toChez))
    case Invoke(b, method, methodTpe, targs, vargs, bargs) =>
      chez.Call(chez.Call(chez.Variable(nameRef(method)), List(toChez(b))), vargs.map(toChez) ++ bargs.map(toChez))
    case If(cond, thn, els) => chez.If(toChez(cond), toChezExpr(thn), toChezExpr(els))
    case Val(id, tpe, binding, body) => bind(toChezExpr(binding), nameDef(id), toChez(body))
    // empty matches are translated to a hole in chez scheme
    case Match(scrutinee, Nil, None) => chez.Builtin("hole")
    case Match(scrutinee, clauses, default) =>
      val sc = toChez(scrutinee)
      val cls = clauses.map { case (constr, branch) =>
        val names = RecordNames(constr)
        val pred = chez.Call(chez.Variable(names.predicate), List(sc))
        val matcher = chez.Call(chez.Variable(names.matcher), List(sc, toChez(branch)))
        (pred, matcher)
      }
      chez.Cond(cls, default.map(toChezExpr))

    case Hole() => chez.Builtin("hole")

    case Var(ref, init, capt, body) =>
      state(nameDef(ref), toChez(init), toChez(body))

    case Alloc(id, init, region, body) =>
      chez.Let(List(Binding(nameDef(id), chez.Builtin("fresh", chez.Variable(nameRef(region)), toChez(init)))), toChez(body))

    case Reset(body) => chez.Reset(toChez(body))

    case Shift(p, body) => chez.Shift(nameRef(p.id), toChez(body))

    // currently bidirectional handlers are not supported
    case Resume(k, Return(expr)) => chez.Call(toChez(k), List(toChez(expr)))

    case Resume(k, other) => sys error s"Not supported yet: ${util.show(stmt)}"

    case Region(body) => chez.Builtin("with-region", toChez(body))

    case stmt: (Def | Let | Get | Put) =>
      chez.Let(Nil, toChez(stmt))
  }

  def toChez(decl: core.Declaration): List[chez.Def] = decl match {
    case Data(did, tparams, ctors) =>
      ctors.flatMap { ctor => generateConstructor(ctor.id, ctor.fields.map(f => f.id)) }

    // We use chez scheme records to also represent capabilities.
    case Declaration.Interface(id, tparams, operations) =>
      generateConstructor(id, operations.map(op => op.id))
  }

  def toChez(decl: core.Extern)(using ErrorReporter): chez.Def = decl match {
    case Extern.Def(id, tpe, cps, vps, bps, ret, capt, body) =>
      val tBody = body match {
        case ExternBody.StringExternBody(featureFlag, contents) => toChez(contents)
        case u: ExternBody.Unsupported =>
          u.report
          chez.Builtin("hole")
      }
      chez.Constant(nameDef(id),
        chez.Lambda(vps.map { p => nameDef(p.id) } ++ bps.map { p => nameDef(p.id) },
          tBody))

    case Extern.Include(ff, contents) =>
      RawDef(contents)
  }

  def toChez(t: Template[core.Expr]): chez.Expr =
    chez.RawExpr(t.strings, t.args.map(e => toChez(e)))

  def toChez(defn: Toplevel): chez.Def = defn match {
    case Toplevel.Def(id, block) => chez.Constant(nameDef(id), toChez(block))
    case Toplevel.Val(id, tpe, binding) => chez.Constant(nameDef(id), run(toChezExpr(binding)))
  }

  def toChez(stmt: Stmt): chez.Block = stmt match {
    case Stmt.Def(id, block, body) =>
      val chez.Block(defs, exprs, result) = toChez(body)
      chez.Block(chez.Constant(nameDef(id), toChez(block)) :: defs, exprs, result)

    case Stmt.Let(Wildcard(), tpe, binding, body) =>
      toChez(binding) match {
        // drop the binding altogether, if it is of the form:
        //   let _ = myVariable; BODY
        // since this might lead to invalid scheme code.
        case _: chez.Variable => toChez(body)
        case expr =>
          toChez(body) match {
            case chez.Block(Nil, exprs, result) => chez.Block(Nil, expr :: exprs, result)
            case rest => chez.Block(Nil, expr :: Nil, chez.Let(Nil, rest))
          }
      }

    case Stmt.Let(id, tpe, binding, body) =>
      val chez.Block(defs, exprs, result) = toChez(body)
      chez.Block(chez.Constant(nameDef(id), toChez(binding)) :: defs, exprs, result)

    case Stmt.Get(id, tpe, ref, capt, body) =>
      val reading = chez.Constant(nameDef(id), chez.Call(chez.Call(nameRef(symbols.builtins.TState.get), nameRef(ref))))
      val chez.Block(defs, exprs, result) = toChez(body)
      chez.Block(reading :: defs, exprs, result)

    case Stmt.Put(ref, capt, value, body) =>
      val writing = chez.Call(chez.Call(nameRef(symbols.builtins.TState.put), nameRef(ref)), toChez(value))
      toChez(body) match {
        case chez.Block(Nil, exprs, result) => chez.Block(Nil, writing :: exprs, result)
        case rest => chez.Block(Nil, writing :: Nil, chez.Let(Nil, rest))
      }


    case other => chez.Block(Nil, Nil, toChezExpr(other))
  }

  def toChez(block: BlockLit): chez.Lambda = block match {
    case BlockLit(tps, cps, vps, bps, body) =>
      chez.Lambda(vps.map(toChez) ++ bps.map(toChez), toChez(body))
  }

  def toChez(block: Block): chez.Expr = block match {
    case BlockVar(id, _, _) =>
      chez.Variable(nameRef(id))

    case b @ BlockLit(tps, cps, vps, bps, body) => toChez(b)

    case Unbox(e) => toChez(e)

    case New(impl) => toChez(impl)
  }

  def toChez(impl: Implementation): chez.Expr =
    val ChezName(name) = nameRef(impl.interface.name)
    chez.Call(chez.Variable(ChezName(name)), impl.operations.map(toChez))

  def toChez(op: Operation): chez.Expr = op match {
    case Operation(name, tps, cps, vps, bps, body) =>
      chez.Lambda(vps.map(toChez) ++ bps.map(toChez), toChez(body))
  }

  def toChez(expr: Expr): chez.Expr = expr match {
    case Literal((), _)         => chez.RawValue("#f")

    case Literal(s: String, _)  => escape(s)
    case Literal(b: Boolean, _) => if (b) chez.RawValue("#t") else chez.RawValue("#f")
    case l: Literal             => chez.RawValue(l.value.toString)
    case ValueVar(id, _)        => chez.Variable(nameRef(id))

    case DirectApp(b, targs, vargs, bargs) => chez.Call(toChez(b), vargs.map(toChez) ++ bargs.map(toChez))
    case PureApp(b, targs, args) => chez.Call(toChez(b), args map toChez)
    case Make(data, tag, targs, args) => chez.Call(chez.Variable(nameRef(tag)), args map toChez)

    case Box(b, _) => toChez(b)
  }


  // STATE
  // -----

  // (define (getter ref)
  //  (lambda () (pure (unbox ref))))
  //
  // (define (setter ref)
  //  (lambda (v) (pure (set-box! ref v))))
  def generateStateAccessors(pure: chez.Expr => chez.Expr): List[chez.Function] = {
    val ref = ChezName("ref")
    val value = ChezName("value")

    val getter = chez.Function(nameDef(symbols.builtins.TState.get), List(ref),
      chez.Lambda(Nil, chez.Builtin("unbox", chez.Variable(ref))))

    val setter = chez.Function(nameDef(symbols.builtins.TState.put), List(ref),
      chez.Lambda(List(value), chez.Builtin("set-box!", chez.Variable(ref), chez.Variable(value))))

    List(getter, setter)
  }

  def escape(scalaString: String): chez.Expr = {
    val parts = mutable.ListBuffer[chez.Expr]()
    val strPart = StringBuilder()
    scalaString.codePoints().forEach {
      case c if escapeSeqs.contains(c.toChar) => strPart.append(escapeSeqs(c.toChar))
      case c if c >= 32 && c <= 126 => strPart.append(String.valueOf(Character.toChars(c)))
      case c if c < 8 * 8 * 8 =>
        strPart.append("\\" + Integer.toString(c, 8).reverse.padTo(3, '0').reverse)
      case c =>
        parts.addOne(chez.RawValue("\"" ++ strPart.mkString ++ "\""))
        strPart.clear()
        parts.addOne(chez.Call(chez.RawExpr("string"), chez.Call(chez.RawExpr("integer->char"),
          chez.RawExpr("#x" ++ c.toHexString))))
    }
    parts.addOne(chez.RawValue("\"" ++ strPart.mkString ++ "\""))

    if (parts.size == 1) {
      parts(0)
    } else {
      chez.Call(chez.RawExpr("string-append"), parts.toList)
    }
  }
}
