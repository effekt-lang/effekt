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

  def toChez(p: Param): ChezName = nameDef(p.id)

  def toChez(module: ModuleDecl)(using ErrorReporter): List[chez.Def] = {
    val decls = module.declarations.flatMap(toChez)
    val externs = module.externs.map(toChez)
     // TODO FIXME, once there is a let _ = ... in there, we are doomed!
    val defns = module.definitions.map(toChez).flatMap {
      case Left(d) => Some(d)
      case Right(None) => None
      case Right(e) => ???
    }
    decls ++ externs ++ defns
  }

  def toChezExpr(stmt: Stmt): chez.Expr = stmt match {
    case Return(e) => pure(toChez(e))
    case App(b, targs, vargs, bargs) => chez.Call(toChez(b), vargs.map(toChez) ++ bargs.map(toChez))
    case If(cond, thn, els) => chez.If(toChez(cond), toChezExpr(thn), toChezExpr(els))
    case Val(id, binding, body) => bind(toChezExpr(binding), nameDef(id), toChez(body))
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

    case Var(id, init, capt, body) =>
      state(nameDef(id), toChez(init), toChez(body))

    case Get(id, capt, tpe) =>
      chez.Call(chez.Call(nameRef(symbols.builtins.TState.get), nameRef(id)))

    case Put(id, capt, value) =>
      chez.Call(chez.Call(nameRef(symbols.builtins.TState.put), nameRef(id)), toChez(value))

    case Alloc(id, init, region, body) if region == symbols.builtins.globalRegion =>
      chez.Let(List(Binding(nameDef(id), chez.Builtin("box", toChez(init)))), toChez(body))

    case Alloc(id, init, region, body) =>
      chez.Let(List(Binding(nameDef(id), chez.Builtin("fresh", chez.Variable(nameRef(region)), toChez(init)))), toChez(body))

    case Try(body, handler) =>
      val handlers: List[chez.Handler] = handler.map { h =>
        val names = RecordNames(h.interface.name)
        chez.Handler(names.constructor, h.operations.map {
          case Operation(op, tps, cps, vps, bps, Some(resume), body) =>
            resume.tpe match {
              case BlockType.Function(tparams, cparams, Nil, bparams, result) =>
                NOT_SUPPORTED(s"Bidirectional handlers are not support by the Chez backends (encountered when translating the handler for ${names.name})")
              case _ => ()
            }
            chez.Operation(nameDef(op), vps.map(p => nameDef(p.id)), nameDef(resume.id), toChezExpr(body))
          case _ => INTERNAL_ERROR("Handler operations should have a continuation argument")
        })
      }
      chez.Handle(handlers, toChez(body))

    case Region(body) => chez.Builtin("with-region", toChez(body))

    case other => chez.Let(Nil, toChez(other))
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
        chez.Lambda((vps ++ bps) map { p => nameDef(p.id) },
          tBody))

    case Extern.Include(ff, contents) =>
      RawDef(contents)
  }

  def toChez(t: Template[core.Expr]): chez.Expr =
    chez.RawExpr(t.strings, t.args.map(e => toChez(e)))

  def toChez(defn: Definition): Either[chez.Def, Option[chez.Expr]] = defn match {
    case Definition.Def(id, block) =>
      Left(chez.Constant(nameDef(id), toChez(block)))

    case Definition.Let(Wildcard(), binding) =>
      toChez(binding) match {
        // drop the binding altogether, if it is of the form:
        //   let _ = myVariable; BODY
        // since this might lead to invalid scheme code.
        case _: chez.Variable => Right(None)
        case other => Right(Some(other))
      }

    // we could also generate a let here...
    case Definition.Let(id, binding) =>
      Left(chez.Constant(nameDef(id), toChez(binding)))
  }

  def toChez(stmt: Stmt): chez.Block = stmt match {
    // TODO maybe this can be simplified after also introducing mutual definitions
    case Scope(definitions, body) =>
      definitions.map(toChez).foldRight(toChez(body)) {
        case (Left(defn), chez.Block(defns, exprs, result)) => chez.Block(defn :: defns, exprs, result)
        case (Right(Some(expr)), chez.Block(Nil, exprs, result)) => chez.Block(Nil, expr :: exprs, result)
        case (Right(Some(expr)), rest) => chez.Block(Nil, expr :: Nil, chez.Let(Nil, rest))
        case (Right(None), rest) => rest
      }

    case other => chez.Block(Nil, Nil, toChezExpr(other))
  }

  def toChez(block: BlockLit): chez.Lambda = block match {
    case BlockLit(tps, cps, vps, bps, body) =>
      chez.Lambda((vps ++ bps) map toChez, toChez(body))
  }

  def toChez(block: Block): chez.Expr = block match {
    case BlockVar(id, _, _) =>
      chez.Variable(nameRef(id))

    case b @ BlockLit(tps, cps, vps, bps, body) => toChez(b)

    case Member(b, field, tpe) =>
      chez.Call(chez.Variable(nameRef(field)), List(toChez(b)))

    case Unbox(e) => toChez(e)

    case New(impl) => toChez(impl)
  }

  def toChez(impl: Implementation): chez.Expr =
    val ChezName(name) = nameRef(impl.interface.name)
    chez.Call(chez.Variable(ChezName(name)), impl.operations.map(toChez))

  def toChez(op: Operation): chez.Expr = op match {
    case Operation(name, tps, cps, vps, bps, resume, body) =>
      chez.Lambda((vps ++ bps) map toChez, toChez(body))
  }

  def toChez(expr: Expr): chez.Expr = expr match {
    case Literal((), _)         => chez.RawValue("#f")

    case Literal(s: String, _)  => ChezString(chez.adaptEscapes(s))
    case Literal(b: Boolean, _) => if (b) chez.RawValue("#t") else chez.RawValue("#f")
    case l: Literal             => chez.RawValue(l.value.toString)
    case ValueVar(id, _)        => chez.Variable(nameRef(id))

    case DirectApp(b, targs, vargs, bargs) => chez.Call(toChez(b), vargs.map(toChez) ++ bargs.map(toChez))
    case PureApp(b, targs, args) => chez.Call(toChez(b), args map toChez)
    case Make(data, tag, args) => chez.Call(chez.Variable(nameRef(tag)), args map toChez)

    case Select(b, field, _) =>
      chez.Call(nameRef(field), toChez(b))

    case Box(b, _) => toChez(b)

    case Run(s) => run(toChezExpr(s))
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
      chez.Lambda(Nil, pure(chez.Builtin("unbox", chez.Variable(ref)))))

    val setter = chez.Function(nameDef(symbols.builtins.TState.put), List(ref),
      chez.Lambda(List(value), pure(chez.Builtin("set-box!", chez.Variable(ref), chez.Variable(value)))))

    List(getter, setter)
  }
}
