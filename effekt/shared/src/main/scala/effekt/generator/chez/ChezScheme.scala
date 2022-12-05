package effekt
package generator
package chez

import effekt.context.Context
import effekt.core.*
import effekt.symbols.{ Module, Symbol, Wildcard, TermSymbol }

import scala.language.implicitConversions
import effekt.util.paths.*
import kiama.output.PrettyPrinterTypes.Document

object ChezSchemeMonadic extends Backend with ChezScheme {

  def run(expr: chez.Expr): chez.Expr =
    Builtin("run", expr)

  def pure(expr: chez.Expr): chez.Expr =
    Builtin("pure", expr)

  def bind(binding: chez.Expr, param: ChezName, body: chez.Block): chez.Expr =
    Builtin("then", binding, chez.Lambda(List(param), body))

  def runMain(main: ChezName): chez.Expr =
    chez.Builtin("run", chez.Call(main))
}

object ChezSchemeCallCC extends Backend with ChezScheme {

  def run(expr: chez.Expr): chez.Expr = expr

  def pure(expr: chez.Expr): chez.Expr = expr

  def bind(binding: chez.Expr, param: ChezName, body: chez.Block): chez.Expr =
    chez.Let(List(Binding(param, binding)), body)

  def runMain(main: ChezName): chez.Expr =
    chez.Builtin("run", Variable(main))
}

trait ChezScheme {

  def run(expr: chez.Expr): chez.Expr
  def pure(expr: chez.Expr): chez.Expr
  def bind(binding: chez.Expr, param: ChezName, body: chez.Block): chez.Expr

  def runMain(main: ChezName): chez.Expr

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, mainSym: TermSymbol)(using C: Context) = {
    val chezModule = cleanup(chez.Let(Nil, compilationUnit(mainSym, main.mod, main.core)))
    val result = chez.PrettyPrinter.pretty(chez.PrettyPrinter.toDoc(chezModule), 100)
    val mainFile = path(main.mod)
    Some(Compiled(mainFile, Map(mainFile -> result)))
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: CoreTransformed)(using C: Context) =
    C.using(module = input.mod) { Some(chez.PrettyPrinter.format(compile(input))) }

  /**
   * Compiles only the given module, does not compile dependencies
   */
  private def compile(in: CoreTransformed)(using Context): List[chez.Def] =
    toChez(in.core)

  def compilationUnit(mainSymbol: Symbol, mod: Module, core: ModuleDecl)(implicit C: Context): chez.Block = {
    val definitions = toChez(core)
    chez.Block(generateStateAccessors ++ definitions, Nil, runMain(nameRef(mainSymbol)))
  }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"


  def toChez(p: Param): ChezName = nameDef(p.id)

  def toChez(module: ModuleDecl): List[chez.Def] = {
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

  def toChez(args: List[Argument]): List[chez.Expr] = args map {
    case b: Block => toChez(b)
    case e: Pure => toChez(e)
  }

  def toChezExpr(stmt: Stmt): chez.Expr = stmt match {
    case Return(e) => pure(toChez(e))
    case App(b, targs, vargs, bargs) => chez.Call(toChez(b), toChez(vargs) ++ toChez(bargs))
    case If(cond, thn, els) => chez.If(toChez(cond), toChezExpr(thn), toChezExpr(els))
    case Val(id, binding, body) => bind(toChezExpr(binding), nameDef(id), toChez(body))
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

    case State(id, init, region, body) if region == symbols.builtins.globalRegion =>
      chez.Let(List(Binding(nameDef(id), chez.Builtin("box", toChez(init)))), toChez(body))

    case State(id, init, region, body) =>
      chez.Let(List(Binding(nameDef(id), chez.Builtin("fresh", Variable(nameRef(region)), toChez(init)))), toChez(body))

    case Try(body, handler) =>
      val handlers: List[chez.Handler] = handler.map { h =>
        // TODO we should not use the symbol here, anymore (we should look it up in the Declarations)
        val names = RecordNames(h.interface.symbol)
        chez.Handler(names.constructor, h.operations.map {
          case Operation(op, tps, cps, vps, bps, resume, body) =>
            chez.Operation(nameDef(op), (vps ++ bps).map(p => nameDef(p.id)), nameDef(resume.get.id), toChezExpr(body))
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

  def toChez(decl: core.Extern): chez.Def = decl match {
    case Extern.Def(id, tpe, cps, vps, bps, ret, capt, body) =>
      chez.Constant(nameDef(id),
        chez.Lambda((vps ++ bps) map { p => ChezName(p.id.name.name) },
          chez.RawExpr(body)))

    case Extern.Include(contents) =>
      RawDef(contents)
  }

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
      Variable(nameRef(id))

    case b @ BlockLit(tps, cps, vps, bps, body) => toChez(b)

    case Member(b, field, tpe) =>
      chez.Call(Variable(nameRef(field)), List(toChez(b)))

    case Unbox(e) =>
      toChez(e)

    case New(Implementation(tpe, clauses)) =>
      val ChezName(name) = nameRef(tpe.symbol)
      chez.Call(Variable(ChezName(s"make-${name}")), clauses.map {
        case Operation(_, tps, cps, vps, bps, resume, body) => chez.Lambda((vps ++ bps) map toChez, toChez(body))
      })
  }

  def toChez(expr: Expr): chez.Expr = expr match {
    case Literal((), _)         => chez.RawValue("#f")
    case Literal(s: String, _)  => ChezString(s)
    case Literal(b: Boolean, _) => if (b) chez.RawValue("#t") else chez.RawValue("#f")
    case l: Literal             => chez.RawValue(l.value.toString)
    case ValueVar(id, _)        => chez.Variable(nameRef(id))

    case DirectApp(b, targs, vargs, bargs) => chez.Call(toChez(b), toChez(vargs) ++ toChez(bargs))
    case PureApp(b, targs, args) => chez.Call(toChez(b), args map toChez)

    case Select(b, field, _) =>
      chez.Call(nameRef(field), toChez(b))

    case Box(b, _) => toChez(b)

    case Run(s) => run(toChezExpr(s))
  }
}
