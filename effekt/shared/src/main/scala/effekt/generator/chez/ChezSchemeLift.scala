package effekt
package generator
package chez

import effekt.context.Context
import effekt.lifted.*
import effekt.symbols.{ Module, Symbol, Wildcard }

import scala.language.implicitConversions
import effekt.util.paths.*
import kiama.output.PrettyPrinterTypes.Document

/**
 * Lifted variant of Chez Scheme. Mostly copy-and-paste from [[ChezScheme]].
 *
 * Difficult to share the code, since core and lifted are different IRs.
 */
object ChezSchemeLift extends Backend {

  def run(expr: chez.Expr): chez.Expr = chez.Builtin("run", expr)
  def pure(expr: chez.Expr): chez.Expr = chez.Builtin("pure", expr)

  // TODO we use the $then variant, for now, since the `then` variant is a macro and would
  // require adding it to the syntax chez.Tree
  def bind(binding: chez.Expr, param: ChezName, body: chez.Block): chez.Expr =
    Builtin("$then", binding, chez.Lambda(List(param), body))

  def runMain(main: ChezName): chez.Expr =
    chez.Builtin("run", chez.Call(main, Variable(ChezName("here"))))

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(using C: Context) = {
    val mainSymbol = C.checkMain(main.mod)
    val deps = dependencies.flatMap { dep => compile(dep) }

    LiftInference(main).map { lifted =>
      val chezModule = chez.Let(Nil, compilationUnit(mainSymbol, lifted.mod, lifted.core, deps))
      val result = chez.PrettyPrinter.pretty(chez.PrettyPrinter.toDoc(chezModule), 100)
      val mainFile = path(main.mod)
      Compiled(mainFile, Map(mainFile -> result))
    }
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
    LiftInference(in).toList.flatMap { lifted => toChez(lifted.core) }

  def compilationUnit(mainSymbol: Symbol, mod: Module, core: ModuleDecl, dependencies: List[chez.Def])(implicit C: Context): chez.Block = {
    val defs = toChez(core)
    chez.Block(generateStateAccessors ++ dependencies ++ defs, Nil, runMain(nameRef(mainSymbol)))
  }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"


  def toChez(p: Param): ChezName = nameDef(p.id)

  def toChez(e: Argument): chez.Expr = e match {
    case e: lifted.Expr  => toChez(e)
    case b: lifted.Block => toChez(b)
    case e: lifted.Evidence => toChez(e)
  }

  def toChez(module: ModuleDecl): List[chez.Def] = {
    val decls = module.decls.flatMap(toChez)
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
    case App(b, targs, args) => chez.Call(toChez(b), args map toChez)
    case If(cond, thn, els) => chez.If(toChez(cond), toChezExpr(thn), toChezExpr(els))
    case Val(id, tpe, binding, body) => bind(toChezExpr(binding), nameDef(id), toChez(body))
    case Match(scrutinee, clauses, default) =>
      val sc = toChez(scrutinee)
      val cls = clauses.map { case (constr, branch) =>
        val names = RecordNames(constr)
        val pred = chez.Call(chez.Variable(names.predicate), List(sc))
        val matcher = chez.Call(chez.Variable(names.matcher), List(sc, toChez(branch)))
        (pred, matcher)
      }
      chez.Cond(cls, default.map(toChezExpr))

    case Hole => chez.Builtin("hole")

    case State(id, init, region, body) if region == symbols.builtins.globalRegion =>
      chez.Let(List(Binding(nameDef(id), chez.Builtin("box", toChez(init)))), toChez(body))

    case State(id, init, region, body) =>
      chez.Let(List(Binding(nameDef(id), chez.Builtin("fresh", Variable(nameRef(region)), toChez(init)))), toChez(body))

    case Try(body, tpe, handler) =>
      val handlers: List[chez.Handler] = handler.map { h =>
        val names = RecordNames(h.id)
        chez.Handler(names.constructor, h.operations.map {
          case Operation(op, BlockLit(params, body)) =>
            // the LAST parameter is the continuation...
            chez.Operation(nameDef(op), params.init.map(p => nameDef(p.id)), nameDef(params.last.id), toChezExpr(body))
        })
      }
      chez.Handle(handlers, toChez(body))

    case Region(body, _) => chez.Builtin("with-region", toChez(body))

    case other => chez.Let(Nil, toChez(other))
  }

  def toChez(decl: lifted.Decl): List[chez.Def] = decl match {
    case Data(did, ctors) =>
      ctors.flatMap {
        case ctor: symbols.Constructor => generateConstructor(ctor, ctor.fields)
        case other => sys error s"Wrong type, expected constructor but got: ${ other }"
      }

    case Record(did, fields) =>
      generateConstructor(did, fields)

    // We use chez scheme records to also represent capabilities.
    case Decl.Interface(id, operations) =>
      generateConstructor(id, operations)
  }

  def toChez(decl: lifted.Extern): chez.Def = decl match {
    case Extern.Def(id, tpe, params, body) =>
      chez.Constant(nameDef(id),
        chez.Lambda(params map { p => ChezName(p.id.name.name) },
          chez.RawExpr(body)))

    case Extern.Include(contents) =>
      RawDef(contents)
  }

  def toChez(defn: Definition): Either[chez.Def, Option[chez.Expr]] = defn match {
    case Definition.Def(id, tpe, block) =>
      Left(chez.Constant(nameDef(id), toChez(block)))

    case Definition.Let(Wildcard(_), tpe, binding) =>
      toChez(binding) match {
        // drop the binding altogether, if it is of the form:
        //   let _ = myVariable; BODY
        // since this might lead to invalid scheme code.
        case _: chez.Variable => Right(None)
        case other => Right(Some(other))
      }

    // we could also generate a let here...
    case Definition.Let(id, tpe, binding) =>
      Left(chez.Constant(nameDef(id), toChez(binding)))
  }

  def toChez(stmt: Stmt): chez.Block = stmt match {

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
    case BlockLit(params, body) =>
      chez.Lambda(params map toChez, toChez(body))
  }

  def toChez(block: Block): chez.Expr = block match {
    case BlockVar(id) =>
      Variable(nameRef(id))

    case b @ BlockLit(params, body) => toChez(b)

    case Member(b, field) =>
      chez.Call(Variable(nameRef(field)), List(toChez(b)))

    case Unbox(e) =>
      toChez(e)

    case New(Implementation(id, clauses)) =>
      val ChezName(name) = nameRef(id)
      chez.Call(Variable(ChezName(s"make-${name}")), clauses.map { case Operation(_, block) => toChez(block) })
  }

  def toChez(scope: Evidence): chez.Expr = scope match {
    case Evidence(Nil) => Variable(ChezName("here"))
    case Evidence(scopes) => chez.Builtin("nested", scopes map { s => chez.Variable(nameRef(s)) }:_*)
  }

  def toChez(expr: Expr): chez.Expr = expr match {
    case Literal((), _) => chez.RawValue("#f")
    case Literal(s: String, _) => ChezString(s)
    case Literal(b: Boolean, _) => if (b) chez.RawValue("#t") else chez.RawValue("#f")
    case l: Literal => chez.RawValue(l.value.toString)
    case ValueVar(id)  => chez.Variable(nameRef(id))

    case PureApp(b, targs, args) => chez.Call(toChez(b), args map {
      case e: Expr  => toChez(e)
      case b: Block => toChez(b)
      case e: Evidence => toChez(e)
    })

    case Select(b, field) =>
      chez.Call(nameRef(field), toChez(b))

    case Box(b) => toChez(b)

    case Run(s, tpe) => run(toChezExpr(s))
  }
}
