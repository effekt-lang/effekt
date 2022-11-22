package effekt
package generator
package chez

import effekt.context.Context
import effekt.core.*
import effekt.symbols.{ Module, Symbol, Wildcard }

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
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(using C: Context) = {
    val mainSym = C.checkMain(main.mod)
    val deps = dependencies.flatMap { dep => compile(dep) }
    val chezModule = cleanup(chez.Let(Nil, compilationUnit(mainSym, main.mod, main.core, deps)))
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

  def toChez(module: ModuleDecl): List[chez.Def] = {
    val decls = module.decls.flatMap(toChez)
    val externs = module.externs.map(toChez)
     // TODO FIXME, once there is a let _ = ... in there, we are doomed!
    val chez.Block(defns, _, _) = toChez(module.defs)
    decls ++ externs ++ defns
  }

  def toChez(args: List[Argument]): List[chez.Expr] = args map {
    case b: Block => toChez(b)
    case e: Pure => toChez(e)
  }

  def toChezExpr(stmt: Stmt): chez.Expr = stmt match {
    case Return(e) => pure(toChez(e))
    case App(b, targs, args) => chez.Call(toChez(b), toChez(args))
    case If(cond, thn, els) => chez.If(toChez(cond), toChezExpr(thn), toChezExpr(els))
    case Val(id, tpe, binding, body) => bind(toChezExpr(binding), nameDef(id), toChez(body))
    case While(cond, body) => chez.Builtin("while", toChezExpr(cond), toChezExpr(body))
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
        val names = RecordNames(h.interface)
        chez.Handler(names.constructor, h.operations.map {
          case Operation(op, BlockLit(params, body)) =>
            // the LAST argument is the continuation...
            chez.Operation(nameDef(op), params.init.map(p => nameDef(p.id)), nameDef(params.last.id), toChezExpr(body))
        })
      }
      chez.Handle(handlers, toChez(body))

    case Region(body) => chez.Builtin("with-region", toChez(body))

    case other => chez.Let(Nil, toChez(other))
  }

  def toChez(decl: core.Decl): List[chez.Def] = decl match {
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

  def toChez(decl: core.Extern): chez.Def = decl match {
    case Extern.Def(id, tpe, params, body) =>
      chez.Constant(nameDef(id),
        chez.Lambda(params map { p => ChezName(p.id.name.name) },
          chez.RawExpr(body)))

    case Extern.Include(contents) =>
      RawDef(contents)
  }

  def toChez(stmt: Stmt): chez.Block = stmt match {

    case Def(id, tpe, block, rest) =>
      val chez.Block(defs, exprs, result) = toChez(rest)
      val constDef = chez.Constant(nameDef(id), toChez(block))
      chez.Block(constDef :: defs, exprs, result)

    case Let(Wildcard(_), tpe, binding, body) =>
      toChez(binding) match {
        // drop the binding altogether, if it is of the form:
        //   let _ = myVariable; BODY
        // since this might lead to invalid scheme code.
        case _: chez.Variable => toChez(body)
        case _ => toChez(body) match {
          case chez.Block(Nil, exprs, result) => chez.Block(Nil, toChez(binding) :: exprs, result)
          case b => chez.Block(Nil, toChez(binding) :: Nil, chez.Let(Nil, b))
        }
      }

    // we could also generate a let here...
    case Let(id, tpe, binding, body) =>
      val chez.Block(defs, exprs, result) = toChez(body)
      val constant = chez.Constant(nameDef(id), toChez(binding))
      chez.Block(constant :: defs, exprs, result)

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

  def toChez(expr: Expr): chez.Expr = expr match {
    case UnitLit()     => chez.RawValue("#f")
    case StringLit(s)  => ChezString(s)
    case BooleanLit(b) => if (b) chez.RawValue("#t") else chez.RawValue("#f")
    case l: Literal[t] => chez.RawValue(l.value.toString)
    case ValueVar(id)  => chez.Variable(nameRef(id))

    case DirectApp(b, targs, args) => chez.Call(toChez(b), toChez(args))
    case PureApp(b, targs, args) => chez.Call(toChez(b), args map toChez)

    case Select(b, field) =>
      chez.Call(nameRef(field), toChez(b))

    case Box(b) => toChez(b)

    case Run(s, tpe) => run(toChezExpr(s))
  }
}
