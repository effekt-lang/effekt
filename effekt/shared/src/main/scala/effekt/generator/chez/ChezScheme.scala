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
    C.checkMain(main.mod)
    val deps = dependencies.flatMap { dep => toChez(dep.core) }
    val chezModule = chez.Let(Nil, compilationUnit(main.mod, main.core, deps))
    val result = chez.PrettyPrinter.pretty(chez.PrettyPrinter.toDoc(chezModule), 100)
    val mainFile = path(main.mod)
    Some(Compiled(mainFile, Map(mainFile -> result)))
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: CoreTransformed)(using C: Context) =
    C.using(module = input.mod) { Some(compile(input)) }

  /**
   * Compiles only the given module, does not compile dependencies
   */
  private def compile(in: CoreTransformed)(using Context): Document =
    chez.PrettyPrinter.format(toChez(in.core))

  def compilationUnit(mod: Module, core: ModuleDecl, dependencies: List[chez.Def])(implicit C: Context): chez.Block = {
    val defs = toChez(core)
    val mainSymbol = mod.terms("main").toList.head
    chez.Block(generateStateAccessors ++ dependencies ++ defs, Nil, runMain(nameRef(mainSymbol)))
  }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"


  def toChez(p: Param): ChezName = nameDef(p.id)

  def toChez(e: core.Argument): chez.Expr = e match {
    case e: core.Expr  => toChez(e)
    case b: core.Block => toChez(b)
  }

  def toChez(module: ModuleDecl): List[chez.Def] = {
    toChez(module.defs).definitions // TODO FIXME, once there is a let _ = ... in there, we are doomed!
  }

  def toChezExpr(stmt: Stmt): chez.Expr = stmt match {
    case Ret(e) => pure(toChez(e))
    case App(b, targs, args) => chez.Call(toChez(b), args map toChez)
    case If(cond, thn, els) => chez.If(toChez(cond), toChezExpr(thn), toChezExpr(els))
    case Val(id, tpe, binding, body) => bind(toChezExpr(binding), nameDef(id), toChez(body))
    case While(cond, body) => chez.Builtin("while", toChezExpr(cond), toChezExpr(body))
    case Match(scrutinee, clauses) =>
      chez.Match(toChez(scrutinee), clauses.map { case (pattern, branch) =>
        (toChez(pattern), curry(toChez(branch)))
      })

    case Hole => ???

    case State(id, init, region, body) if region == symbols.builtins.globalRegion =>
      chez.Let(List(Binding(nameDef(id), chez.Builtin("box", toChez(init)))), toChez(body))

    case State(id, init, region, body) =>
      chez.Let(List(Binding(nameDef(id), chez.Builtin("fresh", Variable(nameRef(region)), toChez(init)))), toChez(body))

    case Handle(body, handler) =>
      val handlers: List[chez.Handler] = handler.map { h =>
        val names = RecordNames(h.id)
        chez.Handler(names.constructor, h.clauses.map {
          case (op, BlockLit(params, body)) =>
            // the LAST argument is the continuation...
            chez.Operation(nameDef(op), params.init.map(p => nameDef(p.id)), nameDef(params.last.id), toChezExpr(body))
        })
      }
      chez.Handle(handlers, toChez(body))

    case Region(body) => chez.Builtin("with-region", toChez(body))

    case other => chez.Let(Nil, toChez(other))
  }

  def toChez(stmt: Stmt): chez.Block = stmt match {

    case Def(id, tpe, block, rest) =>
      val Block(defs, exprs, result) = toChez(rest)
      val constDef = chez.Constant(nameDef(id), toChez(block))
      Block(constDef :: defs, exprs, result)

    case Data(did, ctors, rest) =>
      val Block(defs, exprs, result) = toChez(rest)
      val constructors = ctors.flatMap(ctor => generateConstructor(ctor.asInstanceOf[effekt.symbols.Record]))
      Block(constructors ++ defs, exprs, result)

    case Record(did, fields, rest) =>
      val Block(defs, exprs, result) = toChez(rest)
      val constructors = generateConstructor(did, fields)
      Block(constructors ++ defs, exprs, result)

    case Include(contents, rest) =>
      val include = RawDef(contents)
      val chez.Block(defs, exprs, result) = toChez(rest)
      chez.Block(include :: defs, exprs, result)

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

    case Extern(params, body) =>
      chez.Lambda(params map { p => ChezName(p.id.name.name) }, chez.RawExpr(body))

    case Unbox(e) =>
      toChez(e)

    case New(Handler(id, clauses)) =>
      val ChezName(name) = nameRef(id)
      chez.Call(Variable(ChezName(s"make-${name}")), clauses.map { case (_, block) => toChez(block) })
  }

  def toChez(expr: Expr): chez.Expr = expr match {
    case UnitLit()     => chez.RawExpr("#f")
    case StringLit(s)  => ChezString(s)
    case BooleanLit(b) => if (b) chez.RawExpr("#t") else chez.RawExpr("#f")
    case l: Literal[t] => chez.RawExpr(l.value.toString)
    case ValueVar(id)  => chez.Variable(nameRef(id))

    case PureApp(b, targs, args) => chez.Call(toChez(b), args map {
      case e: Expr  => toChez(e)
      case b: Block => toChez(b)
    })

    case Select(b, field) =>
      chez.Call(nameRef(field), toChez(b))

    case Box(b) => toChez(b)

    case Run(s) => run(toChezExpr(s))
  }

  def toChez(p: Pattern): chez.Expr = p match {
    case IgnorePattern()    => Variable(ChezName("ignore"))
    case AnyPattern()       => Variable(ChezName("any"))
    case LiteralPattern(l)  => Builtin("literal", toChez(l))
    case TagPattern(id, ps) => Builtin("match-" + uniqueName(id), ps map { p => toChez(p) }: _*)
  }
}
