package effekt
package generator
package js

import effekt.context.Context
import effekt.context.assertions.*
import effekt.core.*
import effekt.symbols.{ LocalName, Module, Name, NoName, QualifiedName, Symbol, Wildcard }
import effekt.util.paths.*
import effekt.{ Compiled, CoreTransformed, symbols }
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

import scala.language.implicitConversions

// used by the website
object JavaScriptVirtual extends JavaScript {
  def jsModuleSystem(module: js.Module): List[js.Stmt] = module.virtual
}

// used by REPL and compiler
object JavaScriptMonadic extends JavaScript {
  def jsModuleSystem(module: js.Module): List[js.Stmt] = module.commonjs
}

trait JavaScript extends Backend {

  def jsModuleSystem(module: js.Module): List[js.Stmt]

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(implicit C: Context) = {
    val compiledDependencies = dependencies.flatMap { dep => compile(dep) }.toMap
    compile(main).map {
      case (mainFile, result) =>
        Compiled(mainFile, compiledDependencies.updated(mainFile, result))
    }
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: CoreTransformed)(implicit C: Context) =
    C.using(module = input.mod) {
      Some(js.PrettyPrinter.format(jsModuleSystem(toJS(input.core))))
    }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / jsModuleFile(m.path)).unixPath

  /**
   * Compiles only the given module, does not compile dependencies
   *
   * Writes the compiled result into a file.
   */
  private def compile(in: CoreTransformed)(implicit C: Context): Option[(String, Document)] =
    compileSeparate(in).map { doc =>
      (path(in.mod), doc)
    }


  // Names
  // -----

  val reserved = List(
    // reserved words (according to https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#keywords)
    "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete", "do", "else", "export",
    "extends", "false", "finally", "for", "function", "if", "import", "in", "instanceof", "let", "new", "null", "return",
    "static", "super", "switch", "this", "throw", "true", "try", "typeof", "var", "void", "while", "with", "yield",

    // future reserved words
    "enum", "implements", "interface", "package", "private", "protected", "public",

    // identifiers with special meanings
    "get", "set", "arguments", "async", "eval",

    // special names in CommonJS module systems
    "module", "exports", "require",

    // other special names
    "window", "document", "alert", "console", "this"
  )

  def jsEscape(name: String): String = if (reserved contains name) "$" + name else name

  def jsModuleName(path: String): String = "$" + path.replace('/', '_')

  def jsModuleFile(path: String): String = path.replace('/', '_') + ".js"

  def toJSName(s: String): JSName = JSName(jsEscape(s))

  val `pattern` = JSName("pattern")
  val `exec` = JSName("exec")
  val `fresh` = JSName("fresh")
  val `tag` = JSName("__tag")
  val `data` = JSName("__data")

  def nameDef(id: Symbol): JSName = JSName(jsEscape(id match {
    case b: symbols.BlockParam if b.tpe.isInstanceOf[symbols.InterfaceType] => id.name.toString + "_" + id.id
    case _: symbols.Operation => "op$" + id.name.toString
    case _ => id.name.toString
  }))

  def nameRef(id: Symbol)(using C: Context): js.Expr = {
    def ref(name: String): js.Expr = Variable(JSName(jsEscape(name)))
    id match {
      case b: symbols.BlockParam if b.tpe.isInstanceOf[symbols.InterfaceType] => ref(id.name.toString + "_" + id.id)
      case _: symbols.Operation => ref("op$" + id.name.toString)
      case _: symbols.Interface => ref(id.name.name)
      case _: symbols.Field => ref(id.name.name)
      case _ => id.name match {
        case LocalName(name) => ref(name)
        case QualifiedName(Nil, name) => ref(name)
        // TODO this is rather fragile...
        case QualifiedName(path, name) if C.module.path == path.mkString("/") => ref(name)
        case QualifiedName(path, name) =>
          val prefix = ref("$" + path.mkString("_"))
          val member = JSName(jsEscape(name))
          js.Member(prefix, member)
        case NoName => sys error "Trying to generate code for an anonymous entity"
      }
    }
  }

  // name references for fields and methods
  def memberNameRef(id: Symbol): JSName = JSName(jsEscape(id match {
    case _: symbols.Operation => "op$" + id.name.toString
    case _: symbols.Field => id.name.name
    case _ => sys error "Trying to generate a member reference for something that is not a field or operation!"
  }))

  def toJS(p: Param): JSName = nameDef(p.id)

  // For externs, do not sanitize anything. We assume the programmer
  // knows what they are doing.
  def externParams(p: Param)(using C: Context): JSName = {
    val name = p.id.name.name
    if (reserved contains name) {
      C.warning(s"Identifier '${name}' is used in an extern function, but is a JS reserved keyword.")
    }
    JSName(name)
  }

  def toJS(b: core.Block)(using Context): js.Expr = b match {
    case BlockVar(v) =>
      nameRef(v)
    case BlockLit(ps, body) =>
      val (stmts, ret) = toJSStmt(body)
      monadic.Lambda(ps map toJS, stmts, ret) // TODO
    case Member(b, id) =>
      js.Member(toJS(b), memberNameRef(id))
    case Extern(ps, body) =>
      js.Lambda(ps map externParams, js.RawExpr(body))
    case Unbox(e)     => toJS(e)
    case New(handler) => toJS(handler)
  }

  def toJS(pattern: core.Pattern)(using Context): js.Expr = pattern match {
    case IgnorePattern() => builtin("ignore")
    case AnyPattern() => builtin("any")
    case LiteralPattern(l) => js.Call(builtin("literal"), List(toJS(l)))
    case TagPattern(id, patterns) =>
      val tag = JsString(nameDef(id).name) // TODO improve
      val childMatchers = patterns map toJS
      js.Call(builtin("tagged"), tag :: childMatchers)
  }

  def builtin(name: String): js.Expr = js.Member($effekt, JSName(name))

  def toJS(args: List[Argument])(using Context): List[js.Expr] = args map {
    case b: Block => toJS(b)
    case e: Expr => toJS(e)
  }

  def toJS(expr: core.Expr)(using Context): js.Expr = expr match {
    case UnitLit() => builtin("unit")
    case StringLit(s) => JsString(s)
    case literal: Literal[_] => js.RawExpr(literal.value.toString)
    case ValueVar(id) => nameRef(id)
    case DirectApp(b, targs, args) => js.Call(toJS(b), toJS(args))
    case PureApp(b, targs, args) => js.Call(toJS(b), args map toJS)
    case Select(target, field) => js.Member(toJS(target), memberNameRef(field))
    case Box(b) => toJS(b)
    case Run(s, tpe) => monadic.Run(toJSMonadic(s))
  }

  def toJS(handler: core.Handler)(using Context): js.Expr =
    js.Object(handler.clauses.map { case (id, b) => nameDef(id) -> toJS(b) })

  // TODO
  //  def toJSMonadic(s: core.Stmt)(using Context, Buffer[js.Stmt]): monadic.Control = s match {

  def toJS(module: core.ModuleDecl)(using Context): js.Module = {
    val name    = JSName(jsModuleName(module.path))
    val imports = module.imports.map { i => js.Import(JSName(jsModuleName(i)), jsModuleFile(i)) }
    val exports = module.exports.map { e => js.Export(nameDef(e), nameRef(e)) }
    val (stmts, _) = toJSStmt(module.defs)

    js.Module(name, imports, exports, stmts)
  }


  /**
   * Translate the statement to a javascript expression in a monadic expression context.
   *
   * Not all statement types can be printed in this context!
   */
  def toJSMonadic(s: core.Stmt)(using Context): monadic.Control = s match {
    case core.Val(Wildcard(_), tpe, binding, body) =>
      monadic.Bind(toJSMonadic(binding), toJSMonadic(body))

    case core.Val(id, tpe, binding, body) =>
      monadic.Bind(toJSMonadic(binding), nameDef(id), toJSMonadic(body))

    case core.App(b, targs, args) =>
      monadic.Call(toJS(b), toJS(args))

    case core.If(cond, thn, els) =>
      monadic.If(toJS(cond), toJSMonadic(thn), toJSMonadic(els))

    case core.While(cond, body) =>
      monadic.While(toJSMonadic(cond), toJSMonadic(body))

    case core.Return(e) =>
      monadic.Pure(toJS(e))

    case core.Handle(body, tpe, hs) =>
      monadic.Handle(hs map toJS, toJS(body))

    case core.Region(body) =>
      monadic.Builtin("withRegion", toJS(body))

    case core.Match(sc, clauses) =>
      val cs = js.ArrayLiteral(clauses map {
        case (p, b) => js.Object(`pattern` -> toJS(p), `exec` -> toJS(b))
      })
      monadic.Builtin("match", toJS(sc), cs)

    case core.Hole =>
      monadic.Builtin("hole")

    case other => toJSStmt(other) match {
      case (Nil, ret) => ret
      case (stmts, ret) => monadic.Call(monadic.Lambda(Nil, stmts, ret), Nil)
    }
  }

  /**
   * Translate the statement in a js "direct-style" statement context.
   *
   * That is, multiple statements that end in one monadic return
   */
  def toJSStmt(s: core.Stmt)(using Context): (List[js.Stmt], monadic.Control) = s match {
    case Def(id, tpe, BlockLit(ps, body), rest) =>
      val (restStmts, ret) = toJSStmt(rest)
      val (stmts, jsBody) = toJSStmt(body)
      (monadic.Function(nameDef(id), ps map toJS, stmts, jsBody) :: restStmts, ret)

    case Def(id, tpe, Extern(ps, body), rest) =>
      val (stmts, ret) = toJSStmt(rest)
      (js.Function(nameDef(id), ps map externParams, List(js.Return(js.RawExpr(body)))) :: stmts, ret)

    case Def(id, tpe, block, rest) =>
      val (stmts, ret) = toJSStmt(rest)
      (js.Const(nameDef(id), toJS(block)) :: stmts, ret)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { ctor => generateConstructor(ctor.asConstructor) }
      val (stmts, ret) = toJSStmt(rest)
      (cs ++ stmts, ret)

    case Record(did, fields, rest) =>
      val (stmts, ret) = toJSStmt(rest)
      (generateConstructor(did, fields) :: stmts, ret)

    case Include(contents, rest) =>
      val (stmts, ret) = toJSStmt(rest)
      (js.RawStmt(contents) :: stmts, ret)

    case Let(Wildcard(_), tpe, binding, body) =>
      val (stmts, ret) = toJSStmt(body)
      (js.ExprStmt(toJS(binding)) :: stmts, ret)

    case Let(id, tpe, binding, body) =>
      val (stmts, ret) = toJSStmt(body)
      (js.Const(nameDef(id), toJS(binding)) :: stmts, ret)

    case State(id, init, region, body) if region == symbols.builtins.globalRegion =>
      val (stmts, ret) = toJSStmt(body)
      (js.Const(nameDef(id), js.MethodCall($effekt, `fresh`, toJS(init))) :: stmts, ret)

    case State(id, init, region, body) =>
      val (stmts, ret) = toJSStmt(body)
      (js.Const(nameDef(id), js.MethodCall(nameRef(region), `fresh`, toJS(init))) :: stmts, ret)

    case other =>
      (Nil, toJSMonadic(other))
  }

  def generateConstructor(ctor: symbols.Record): js.Stmt =
    generateConstructor(ctor, ctor.fields)

  def generateConstructor(ctor: Symbol, fields: List[Symbol]): js.Stmt =
    js.Function(
      nameDef(ctor),
      fields.map { f => nameDef(f) },
      List(js.Return(js.Object(List(
        `tag`  -> JsString(nameDef(ctor).name), // TODO improve
        `data` -> js.ArrayLiteral(fields map { f => Variable(nameDef(f)) })
      ) ++ fields.map { f => (nameDef(f), Variable(nameDef(f))) })))
    )
}
