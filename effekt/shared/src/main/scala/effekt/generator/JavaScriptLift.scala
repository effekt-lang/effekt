package effekt
package generator

import effekt.context.Context
import effekt.core.*
import effekt.symbols.{ Module, Wildcard }
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import effekt.util.paths.*

import scala.language.implicitConversions

object JavaScriptLift extends Backend {

  val prettyPrinter: JavaScriptLiftPrinter = new JavaScriptLiftPrinter {}

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
    C.using(module = input.mod) { Some(prettyPrinter.format(input.core)) }

  /**
   * Compiles only the given module, does not compile dependencies
   */
  private def compile(in: CoreTransformed)(implicit C: Context) =
    LiftInference(in).map { lifted =>
      val doc = prettyPrinter.format(lifted.core)
      (path(in.mod), doc)
    }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / prettyPrinter.moduleFile(m.path)).unixPath
}

/**
 * A JavaScript PrettyPrinter that generates code using the explicit
 * lift evidence.
 *
 * Requires a different stdlib.
 *
 * Use like: effekt --generator jslift --compile --lib libraries/js/lift test.effekt
 */
trait JavaScriptLiftPrinter extends JavaScriptBase {

  def toDoc(b: Block)(implicit C: Context): Doc = link(b, b match {
    case BlockVar(v) =>
      nameRef(v)
    case BlockLit(ps, body) =>
      jsLambda(ps map toDoc, toDoc(body))
    case Member(b, id) =>
      toDoc(b) <> "." <> nameDef(id)
    case Extern(ps, body) =>
      jsLambda(ps map toDoc, body)

    case ScopeApp(b, sc) => jsCall(toDoc(b), List(toDoc(sc)))
    case ScopeAbs(id, b) => jsLambda(List(nameDef(id)), toDoc(b))
    case Lifted(ev, b)   => jsCall("$effekt.liftBlock", List(toDoc(ev), toDoc(b)))
    case Unbox(e)        => toDoc(e)
  })

  // pretty print the statement in a javascript expression context
  // not all statement types can be printed in this context!
  def toDocExpr(s: Stmt)(implicit C: Context): Doc = s match {

    case Val(Wildcard(_), tpe, binding, body) =>
      "$effekt.then" <> parens(toDocExpr(binding)) <> parens(jsLambda(Nil, toDoc(body)))

    case Val(id, tpe, binding, body) =>
      "$effekt.then" <> parens(toDocExpr(binding)) <> parens(jsLambda(List(nameDef(id)), toDoc(body)))

    case App(b, targs, args) =>
      jsCall(toDoc(b), args map argToDoc)

    case If(cond, thn, els) =>
      parens(toDoc(cond)) <+> "?" <+> toDocExpr(thn) <+> ":" <+> toDocExpr(els)

    case While(cond, body) =>
      jsCall(
        "$effekt._while", toDocExpr(cond), toDocExpr(body)
      )

    case Ret(e) =>
      jsCall("$effekt.pure", toDoc(e))

    case State(id, tpe, get, put, init, body) =>
      "$effekt.state" <> parens(toDocExpr(init)) <> parens(toDoc(body))

    case Handle(body, hs) =>
      val handlers = hs map { handler => jsObject(handler.clauses.map { case (id, b) => nameDef(id) -> toDoc(b) }) }
      val cs = parens(jsArray(handlers))
      "$effekt.handle" <> cs <> parens(nest(line <> toDoc(body)))

    case Match(sc, clauses) =>
      val cs = jsArray(clauses map {
        case (pattern, b) => jsObject(
          text("pattern") -> toDoc(pattern),
          text("exec") -> toDoc(b)
        )
      })
      jsCall("$effekt.match", toDoc(sc), cs)

    case Hole =>
      jsCall("$effekt.hole", Nil)

    case Exports(path, exports) =>
      jsCall(
        "module.exports = Object.assign",
        jsModuleName(path),
        jsObject(exports.map { e => toDoc(e.name) -> toDoc(e.name) })
      )

    case other =>
      sys error s"Cannot print ${other} in expression position"
  }

  override def toDocStmt(s: Stmt)(implicit C: Context): Doc = s match {
    case Def(id, tpe, ScopeAbs(sc, BlockLit(ps, body)), rest) =>
      jsFunction(nameDef(id), List(nameDef(sc)),
        "return" <+> jsLambda(ps map toDoc, toDoc(body))) <> emptyline <> toDocStmt(rest)
    case _ => super.toDocStmt(s)
  }

  override def toDocTopLevel(s: Stmt)(implicit C: Context): Doc = s match {
    case Def(id, tpe, ScopeAbs(sc, BlockLit(ps, body)), rest) =>
      jsFunction(nameDef(id), List(nameDef(sc)),
        "return" <+> jsLambda(ps map toDoc, toDoc(body))) <> emptyline <> toDocTopLevel(rest)
    case _ => super.toDocTopLevel(s)
  }

  def toDoc(a: Scope)(implicit C: Context): Doc = a match {
    case Here() => "$effekt.here"
    case Nested(scopes) =>
      val ss: List[Doc] = scopes.map(a => toDoc(a))
      jsCall("$effekt.nested", ss)
    case ScopeVar(id) => id.name.toString
  }
}
