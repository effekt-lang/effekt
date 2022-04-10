package effekt.generator

import effekt.context.Context
import effekt.core._
import effekt.symbols.{ Module, Wildcard }
import org.bitbucket.inkytonik.kiama
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

import effekt.util.paths._

import scala.language.implicitConversions

class JavaScriptLift extends Generator {

  val prettyPrinter: JavaScriptLiftPrinter = new JavaScriptLiftPrinter {}

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / prettyPrinter.moduleFile(m.path)).unixPath

  /**
   * This is only called on the main entry point, we have to manually traverse the dependencies
   * and write them.
   */
  def run(src: Source)(implicit C: Context): Option[Document] = for {
    mod <- C.frontend(src)
    _ = mod.dependencies.flatMap(compile)
    doc <- compile(mod)
  } yield doc

  /**
   * Compiles only the given module, does not compile dependencies
   */
  def compile(mod: Module)(implicit C: Context): Option[Document] = for {
    core <- C.backend(mod.source)
    // setting the scope to mod is important to generate qualified names
    doc = C.using(module = mod) { prettyPrinter.format(core) }
    _ = C.saveOutput(doc.layout, path(mod))
  } yield doc
}

/**
 * A JavaScript PrettyPrinter that generates code using the explicit
 * lift evidence.
 *
 * Requires a different stdlib.
 *
 * Use like: effekt --generator jslift --compile --lib lift/lib test.effekt
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
