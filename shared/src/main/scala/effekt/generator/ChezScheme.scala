package effekt.generator

import effekt.context.Context
import effekt.core._
import effekt.symbols.Module
import effekt.symbols.{ Name, Symbol }

import org.bitbucket.inkytonik.kiama
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

import scala.language.implicitConversions

import effekt.util.paths._

/**
 * It would be nice if Core could have an Effect Declaration or
 * translate effect declarations to Records...
 */
class ChezScheme extends Generator with ParenPrettyPrinter {

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"

  /**
   * This is only called on the main entry point, we have to manually traverse the dependencies
   * and write them.
   */
  def run(src: Source)(implicit C: Context): Option[Document] = for {
    core <- C.lower(src)
    mod <- C.frontend(src)
    _ = mod.dependencies.flatMap(compile)
    doc <- compile(mod)
  } yield doc

  /**
   * Compiles only the given module, does not compile dependencies
   */
  def compile(mod: Module)(implicit C: Context): Option[Document] = for {
    core <- C.lower(mod.source)
    doc = ChezSchemePrinter.format(core)
    _ = C.saveOutput(doc, path(mod))
  } yield doc
}

object ChezSchemePrinter extends ParenPrettyPrinter {

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  def moduleFile(path: String): String = path.replace('/', '_') + ".ss"

  def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(module(t))

  val emptyline: Doc = line <> line

  def module(m: ModuleDecl)(implicit C: Context): Doc = {
    vsep(m.imports.map { im => schemeCall("load", jsString(moduleFile(im))) }, line) <> emptyline <>
      toDoc(m)
  }

  // TODO print all top level value declarations as "var"
  def toDoc(m: ModuleDecl)(implicit C: Context): Doc =
    toDocTopLevel(m.defs)

  def toDoc(b: Block)(implicit C: Context): Doc = link(b, b match {
    case BlockVar(v) =>
      nameRef(v)
    case BlockDef(ps, body) =>
      schemeLambda(ps map toDoc, toDoc(body))
    case Lift(b) =>
      schemeCall("$effekt.lift", toDoc(b))
    case Extern(ps, body) =>
      schemeLambda(ps map toDoc, body)
  })

  def toDoc(p: Param)(implicit C: Context): Doc = link(p, nameDef(p.id))

  def toDoc(n: Name)(implicit C: Context): Doc = link(n, n.toString)

  // we prefix op$ to effect operations to avoid clashes with reserved names like `get` and `set`
  def nameDef(id: Symbol)(implicit C: Context): Doc =
    id.name.toString + "_" + id.id

  def nameRef(id: Symbol)(implicit C: Context): Doc =
    id.name.toString + "_" + id.id

  def toDoc(e: Expr)(implicit C: Context): Doc = link(e, e match {
    case UnitLit()     => "#f"
    case StringLit(s)  => jsString(s)
    case l: Literal[t] => l.value.toString
    case ValueVar(id)  => nameRef(id)

    // we use mutable state for now.
    case Deref(id)     => nameRef(id)
    case Assign(id, e) => schemeCall("set!", List(nameRef(id), toDoc(e)))

    case PureApp(b, args) => schemeCall(toDoc(b), args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
    })

    case Select(b, field) =>
      schemeCall(nameRef(field), toDoc(b))
  })

  def argToDoc(e: Argument)(implicit C: Context): Doc = e match {
    case e: Expr  => toDoc(e)
    case b: Block => toDoc(b)
  }

  def toDoc(p: Pattern)(implicit C: Context): Doc = ";; NOT YET SUPPORTED "

  def toDoc(s: Stmt)(implicit C: Context): Doc = s match {

    case While(cond, body) =>
      parens("while" <+> toDoc(cond) <+> toDoc(body))

    case Def(id, BlockDef(ps, body), rest) =>
      defineFunction(nameDef(id), ps map toDoc, toDoc(body)) <> emptyline <> toDoc(rest)

    case Def(id, Extern(ps, body), rest) =>
      defineFunction(nameDef(id), ps map toDoc, body) <> emptyline <> toDoc(rest)

    case Var(id, binding, rest) =>
      defineValue(nameDef(id), toDoc(binding)) <> emptyline <> toDoc(rest)

    case Data(did, ctors, rest) => ";; NOT YET SUPPORTED "

    // (define-record point (x y))
    case Record(did, fields, rest) =>
      parens("define-record" <+> nameDef(did) <+> parens(hsep(fields.map { nameDef }, space))) <>
        emptyline <> toDoc(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDoc(rest)

    case App(b, args) => schemeCall(toDoc(b), args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
    })
    case Do(b, id, args) =>
      schemeCall(schemeCall(nameRef(id), List(toDoc(b))), args.map(argToDoc))

    case Val(Wildcard(_), binding, body) =>
      toDoc(binding) <> line <> toDoc(body)

    case Val(id, binding, body) =>
      parens("let" <+> parens(parens(nameDef(id) <+> toDoc(binding))) <+> group(nest(line <> toDoc(body))))

    case Ret(e) => toDoc(e)

    // TODO change to macro in Scheme
    case Handle(body, handler: List[Handler]) =>
      val handlers: List[Doc] = handler.map { h =>
        // call capability constructor
        schemeCall(nameRef(h.id), h.clauses.map {
          case (op, impl) =>
            // TODO the LAST argument is the continuation...
            val params = impl.params.init
            val kParam = impl.params.last
            schemeLambda(
              params.map { p => nameRef(p.id) },
              schemeCall("shift0-at", List(string("P"), nameRef(kParam.id), toDoc(impl.body)))
            )
        })
      }
      parens("let" <+>
        parens(brackets("P (newPrompt)")) <+>
        parens("pushPrompt" <+> "P" <+> schemeCall(toDoc(body), handlers)))

    //    case class Handler(id: Symbol, clauses: List[(Symbol, Block)])

    case other => other.toString
  }

  def toDocTopLevel(s: Stmt)(implicit C: Context): Doc = s match {
    case Def(id, BlockDef(ps, body), rest) =>
      defineFunction(nameDef(id), ps.map { p => nameDef(p.id) }, toDoc(body)) <> emptyline <> toDocTopLevel(rest)

    // we can't use the unique id here, since we do not know it in the extern string.
    case Def(id, Extern(ps, body), rest) =>
      defineFunction(nameDef(id), ps.map { p => p.id.name.toString }, body) <> emptyline <> toDocTopLevel(rest)

    case Val(id, binding, rest) => defineValue(nameDef(id), toDoc(binding)) <> emptyline <> toDocTopLevel(rest)
    case Var(id, binding, rest) => ";; VARS NOT YET SUPPORTED" <> emptyline <> toDocTopLevel(rest)
    case Data(id, ctors, rest)  => ";; DATA NOT YET SUPPORTED" <> emptyline <> toDocTopLevel(rest)

    // TODO add class=immutable and type to avoid boxing
    // https://www.scheme.com/csug8/objects.html
    case Record(did, fields, rest) =>
      s";;; Record definition for ${did.name}" <> line <>
        parens("define-record" <+> nameDef(did) <+> parens(hsep(fields.map { nameDef }, space))) <> line <>
        defineValue(nameDef(did), "make-" <> nameDef(did)) <> line <>
        vsep(fields.map { f =>
          defineValue(nameRef(f), nameRef(did) <> "-" <> nameRef(f))
        }) <>
        emptyline <> toDocTopLevel(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDocTopLevel(rest)

    case Exports(path, exports) =>
      vsep(exports.map { e =>
        defineValue(e.name.toString, nameDef(e))
      }, line)
    case _ => ";; NOT YET SUPPORTED"
  }

  def defineValue(name: Doc, binding: Doc) =
    parens("define" <+> name <+> binding)

  def defineFunction(name: Doc, params: List[Doc], body: Doc) =
    parens("define" <+> name <+> schemeLambda(params, body))

  def schemeLambda(params: List[Doc], body: Doc) =
    parens("lambda" <+> parens(hsep(params, space)) <> group(nest(line <> body)))

  def jsObject(fields: (Doc, Doc)*): Doc =
    jsObject(fields.toList)

  def jsObject(fields: List[(Doc, Doc)]): Doc =
    group(jsBlock(vsep(fields.map { case (n, d) => jsString(n) <> ":" <+> d }, comma)))

  def jsBlock(content: Doc): Doc = braces(nest(line <> content) <> line)

  def jsArray(els: List[Doc]): Doc =
    brackets(hsep(els, comma))

  def jsString(contents: Doc): Doc =
    "\"" <> contents <> "\""

  def schemeCall(fun: Doc, args: Doc*): Doc = schemeCall(fun, args.toList)
  def schemeCall(fun: Doc, args: List[Doc]): Doc = parens(hsep(fun :: args, space))

  def requiresBlock(s: Stmt): Boolean = s match {
    case Data(did, ctors, rest) => true
    case Def(id, d, rest) => true
    case _ => false
  }
}
