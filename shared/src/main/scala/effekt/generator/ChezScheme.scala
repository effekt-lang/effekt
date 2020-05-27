package effekt.generator

import effekt.context.Context
import effekt.core._
import effekt.symbols.Module
import effekt.symbols.{ Name, Symbol }

import org.bitbucket.inkytonik.kiama
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import effekt.context.assertions._

import scala.language.implicitConversions

import effekt.util.paths._

/**
 * It would be nice if Core could have an Effect Declaration or
 * translate effect declarations to Records...
 */
class ChezScheme extends Generator {

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
    mod <- C.frontend(src)
    _ = C.checkMain(mod)
    deps = mod.dependencies.flatMap(dep => compile(dep))
    core <- C.lower(src)
    result = ChezSchemePrinter.compilationUnit(mod, core, deps)
    _ = C.saveOutput(result.layout, path(mod))
  } yield result

  /**
   * Compiles only the given module, does not compile dependencies
   */
  def compile(mod: Module)(implicit C: Context): Option[Document] = for {
    core <- C.lower(mod.source)
    doc = ChezSchemePrinter.format(core)
  } yield doc
}

object ChezSchemePrinter extends ParenPrettyPrinter {

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  val prelude = "#!/usr/local/bin/scheme --script\n\n(import (chezscheme))\n\n"

  def moduleFile(path: String): String = path.replace('/', '_') + ".ss"

  def compilationUnit(mod: Module, core: ModuleDecl, dependencies: List[Document])(implicit C: Context): Document =
    pretty {

      val main = mod.terms("main").toList.head

      prelude <>
        vsep(dependencies.map { m => string(m.layout) }) <>
        module(core) <> emptyline <>
        "(runCC " <> nameRef(main) <> ")"
    }

  def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(module(t))

  val emptyline: Doc = line <> line

  def module(m: ModuleDecl)(implicit C: Context): Doc = {
    //    vsep(m.imports.map { im => schemeCall("load", jsString(moduleFile(im))) }, line) <> emptyline <>
    toDoc(m)
  }

  // TODO print all top level value declarations as "var"
  def toDoc(m: ModuleDecl)(implicit C: Context): Doc =
    toDoc(m.defs)

  def toDoc(b: Block)(implicit C: Context): Doc = link(b, b match {
    case BlockVar(v) =>
      nameRef(v)
    case BlockDef(ps, body) =>
      schemeLambda(ps map toDoc, toDoc(body))
    case Member(b, id) =>
      schemeCall(nameRef(id), toDoc(b))
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
    case BooleanLit(b) => if (b) "#t" else "#f"
    case l: Literal[t] => l.value.toString
    case ValueVar(id)  => nameRef(id)

    case PureApp(b, args) => schemeCall(toDoc(b), args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
      case s: Scope => toDoc(s)
    })

    case Select(b, field) =>
      schemeCall(nameRef(field), toDoc(b))
  })

  def argToDoc(e: Argument)(implicit C: Context): Doc = e match {
    case e: Expr  => toDoc(e)
    case b: Block => toDoc(b)
    case s: Scope => toDoc(s)
  }

  def toDoc(s: Scope)(implicit C: Context): Doc = ???

  def toDoc(s: Stmt)(implicit C: Context): Doc = s match {

    case If(cond, thn, els) =>
      parens("if" <+> toDoc(cond) <+> toDoc(thn) <+> toDoc(els))

    case While(cond, body) =>
      parens("while" <+> toDoc(cond) <+> toDoc(body))

    case Def(id, BlockDef(ps, body), rest) =>
      defineFunction(nameDef(id), ps map toDoc, toDoc(body)) <> emptyline <> toDoc(rest)

    // we can't use the unique id here, since we do not know it in the extern string.
    case Def(id, Extern(ps, body), rest) =>
      defineFunction(nameDef(id), ps.map { p => p.id.name.toString }, body) <> emptyline <> toDoc(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { ctor => generateConstructor(ctor.asConstructor) }
      vsep(cs) <> emptyline <> toDoc(rest)

    case Record(did, fields, rest) =>
      generateConstructor(did, fields) <> emptyline <> toDoc(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDoc(rest)

    case App(b, args) => schemeCall(toDoc(b), args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
      case s: Scope => toDoc(s)
    })

    case Val(Wildcard(_), binding, body) =>
      toDoc(binding) <> line <> toDoc(body)

    case Val(id, binding, body) =>
      parens("let" <+> parens(parens(nameDef(id) <+> toDoc(binding))) <+> group(nest(line <> toDoc(body))))

    case Ret(e) => toDoc(e)

    case Handle(body, handler: List[Handler]) =>
      val handlers: List[Doc] = handler.map { h =>

        brackets(nameRef(h.id) <+> vsep(h.clauses.map {
          // impl now is not a BlockDef anymore, but a potentially lifted block
          case (op, impl) =>
            // the LAST argument is the continuation...
            val params = impl.params.init
            val kParam = impl.params.last

            parens(nameDef(op) <+>
              parens(hsep(params.map { p => nameRef(p.id) }, space)) <+>
              nameRef(kParam.id) <+>
              toDoc(impl.body))
        }, line))
      }
      parens("handle" <+> parens(vsep(handlers)) <+> toDoc(body))

    case Match(sc, cls) =>
      val clauses: List[Doc] = cls map {

        // curry the block
        case (pattern, b: BlockDef) =>
          brackets(toDoc(pattern) <+> b.params.foldLeft(toDoc(b.body)) {
            case (body, p) => schemeLambda(List(nameDef(p.id)), body)
          })

        case (pattern, b) => sys error "Right hand side of a matcher needs to be a block definition"
      }
      schemeCall("pattern-match", toDoc(sc) :: parens(vsep(clauses, line)) :: Nil)

    // only export main for now
    case Exports(path, exports) =>
      exports.find(e => e.name.name == "main").map { main =>
        defineValue("main", nameDef(main))
      }.getOrElse("")

    case other => other.toString
  }

  def toDoc(p: Pattern)(implicit C: Context): Doc = p match {
    case IgnorePattern()    => "ignore"
    case AnyPattern()       => "any"
    case LiteralPattern(l)  => schemeCall("literal", toDoc(l))
    case TagPattern(id, ps) => schemeCall("match-" <> nameDef(id), ps map { p => toDoc(p) })
  }

  def generateConstructor(ctor: effekt.symbols.Record)(implicit C: Context): Doc =
    generateConstructor(ctor, ctor.fields)

  // https://www.scheme.com/csug8/objects.html
  // https://scheme.com/tspl4/records.html
  def generateConstructor(did: Symbol, fields: List[Symbol])(implicit C: Context): Doc = {
    val pred = nameDef(did) <> "?"
    val matcher = "match-" <> nameDef(did)

    val definition =
      parens("define-record-type" <+> parens(nameDef(did) <+> nameDef(did) <+> pred) <>
        nest(line <> parens("fields" <+> nest(line <> vsep(fields.map { f => parens("immutable" <+> nameDef(f) <+> nameDef(f)) }))) <> line <>
          parens("nongenerative" <+> nameDef(did))))

    var fresh = 0;

    val matcherDef =
      parens("define-matcher" <+> matcher <+> pred <>
        nest(line <> parens(vsep(fields.map { f =>
          fresh += 1
          brackets(s"p${fresh}" <+> nameDef(f))
        }))))

    s";;; Record definition for ${did.name}" <> line <> definition <> line <> matcherDef
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
