package effekt
package generator
package chez

import effekt.context.Context
import effekt.core.*
import effekt.generator.chez
import effekt.symbols.{ Module, Symbol, Wildcard }
import effekt.symbols.builtins.{ TRegion, TState }
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

import scala.language.implicitConversions
import effekt.util.paths.*

object ChezSchemeMonadic extends Backend {

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(using Context) = {
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

    val main = chez.Constant(ChezName("main"), chez.Variable(nameRef(mainSymbol)))
    val run = monadic.Run(chez.Call(nameRef(mainSymbol)))

    chez.Block(generateStateAccessors ++ dependencies ++ (defs :+ main), Nil, run)
  }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"

  def uniqueName(id: Symbol): String = id.name.toString + "_" + id.id

  def nameRef(id: Symbol): ChezName = nameDef(id)

  // TODO sanitize
  def nameDef(id: Symbol): ChezName = ChezName(uniqueName(id))

  def intersperse[T](l: List[T], el: T): List[T] = l match {
    case Nil => Nil
    case head :: Nil => head :: Nil
    case head :: rest => head :: el :: intersperse(rest, el)
  }

  case class RecordNames(sym: Symbol) {
    val name = uniqueName(sym)
    val basename = sym.name.name
    val id = sym.id.toString

    val uid = ChezName(name)
    val typeName = ChezName(basename + "$Type" + id)
    val predicate = ChezName(name + "?")
    val constructor = sym match {
      case _: effekt.symbols.InterfaceType => ChezName(s"make-${name}")
      case _ => uid
    }
  }


  def generateConstructor(ctor: effekt.symbols.Record): List[chez.Def] =
    generateConstructor(ctor, ctor.fields)

  // https://www.scheme.com/csug8/objects.html
  // https://scheme.com/tspl4/records.html
  def generateConstructor(did: Symbol, fields: List[Symbol]): List[chez.Def] = {

    val names = RecordNames(did)

    // Record
    val record = chez.Record(names.typeName, names.constructor, names.predicate, names.uid, fields map nameDef)

    // Matcher
    def matcher = {

      var fresh = 0;
      val fieldNames = fields map { _ => fresh += 1; ChezName("p" + fresh) }
      val matcherName = ChezName("match-" + names.name)
      val sc = ChezName("sc")
      val matched = ChezName("matched")
      val failed = ChezName("failed")
      val k = ChezName("k")

      val matcherBody = fields match {

        // (define (<MATCHER-NAME>)
        //   (lambda (sc matched failed k)
        //     (if (pred sc) (k matched) (failed))))
        case Nil => chez.Call(k, Variable(matched))

        // (define (name p1 p2 ...)
        //   (lambda (sc matched failed k)
        //     ;; has correct tag?
        //     (if (pred sc)
        //       (match-fields sc matched failed k ([p1 sel1] [p2 sel2] ...))
        //       (failed))))]))
        case _ =>
          def matchFields(fields: List[(ChezName, Symbol)]): chez.Expr = fields match {
            case Nil => chez.Call(k, Variable(matched))
            case (p, field) :: fields =>
              val sel = nameRef(field)
              chez.Call(p, chez.Call(sel, Variable(sc)), Variable(matched), Variable(failed),
                chez.Lambda(List(matched), // the continuation shadows `matched`
                  matchFields(fields)))
          }
          matchFields(fieldNames zip fields)
      }
      chez.Function(matcherName, fieldNames,
            chez.Lambda(List(sc, matched, failed, k),
              chez.If(
                chez.Call(names.predicate, Variable(sc)),
                matcherBody,
                chez.Call(failed))))
    }

    List(record, matcher)
  }

  // (define (getter ref)
  //  (lambda () (unbox ref)))
  //
  // (define (setter ref)
  //  (lambda (v) (set-box! ref v)))
  def generateStateAccessors: List[chez.Function] = {
    val ref = ChezName("ref")
    val value = ChezName("value")

    val getter = chez.Function(nameDef(symbols.builtins.TState.get), List(ref),
      chez.Lambda(Nil, chez.Builtin("unbox", Variable(ref))))

    val setter = chez.Function(nameDef(symbols.builtins.TState.put), List(ref),
      chez.Lambda(List(value), chez.Builtin("set-box!", Variable(ref), Variable(value))))

    List(getter, setter)
  }

  def toChez(p: Param): ChezName = nameDef(p.id)

  def toChez(e: core.Argument): chez.Expr = e match {
    case e: core.Expr  => toChez(e)
    case b: core.Block => toChez(b)
  }

  def toChez(module: ModuleDecl): List[chez.Def] = {
    toChez(module.defs).definitions // TODO FIXME, once there is a let _ = ... in there, we are doomed!
  }

  def curry(lam: chez.Lambda): chez.Lambda = lam.params.foldRight[chez.Lambda](chez.Lambda(Nil, lam.body)) {
    case (p, body) => chez.Lambda(List(p), body)
  }

  def toChezMonadic(stmt: Stmt): monadic.Control = stmt match {
    case Ret(e) => monadic.Pure(toChez(e))
    case App(b, targs, args) => chez.Call(toChez(b), args map toChez)
    case If(cond, thn, els) => chez.If(toChez(cond), toChezMonadic(thn), toChezMonadic(els))
    case Val(id, tpe, binding, body) => monadic.Bind(toChezMonadic(binding), nameDef(id), toChez(body))
    case While(cond, body) => chez.Builtin("while", toChezMonadic(cond), toChezMonadic(body))
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
            chez.Operation(nameDef(op), params.init.map(p => nameDef(p.id)), nameDef(params.last.id), toChezMonadic(body))
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

    case other => chez.Block(Nil, Nil, toChezMonadic(other))
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

    case Run(s) => monadic.Run(toChezMonadic(s))
  }

  def toChez(p: Pattern): chez.Expr = p match {
    case IgnorePattern()    => Variable(ChezName("ignore"))
    case AnyPattern()       => Variable(ChezName("any"))
    case LiteralPattern(l)  => Builtin("literal", toChez(l))
    case TagPattern(id, ps) => Builtin("match-" + uniqueName(id), ps map { p => toChez(p) }: _*)
  }
}

object monadic {

  import chez.*

  // For Chez Scheme, we do not perform the (opaque) distinction between monadic.Control and chez.Expr, since we
  // want to reuse the translation for different versions (monadic, callcc).
  type Control = Expr

  def Pure(expr: Expr): Control = Builtin("pure", expr)
  def Run(m: Control): Expr = Builtin("run", m)

  def Bind(m: Control, param: ChezName, body: Block): Control = Builtin("then", m, chez.Lambda(List(param), body))
//
//  def Call(callee: Expr, args: List[Expr]): Control = js.Call(callee, args)
//  def If(cond: Expr, thn: Control, els: Control): Control = js.IfExpr(cond, thn, els)
//  def While(cond: Control, body: Control): Control = Builtin("_while", js.Lambda(Nil, cond), js.Lambda(Nil, body))
//  def Handle(handlers: List[Expr], body: Expr): Control = js.Call(Builtin("handle", js.ArrayLiteral(handlers)), List(body))
//

//
//  def Lambda(params: List[JSName], stmts: List[Stmt], ret: Control): Expr =
//    js.Lambda(params, js.Block(stmts :+ js.Return(ret)))
//
//  def Function(name: JSName, params: List[JSName], stmts: List[Stmt], ret: Control): Stmt =
//    js.Function(name, params, stmts :+ js.Return(ret))
}