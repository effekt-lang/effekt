package effekt
package generator
package chez

import cps.*
import core.Declaration
import effekt.core
import effekt.symbols.Symbol
import effekt.util.UByte
import effekt.util.messages.ErrorReporter

/** Lowers the direct-style CPS IR to Chez Scheme.
 *
 *  Chez already provides proper tail calls, so CPS applications can be
 *  translated literally. Reset, shift, and resume use the existing Chez CPS
 *  runtime; their CPS bodies become the program lambdas expected there.
 */
object TransformerCps {

  val HOLE = "hole"

  // Defined in chez/cps/effekt.ss
  val RUN_TOP_LEVEL = "run-top-level"
  val RESET = "reset"
  val RESUME = "resume"
  val SHIFT = "shift"
  val VAR = "var"
  val GET = "get"
  val PUT = "put"
  val CREATE_REGION = "create-region"
  val ALLOCATE = "allocate"
  val DEALLOCATE = "deallocate"

  def compile(input: cps.ModuleDecl, mainSymbol: symbols.TermSymbol)(using ErrorReporter): chez.Block = {
    val externs = input.externs.map(toChez)
    val declarations = input.declarations.flatMap(toChez)
    val definitions = input.definitions.map(toChez)
    val runMain = Builtin(RUN_TOP_LEVEL, nameRef(mainSymbol))
    chez.Block(externs ++ declarations ++ definitions, Nil, runMain)
  }

  def compileLSP(input: cps.ModuleDecl, mainSymbol: symbols.TermSymbol)(using ErrorReporter): chez.Block = {
    val lspModule = input.copy(
      includes = Nil,
      declarations = Nil,
      externs = input.externs.collect { case d: Extern.Def => d }
    )
    compile(lspModule, mainSymbol)
  }

  def toChez(definition: cps.ToplevelDefinition): chez.Def = definition match {
    case ToplevelDefinition.Def(id, params, body) =>
      chez.Function(nameDef(id), params.map(nameDef), toChez(body))

    case ToplevelDefinition.Val(id, ks, k, binding) =>
      val lambda = chez.Lambda(List(ks, k).map(nameDef), toChez(binding))
      chez.Constant(nameDef(id), Builtin(RUN_TOP_LEVEL, lambda))
  }

  def toChez(declaration: core.Declaration): List[chez.Def] = declaration match {
    case Declaration.Data(_, _, constructors) =>
      constructors.flatMap { ctor => generateConstructor(ctor.id, ctor.fields.map(_.id)) }
    case Declaration.Interface(id, _, properties) =>
      generateConstructor(id, properties.map(_.id))
  }

  def toChez(extern: cps.Extern)(using ErrorReporter): chez.Def = extern match {
    // This preserves the old Chez backend's treatment of async externs. The
    // Chez test suite excludes asynchronous I/O, and its FFI calls are direct.
    case Extern.Def(id, params, _, body) =>
      chez.Function(nameDef(id), params.map(nameDef), toChez(body))

    case Extern.Include(_, contents) =>
      chez.RawDef(contents)
  }

  def toChez(externBody: cps.ExternBody)(using ErrorReporter): chez.Expr = externBody match {
    case ExternBody.StringExternBody(_, contents) =>
      RawExpr(contents.strings, contents.args.map(toChez))
    case unsupported: ExternBody.Unsupported =>
      unsupported.report
      chez.Builtin(HOLE)
  }

  def toChez(operation: cps.Operation): chez.Expr = operation match {
    case Operation(_, params, body) =>
      chez.Lambda(params.map(nameDef), toChez(body))
  }

  def toChez(stmt: cps.Stmt): chez.Block = stmt match {
    case Stmt.Def(id, params, binding, rest) =>
      val lambda = chez.Lambda(params.map(nameDef), toChez(binding))
      resolveLet(id, lambda, rest)

    case Stmt.New(id, interface, operations, rest) =>
      val binding = chez.Call(nameRef(interface), operations.map(toChez))
      resolveLet(id, binding, rest)

    case Stmt.Let(id, binding, rest) =>
      resolveLet(id, toChez(binding), rest)

    case Stmt.Run(id, callee, args, _, rest) =>
      val binding = chez.Call(nameRef(callee), args.map(toChez))
      resolveLet(id, binding, rest)

    case Stmt.Region(id, ks, rest) =>
      resolveLet(id, Builtin(CREATE_REGION, toChez(ks)), rest)

    case Stmt.Alloc(id, init, region, rest) =>
      resolveLet(id, Builtin(ALLOCATE, toChez(init), nameRef(region)), rest)

    case Stmt.Get(ref, id, rest) =>
      resolveLet(id, Builtin(GET, nameRef(ref)), rest)

    case Stmt.Put(ref, value, rest) =>
      prepend(Builtin(PUT, nameRef(ref), toChez(value)), rest)

    case Stmt.Dealloc(ref, rest) =>
      prepend(Builtin(DEALLOCATE, nameRef(ref)), rest)

    case Stmt.Var(id, init, ks, rest) =>
      resolveLet(id, Builtin(VAR, toChez(init), toChez(ks)), rest)

    case _ =>
      chez.Block(Nil, Nil, toChezExpr(stmt))
  }

  private def resolveLet(toBind: Symbol, bindTo: chez.Expr, rest: cps.Stmt): chez.Block = {
    val chez.Block(definitions, expressions, result) = toChez(rest)
    chez.Block(chez.Constant(nameDef(toBind), bindTo) :: definitions, expressions, result)
  }

  private def prepend(expression: chez.Expr, rest: cps.Stmt): chez.Block = {
    // Keep the remainder nested: hoisting its definitions ahead of this
    // expression could reorder an effectful Run binding.
    chez.Block(Nil, List(expression), toChezExpr(rest))
  }

  def toChezExpr(stmt: cps.Stmt): chez.Expr = stmt match {
    case Stmt.Call(result, id, args, ks, rest) =>
      val returnedKs = core.Id("ks")
      val continuation = chez.Lambda(
        List(result, returnedKs).map(nameDef),
        toChez(rest))
      val lowered = args.map(toChez) ++ List(toChez(ks), continuation)
      chez.Call(nameRef(id), lowered)

    case Stmt.App(id, args) =>
      chez.Call(nameRef(id), args.map(toChez))

    case Stmt.Invoke(id, method, args) =>
      val operation = chez.Call(nameRef(method), nameRef(id))
      chez.Call(operation, args.map(toChez))

    case Stmt.Return(value) =>
      toChez(value)

    case Stmt.Hole(span) =>
      chez.Builtin(HOLE, chez.ChezString(span.range.from.format))

    case Stmt.If(cond, thn, els) =>
      chez.If(toChez(cond), toChezExpr(thn), toChezExpr(els))

    case Stmt.Match(_, Nil, None) =>
      chez.Builtin("unreachable")

    case Stmt.Match(scrutinee, clauses, default) =>
      val sc = toChez(scrutinee)
      val branches = clauses.map { case (constructor, branch) =>
        val names = RecordNames(constructor)
        val predicate = chez.Call(chez.Variable(names.predicate), sc)
        val matcher = chez.Call(chez.Variable(names.matcher), sc, toChez(branch))
        (predicate, matcher)
      }
      chez.Cond(branches, default.map(toChezExpr))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      val program = chez.Lambda(List(p, ks, k).map(nameDef), toChez(body))
      chez.Builtin(RESET, program, toChez(ks1), toChez(k1))

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      val program = chez.Lambda(List(resume, ks, k).map(nameDef), toChez(body))
      chez.Builtin(SHIFT, nameRef(prompt), program, toChez(ks1), toChez(k1))

    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      val block = chez.Lambda(List(ks, k).map(nameDef), toChez(body))
      chez.Builtin(RESUME, nameRef(resumption), block, toChez(ks1), toChez(k1))

    case nested: (Stmt.Def | Stmt.New | Stmt.Let | Stmt.Run | Stmt.Region |
                  Stmt.Alloc | Stmt.Var | Stmt.Dealloc | Stmt.Get | Stmt.Put) =>
      chez.Let(Nil, toChez(nested))
  }

  def toChez(expr: cps.Expr): chez.Expr = expr match {
    case Expr.Variable(id) => chez.Variable(nameRef(id))
    case Expr.Literal((), core.Type.TUnit) => chez.RawValue("(void)")
    case Expr.Literal(value: String, core.Type.TString) =>
      TransformerMonadic.escape(value)
    case Expr.Literal(value: Boolean, core.Type.TBoolean) =>
      if value then chez.RawValue("#t") else chez.RawValue("#f")
    case Expr.Literal(value: Byte, core.Type.TByte) =>
      chez.RawValue(UByte.unsafeFromByte(value).toInt.toString)
    case Expr.Literal(value, _) =>
      chez.RawValue(value.toString)
    case Expr.Make(_, tag, args) =>
      chez.Call(nameRef(tag), args.map(toChez))
    case Expr.Abort =>
      chez.RawExpr("(void)")
    case Expr.Toplevel =>
      chez.Variable(ChezName("top-level-ks"))
  }

  def toChez(clause: cps.Clause): chez.Expr = clause match {
    case Clause(params, body) =>
      chez.Lambda(params.map(nameDef), toChez(body))
  }
}
