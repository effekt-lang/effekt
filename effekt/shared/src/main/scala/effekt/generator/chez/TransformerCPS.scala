package effekt
package generator
package chez

import cps.*
import core.Declaration
import effekt.symbols.Symbol
import effekt.core
import effekt.util.UByte
import effekt.util.messages.ErrorReporter

/**
 * Lowers the CPS IR to Chez Scheme.
 *
 * Chez already provides proper tail calls, so CPS applications can be
 * translated literally. Reset, shift, and resume use the Chez CPS
 * runtime; their CPS bodies become the program lambdas expected there.
 */
object TransformerCPS {

  val HOLE = "hole"

  // Defined in chez/cps/effekt.ss
  val RUN_TOP_LEVEL = "run-top-level"
  val TOP_LEVEL_KS = "top-level-ks"
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

  def compileLSP(input: cps.ModuleDecl, mainSymbol: symbols.TermSymbol)(using ErrorReporter): chez.Block =
    compile(input.copy(
      includes = Nil,
      declarations = Nil,
      externs = input.externs.collect { case d: Extern.Def => d }
    ), mainSymbol)

  def toChez(definition: cps.ToplevelDefinition): chez.Def = definition match {
    case ToplevelDefinition.Def(id, params, body) =>
      chez.Function(nameDef(id), params.map(nameDef), toChez(body))
    case ToplevelDefinition.Val(id, ks, k, binding) =>
      val lambda = chez.Lambda(List(ks, k).map(nameDef), toChez(binding))
      chez.Constant(nameDef(id), Builtin(RUN_TOP_LEVEL, lambda))
  }

  def toChez(declaration: core.Declaration): List[chez.Def] = declaration match {
    case Declaration.Data(id, _, constructors) =>
      constructors.flatMap { ctor =>
        generateConstructor(ctor.id, ctor.fields.map(f => f.id)) ++ Option.when(ctor.fields.isEmpty)(generateInstance(ctor.id))
      }
    case Declaration.Interface(id, _, properties) =>
      generateConstructor(id, properties.map(_.id))
  }

  def toChez(extern: cps.Extern)(using ErrorReporter): chez.Def = extern match {
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
    case Stmt.Def(id, params, body, rest) =>
      resolveLet(id, chez.Lambda(params.map(nameDef), toChez(body)), rest)
    case Stmt.New(id, interface, operations, rest) =>
      resolveLet(id, chez.Call(nameRef(interface), operations.map(toChez)), rest)
    case Stmt.Let(id, binding, rest) =>
      resolveLet(id, toChez(binding), rest)
    case Stmt.Run(id, callee, args, _, rest) =>
      resolveLet(id, chez.Call(nameRef(callee), args.map(toChez)), rest)
    case Stmt.Region(id, ks, rest) =>
      resolveLet(id, Builtin(CREATE_REGION, toChez(ks)), rest)
    case Stmt.Alloc(id, init, region, rest) =>
      resolveLet(id, Builtin(ALLOCATE, toChez(init), toChez(region)), rest)
    case Stmt.Get(ref, id, rest) =>
      resolveLet(id, Builtin(GET, toChez(ref)), rest)
    case Stmt.Put(ref, value, rest) =>
      chez.Block(Nil, List(Builtin(PUT, toChez(ref), toChez(value))), toChezExpr(rest))
    case Stmt.Dealloc(ref, rest) =>
      chez.Block(Nil, List(Builtin(DEALLOCATE, toChez(ref))), toChezExpr(rest))
    case Stmt.Var(id, init, ks, rest) =>
      resolveLet(id, Builtin(VAR, toChez(init), toChez(ks)), rest)
    case _ => chez.Block(Nil, Nil, toChezExpr(stmt))
  }

  def resolveLet(toBind: Symbol, bindTo: chez.Expr, rest: cps.Stmt): chez.Block = {
    val chez.Block(defs, exprs, result) = toChez(rest)
    chez.Block(chez.Constant(nameDef(toBind), bindTo) :: defs, exprs, result)
  }

  def toChez(callee: Callee): chez.Expr = callee match {
    case Callee.Function(id) => toChez(id)
    case Callee.Method(receiver, method) => chez.Call(toChez(method), toChez(receiver))
  }

  def toChezExpr(stmt: cps.Stmt): chez.Expr = stmt match {
    case Stmt.Call(callee, args, ReturnPoint.Bind(results, returnedKs, ks, rest)) =>
      val continuation = chez.Lambda((results :+ returnedKs).map(nameDef), toChez(rest))
      chez.Call(toChez(callee), (args :+ ks).map(toChez) :+ continuation)

    case Stmt.Call(callee, args, ReturnPoint.Direct(List(result), rest)) =>
      chez.Let(List(Binding(nameDef(result), chez.Call(toChez(callee), args.map(toChez)))), toChez(rest))

    case Stmt.Call(callee, args, ReturnPoint.Direct(results, rest)) =>
      Builtin("call-with-values",
        chez.Lambda(Nil, chez.Call(toChez(callee), args.map(toChez))),
        chez.Lambda(results.map(nameDef), toChez(rest)))

    case call @ Stmt.Call(callee, _, _: ReturnPoint.Tail | ReturnPoint.Jump) =>
      chez.Call(toChez(callee), call.knownArguments.map(toChez))

    case Stmt.Return(List(value)) => toChez(value)
    case Stmt.Return(values) => Builtin("values", values.map(toChez)*)

    case Stmt.Hole(span) => chez.Builtin(HOLE, chez.ChezString(span.range.from.format))
    case Stmt.If(cond, thn, els) =>
      chez.If(toChez(cond), toChezExpr(thn), toChezExpr(els))
    case Stmt.Match(scrutinee, clauses, default) =>
      val sc = toChez(scrutinee)
      val cls = clauses.map { case (constr, branch) =>
        val names = RecordNames(constr)
        val pred = generateTest(constr, branch.params.isEmpty, sc)
        val matcher = chez.Call(chez.Variable(names.matcher), sc, toChez(branch))
        (pred, matcher)
      }
      generateMatch(cls, default.map(toChezExpr))
    case Stmt.Reset(p, ks, k, body, arity, ks1, k1) =>
      val prog = chez.Lambda(List(p, ks, k).map(nameDef), toChez(body))
      chez.Builtin(RESET, prog, returnFrom(arity), toChez(ks1), toChez(k1))
    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      val handler = chez.Lambda(List(resume, ks, k).map(nameDef), toChez(body))
      chez.Builtin(SHIFT, toChez(prompt), handler, toChez(ks1), toChez(k1))
    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      val resumed = chez.Lambda(List(ks, k).map(nameDef), toChez(body))
      chez.Builtin(RESUME, toChez(resumption), resumed, toChez(ks1), toChez(k1))
    case let: (Stmt.Def | Stmt.New | Stmt.Let | Stmt.Run
             | Stmt.Region | Stmt.Alloc | Stmt.Get | Stmt.Put | Stmt.Dealloc | Stmt.Var) =>
      chez.Let(Nil, toChez(stmt))
  }

  // Returns the answer of a reset to the stack below; Chez allocates this closed lambda statically:
  //   (lambda (x0 ... xn ks) ((meta-cont-cont (meta-cont-rest ks)) x0 ... xn (meta-cont-rest ks)))
  def returnFrom(arity: Int): chez.Expr = {
    val values = List.tabulate(arity) { i => ChezName(s"x$i") }
    val ks = ChezName("ks")
    val rest = Builtin("meta-cont-rest", chez.Variable(ks))
    chez.Lambda(values :+ ks, chez.Call(Builtin("meta-cont-cont", rest), values.map(chez.Variable.apply) :+ rest))
  }

  def toChez(expr: cps.Expr): chez.Expr = expr match {
    case Expr.Variable(id) => toChez(id)
    case Expr.Literal((), core.Type.TUnit) => chez.RawValue("(void)")
    //TODO: Copy paste escape procedure to this transformer?
    case Expr.Literal(v: String, core.Type.TString) =>
      effekt.generator.chez.TransformerMonadic.escape(v)
    case Expr.Literal(b: Boolean, core.Type.TBoolean) =>
      if (b) chez.RawValue("#t") else chez.RawValue("#f")
    case Expr.Literal(b: Byte, core.Type.TByte) => chez.RawValue(UByte.unsafeFromByte(b).toInt.toString)
    case Expr.Literal(n: Long, core.Type.TInt) => intLiteral(n)
    case Expr.Literal(value, _) => chez.RawValue(value.toString()) // TODO this should match on the actual types...
    case Expr.Make(_, tag, Nil) => chez.Variable(RecordNames(tag).instance)
    case Expr.Make(_, tag, vargs) =>
      chez.Call(nameRef(tag), vargs.map(toChez))
    case Expr.Abort => chez.RawExpr("(void)")
    case Expr.Toplevel => chez.Variable(ChezName(TOP_LEVEL_KS))
  }

  def toChez(clause: cps.Clause): chez.Expr = clause match {
    case Clause(params, body) =>
      chez.Lambda(params.map(nameDef), toChez(body))
  }

  def toChez(id: Symbol): chez.Variable = chez.Variable(nameRef(id))
}
