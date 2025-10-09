package effekt
package generator
package chez

import cps.*
import core.Declaration
import effekt.symbols.Symbol
import effekt.core
import effekt.util.messages.ErrorReporter

object TransformerCPS {

  val HOLE = "hole"

  // Defined in chez/cps/effekt.ss
  val TOP_LEVEL_K = "top-level-k"
  val RUN_TOP_LEVEL = "run-top-level"
  val RESET = "reset"
  val RESUME = "resume"
  val SHIFT = "shift"

  def compile(input: cps.ModuleDecl, mainSymbol: symbols.TermSymbol)(using ErrorReporter): chez.Block = {
    val externs = input.externs.map(toChez)
    val declarations = input.declarations.flatMap(toChez)
    val definitions = input.definitions.map(toChez)
    val runMain = Builtin(RUN_TOP_LEVEL, nameRef(mainSymbol))
    chez.Block(externs ++ declarations ++ definitions, Nil, runMain)
  }

  def compileLSP(input: cps.ModuleDecl, mainSymbol: symbols.TermSymbol)(using ErrorReporter): chez.Block = input match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      val lspModule = cps.ModuleDecl(
        path,
        Nil,
        declarations,
        externs.collect{case d: Extern.Def => d},
        definitions,
        exports
      )
      compile(lspModule, mainSymbol)
  }

  def toChez(definition: cps.ToplevelDefinition): chez.Def = definition match {
    case ToplevelDefinition.Def(id, block) => chez.Constant(nameDef(id), toChez(block))
    case ToplevelDefinition.Let(id, expr) => chez.Constant(nameDef(id), toChez(expr))
    case ToplevelDefinition.Val(id, ks, k, binding) =>
      chez.Function(nameDef(id), List(ks, k).map(nameDef), toChez(binding))
  }

  def toChez(declaration: core.Declaration): List[chez.Def] = declaration match {
    case Declaration.Data(id, _, constructors) =>
      constructors.flatMap { ctor => generateConstructor(ctor.id, ctor.fields.map(f => f.id)) }
    case Declaration.Interface(id, _, properties) =>
      generateConstructor(id, properties.map(_.id))
  }

  def toChez(extern: cps.Extern)(using ErrorReporter): chez.Def = extern match {
    case Extern.Def(id, vparams, bparams, _, body) =>
      val params = (vparams ++ bparams).map(nameDef)
      chez.Function(nameDef(id), params, toChez(body))
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

  def toChez(block: cps.Block): chez.Expr = block match {
    case BlockVar(id) => chez.Variable(nameRef(id))
    case BlockLit(vparams, bparams, ks, k, body) =>
      val params = vparams ++ bparams ++ List(ks, k)
      chez.Lambda(params.map(nameDef), toChez(body))
    case New(impl) =>
      chez.Call(toChez(impl.interface.name), impl.operations.map(toChez))
    case Unbox(pure) => toChez(pure)
  }

  def toChez(operation: cps.Operation): chez.Expr = operation match {
    case Operation(_, vparams, bparams, ks, k, body) =>
      val params = (vparams ++ bparams ++ List(ks, k)).map(nameDef)
      chez.Lambda(params, toChez(body))
  }

  def toChez(stmt: cps.Stmt): chez.Block = stmt match {
    case ImpureApp(id, callee, vargs, bargs, body) =>
      val binding = chez.Call(toChez(callee), vargs.map(toChez) ++ bargs.map(toChez))
      resolveLet(id, binding, body)
    case LetCont(id, binding, body) => resolveLet(id, toChez(binding), body)
    case LetDef(id, binding, body) => resolveLet(id, toChez(binding), body)
    case LetExpr(id, binding, body) => resolveLet(id, toChez(binding), body)
    case _ => chez.Block(Nil, Nil, toChezExpr(stmt))
  }

  def resolveLet(toBind: Symbol, bindTo: chez.Expr, thn: cps.Stmt): chez.Block = {
    val chez.Block(defs, exprs, result) = toChez(thn)
    chez.Block(chez.Constant(nameDef(toBind), bindTo) :: defs, exprs, result)
  }

  def toChezExpr(stmt: cps.Stmt): chez.Expr = stmt match {
    case App(callee, vargs, bargs, ks, k) =>
      val args = vargs.map(toChez) ++ bargs.map(toChez)
              ++ List(toChez(ks), toChez(k))
      chez.Call(toChez(callee), args)
    case Jump(k, vargs, ks) =>
      val args = vargs.map(toChez) :+ toChez(ks)
      chez.Call(toChez(k), args)
    case Hole(span) => chez.Builtin(HOLE)
    case If(cond, thn, els) =>
      val chezCond = toChez(cond)
      chez.If(chezCond, toChezExpr(thn), toChezExpr(els))
    case Match(scrutinee, clauses, default) => ???
    case Reset(prog, ks, k) =>
      chez.Builtin(RESET, toChez(prog), toChez(ks), toChez(k))
    case Resume(resumption, body, ks, k) =>
      chez.Builtin(RESUME, toChez(resumption), toChez(body), toChez(ks), toChez(k))
    case Shift(prompt, body, ks, k) =>
      chez.Builtin(SHIFT, toChez(prompt), toChez(body), toChez(ks), toChez(k))
    case Invoke(callee, method, vargs, bargs, ks, k) =>
      val methodLam = chez.Call(toChez(method), toChez(callee))
      val args = vargs.map(toChez) ++ bargs.map(toChez)
              ++ List(toChez(ks), toChez(k))
      chez.Call(methodLam, args)
    // TODO: Local mutable state
    case Region(id, ks, body) => ???
    case Alloc(id, init, region, body) => ???
    case Get(ref, id, body) => ???
    case Put(ref, value, body) => ???
    case Dealloc(ref, body) => ???
    case Var(id, init, ks, body) => ???
    case let: (LetCont | LetDef | LetExpr | ImpureApp) =>
      chez.Let(Nil, toChez(stmt))
  }

  def toChez(expr: cps.Expr): chez.Expr = expr match {
    case ValueVar(id) => toChez(id)
    case Literal(()) => chez.RawValue("(void)")
    case Literal(v: String) => chez.RawValue(s"\"$v\"")
    case Literal(value) => chez.RawValue(value.toString())
    case PureApp(id, vargs) => chez.Call(toChez(id), vargs.map(toChez))
    case Make(_, tag, vargs) => ???
    case Box(id) => toChez(id)
  }

  def toChez(cont: cps.Cont): chez.Expr = cont match {
    case Cont.ContVar(id) => toChez(id)
    case Cont.ContLam(results, ks, body) =>
      val params = (results :+ ks).map(nameDef)
      chez.Lambda(params, toChez(body))
    case Cont.Abort => chez.RawExpr("(void)")
  }

  def toChez(id: Symbol): chez.Variable = Variable(nameRef(id))

  def toChez(mc: cps.MetaCont): chez.Variable = toChez(mc.id)
}