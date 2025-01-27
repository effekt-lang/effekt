package effekt
package generator
package jit
import effekt.core
import effekt.symbols
import effekt.generator.jit
import effekt.context.Context
import scala.collection.mutable
import effekt.core.ValueType
import effekt.util.messages.ErrorReporter
import effekt.symbols.{TmpBlock, TmpValue}

object Transformer {

  import effekt.core.{Extern, Stmt, Toplevel}

  def transform(t: core.Type)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Type = t match {
    case valueType: core.ValueType => transform(valueType)
    case blockType: core.BlockType => transform(blockType)
  }
  def transform(t: core.ValueType)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Type = t match {
    case core.ValueType.Var(name) => jit.Top // Type variables are erased
    case core.ValueType.Data(symbols.builtins.UnitSymbol, Nil) => jit.Base.Unit
    case core.ValueType.Data(symbols.builtins.IntSymbol, Nil) => jit.Base.Int
    case core.ValueType.Data(symbols.builtins.BooleanSymbol, Nil) => jit.Base.Bool
    case core.ValueType.Data(symbols.builtins.DoubleSymbol, Nil) => jit.Base.Double
    case core.ValueType.Data(symbols.builtins.TopSymbol, Nil) => jit.Top
    case core.ValueType.Data(symbols.builtins.BottomSymbol, Nil) => jit.Bottom
    case core.ValueType.Data(symbols.builtins.StringSymbol, Nil) => jit.Base.String
    case core.ValueType.Data(symbols.builtins.CharSymbol, Nil) => jit.Base.String
    case core.ValueType.Data(name, _) if symbols.builtins.rootTypes.values.exists(_ == name) =>
      C.error(s"Unsupported builtin type ${core.PrettyPrinter.format(t)}.")
      jit.Top
    case core.ValueType.Data(name, targs) => DC.findData(name) match {
      case Some(core.Declaration.Data(id, tparams, constructors)) =>
        jit.Data(name, constructors map { cons =>
          jit.Constructor(cons.id, cons.fields.map { f =>
            if f.tpe == t then jit.Ptr else transform(f.tpe)
          })
        })
      case None => // assume this is a `extern type`, and that this is a ptr
        jit.Ptr
    }
    case core.ValueType.Boxed(tpe, capt) => transform(tpe)
  }
  def capabilityParamsFor(cparams: List[core.Id])(using DC: core.DeclarationContext, C: ErrorReporter): List[jit.LhsOperand] = Nil
  def capabilityParamTypesFor(cparams: List[core.Id])(using DC: core.DeclarationContext, C: ErrorReporter): List[Type] = Nil
  def transform(t: core.BlockType)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Type = t match {
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      jit.Function((vparams map transform) ++ (bparams map transform) ++ capabilityParamTypesFor(cparams),
        transform(result),
        Purity.Effectful) // FIXME
    case core.BlockType.Interface(symbols.builtins.RegionSymbol, _) => jit.Region
    case core.BlockType.Interface(symbols.builtins.TState.interface, List(to)) => jit.Ref(transform(to))
    case core.BlockType.Interface(name, _) if symbols.builtins.rootTypes.values.exists(_ == name) =>
      C.error(s"Unsupported builtin type ${core.PrettyPrinter.format(t)}.")
      jit.Top
    case core.BlockType.Interface(name, targs) =>
      val core.Declaration.Interface(id, tparams, properties) = DC.getInterface(name)
      jit.Codata(name, properties.map{ prop =>
        val core.BlockType.Function(tparams, cparams, vparams, bparams, result) = prop.tpe : @unchecked
        jit.Method(prop.id, ((vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams).map(_.tpe), transform(result))
      })
  }
  def transform(p: core.ValueParam | core.BlockParam)(using C: ErrorReporter, DC: core.DeclarationContext): jit.LhsOperand = p match {
    case core.ValueParam(id, tpe) => jit.Var(id, transform(tpe))
    case core.BlockParam(id, tpe, capt) => jit.Var(id, transform(tpe))
  }
  def transform(b: core.Block)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Term = b match {
    case core.Block.BlockVar(id, annotatedTpe, annotatedCapt) => jit.Var(id, transform(annotatedTpe))
    case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      jit.Abs(((vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams), transform(body))
    case core.Block.Unbox(pure) => transform(pure)
    case core.Block.New(impl: core.Implementation) =>
      transform(impl, None)
  }

  def transform(implementation: core.Implementation, prompt: Option[jit.Var])(using core.DeclarationContext, ErrorReporter): jit.Term = implementation match {
    case core.Implementation(interface, operations) =>
      val name = interface.name.show ++ "_" ++ interface.targs.map(_.toString).mkString("_")
      jit.New(name, operations.map(transform(_, prompt)))
  }
  def transform(op: core.Operation, prompt: Option[jit.Var])(using DC: core.DeclarationContext, C: ErrorReporter): (Id, jit.Clause) = op match {
    case core.Operation(name, tparams, cparams, vparams, bparams, body) =>
      val tBody =  transform(body)
      (name, jit.Clause(
        ((vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams),
        tBody))
  }

  def transformClause(b: core.BlockLit)(using core.DeclarationContext, ErrorReporter): jit.Clause = b match {
    case core.BlockLit(tparams, cparams, vparams, bparams, body) =>
      jit.Clause(((vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams),
        transform(body))
  }
  def transform(e: core.Expr)(using core.DeclarationContext, ErrorReporter): jit.Term = e match {
    case core.DirectApp(b, targs, vargs, bargs) => jit.App(transform(b), (vargs map transform) ++ (bargs map transform))
    case pure: core.Pure => transform(pure)
  }
  def transform(e: core.Pure)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Term = e match {
    case core.Pure.ValueVar(id, annotatedType) => jit.Var(id, transform(annotatedType))
    case core.Pure.Literal(value: Int, ValueType.Data(symbols.builtins.IntSymbol, Nil)) => jit.Literal.Int(value)
    case core.Pure.Literal(value: Long, ValueType.Data(symbols.builtins.IntSymbol, Nil)) => jit.Literal.Int(value.toInt)
    case core.Pure.Literal(value: Double, ValueType.Data(symbols.builtins.DoubleSymbol, Nil)) => jit.Literal.Double(value)
    case core.Pure.Literal(value: String, ValueType.Data(symbols.builtins.StringSymbol, Nil)) => jit.Literal.String(value)
    case core.Pure.Literal(value: Int, ValueType.Data(symbols.builtins.CharSymbol, Nil)) =>
      jit.Literal.String(Character.toString(value)) // will be escaped in pretty-printer
    case core.Pure.Literal(_, ValueType.Data(symbols.builtins.UnitSymbol, Nil)) => jit.Literal.Unit
    case core.Pure.Literal(value: Boolean, ValueType.Data(symbols.builtins.BooleanSymbol, Nil)) =>
      jit.Literal.Bool(value)
    case core.Pure.Literal(value, annotatedType) =>
      C.abort(s"Unsupported ${core.PrettyPrinter.format(annotatedType)} literal" +
        s" (or unsupported scala representation as ${value.getClass}).")
    case core.Pure.PureApp(b, targs, vargs) => jit.App(transform(b), vargs map transform)
    case core.Pure.Make(data, tag, vargs) => jit.The(transform(data), jit.Construct(data.name, tag, vargs map transform))
    case core.Pure.Box(b, annotatedCapture) => transform(b)
  }
  def transform(stmt: core.Stmt)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Term = stmt match {
    case Stmt.Def(id, block, body) =>
      jit.Let(List(jit.Definition(jit.Var(id, transform(block.tpe)), transform(block))), transform(body))
    case Stmt.Let(id, annotatedTpe, binding, body) =>
      jit.Let(List(jit.Definition(jit.Var(id, transform(annotatedTpe)), transform(binding))), transform(body))
    case Stmt.Reset(core.BlockLit(tparams, cparams, List(), List(prmpt), body)) =>
      val prmptV = transform(prmpt)
      val r = jit.Var(TmpValue(), transform(body.tpe))
      jit.Let(List(jit.Definition(prmptV, jit.FreshLabel())),
        jit.Reset(prmptV, jit.Var(TmpValue(), jit.Region), transform(body),
          jit.Clause(List(r), r)))
    case Stmt.Reset(_) => C.abort("Parameter to reset does not have 0 value and 1 block parameters.")
    case Stmt.Shift(prompt, core.BlockLit(tparams, cparams, List(), List(k), body)) =>
      jit.Shift(transform(prompt), jit.Literal.Int(0), transform(k), transform(body), transform(body.tpe))
    case Stmt.Shift(_, _) => C.abort("Shift body does not take exactly one block parameter.")
    case Stmt.Resume(k, body) =>
      jit.Resumed(transform(k), transform(body))
    case Stmt.Return(expr) => transform(expr)
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      import effekt.core.BlockType
      val ifce = callee.tpe match {
        case BlockType.Interface(name, targs) => name
        case _ => C.abort("Invoke on function value")
      }
      jit.Invoke(transform(callee), ifce, method, vargs.map(transform) ++ bargs.map(transform))
    case core.Stmt.Val(id, tpe, binding, body) =>
      jit.Let(List(jit.Definition(
        jit.Var(id, transform(tpe)),
        transform(binding))),
        transform(body))
    case core.Stmt.App(callee, targs, vargs, bargs) =>
      jit.App(transform(callee), (vargs map transform) ++ (bargs map transform))
    case core.Stmt.If(cond, thn, els) =>
      jit.IfZero(transform(cond), transform(els), transform(thn))
    case core.Stmt.Match(scrutinee, clauses, default) =>
      val tClauses = clauses.map {
        case (id, b) => (id, transformClause(b))
      }
      val tDefault = default.map { d => jit.Clause(Nil, transform(d)) }.getOrElse {
        val ret = Var("%bottom", Bottom)
        jit.Clause(Nil, jit.Primitive("non-exhaustive match", Nil, List(ret), ret))
      }
      val tpe: ValueType.Data = core.Type.inferType(scrutinee).asInstanceOf[ValueType.Data]
      jit.Match(transform(scrutinee), tpe.name, tClauses, tDefault)
    case core.Stmt.Region(core.BlockLit(Nil, cparams, Nil, List(reg), body)) =>
      val r = jit.Var(TmpValue(), transform(core.Type.inferType(body)))
      jit.Reset(jit.FreshLabel(), transform(reg), transform(body), Clause(List(r), r))
    case core.Stmt.Region(body) =>
      assert(false, "body of a `Region` is always a `BlockLit` with exactly one block param.")
    case core.Stmt.Alloc(id, init, region, body) =>
      jit.LetRef(jit.Var(id, jit.Ref(transform(core.Type.inferType(init)))), jit.Var(region, jit.Region), transform(init),
        transform(body))
    case core.Stmt.Var(id, init, capture, body) =>
      // TODO for now, just use a new region
      val reg = jit.Var(TmpBlock(), jit.Region)
      val r = jit.Var(TmpValue(), transform(core.Type.inferType(body)))
      jit.Reset(jit.FreshLabel(), reg, jit.LetRef(jit.Var(id, jit.Ref(transform(core.Type.inferType(init)))), reg,
        transform(init), transform(body)), Clause(List(r), r))
    case core.Stmt.Get(id, annotatedCapt, annotatedTpe) =>
      jit.Load(jit.Var(id, jit.Ref(transform(annotatedTpe))))
    case core.Stmt.Put(id, annotatedCapt, value) =>
      jit.Store(jit.Var(id, jit.Ref(transform(core.Type.inferType(value)))), transform(value))
    case core.Stmt.Hole() => jit.Primitive("hole", Nil, Nil, jit.Literal.Unit)
  }
  def transform(e: core.Extern)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Definition = e match {
    case Extern.Def(id, tparams, cparams, vparams, bparams, retTpe, annotatedCapture,
          core.ExternBody.StringExternBody(featureFlag, Template(List(body), Nil))) =>
      val args = ((vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams)
      val ret = jit.Var(TmpValue(), transform(retTpe))
      val tpe = jit.Function(args.map(_.tpe), transform(retTpe),
        if annotatedCapture.isEmpty then Purity.Pure else Purity.Effectful)
      jit.Definition(jit.Var(id, tpe), jit.Abs(args,
        jit.Primitive(body, args, List(ret), ret)))
    case Extern.Def(id, tparams, cparams, vparams, bparams, retTpe,
           annotatedCapture,core.ExternBody.Unsupported(err)) =>
      C.report(err)
      val args = ((vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams)
      val ret = jit.Var(TmpValue(), transform(retTpe))
      val tpe = jit.Function(args.map(_.tpe), transform(retTpe),
        if annotatedCapture.isEmpty then Purity.Pure else Purity.Effectful)
      jit.Definition(jit.Var(id, tpe), jit.Abs(args,
        jit.Primitive(s"!undefined:${id.name.toString}", args, List(ret), ret)))
    case Extern.Include(_, contents) =>
      C.abort("Extern includes are not supported in the JIT backend.")
    case e => C.abort(s"Unsupported extern: ${e}")
  }
  def transform(tl: core.Toplevel)(using C: ErrorReporter, DC: core.DeclarationContext): jit.Definition = tl match {
    case Toplevel.Def(id, block) => jit.Definition(jit.Var(id, transform(block.tpe)), transform(block))
    case Toplevel.Val(id, tpe, binding) => jit.Definition(jit.Var(id, transform(tpe)), transform(binding))
  }
  def transform(coreMod: core.ModuleDecl, mod: effekt.symbols.Module)(using C: Context): jit.Program = coreMod match {
    case core.ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      given DC: core.DeclarationContext = new core.DeclarationContext(coreMod.declarations, coreMod.externs)
      val main = C.checkMain(mod)
      val tDefinitions = (externs map transform) ++ (definitions map transform)
      jit.Program(tDefinitions, jit.App(jit.Var(main, jit.Function(Nil, jit.Base.Unit, jit.Purity.Effectful)), Nil))
  }

}
