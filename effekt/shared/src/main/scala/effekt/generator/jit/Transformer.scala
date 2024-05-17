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
import effekt.core.Param
import effekt.symbols.{TmpBlock, TmpValue}

object Transformer {

  import effekt.core.Extern

  def transform(t: core.Type)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Type = t match {
    case valueType: core.ValueType => transform(valueType)
    case blockType: core.BlockType => transform(blockType)
  }
  def transform(t: core.ValueType)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Type = t match {
    case core.ValueType.Var(name) => jit.Top // Type variables are erased
    case core.ValueType.Data(symbols.builtins.UnitSymbol, Nil) => jit.Base.Unit
    case core.ValueType.Data(symbols.builtins.IntSymbol, Nil) => jit.Base.Int
    case core.ValueType.Data(symbols.builtins.BooleanSymbol, Nil) => jit.Base.Int
    case core.ValueType.Data(symbols.builtins.DoubleSymbol, Nil) => jit.Base.Double
    case core.ValueType.Data(symbols.builtins.TopSymbol, Nil) => jit.Top
    case core.ValueType.Data(symbols.builtins.BottomSymbol, Nil) => jit.Bottom
    case core.ValueType.Data(symbols.builtins.StringSymbol, Nil) => jit.Base.String
    case core.ValueType.Data(name, _) if symbols.builtins.rootTypes.values.exists(_ == name) =>
      C.error(s"Unsupported builtin type ${core.PrettyPrinter.format(t)}.")
      jit.Top
    case core.ValueType.Data(name, targs) =>
      val core.Declaration.Data(id, tparams, constructors) = DC.getData(name)
      jit.Data(name, constructors map { cons =>
        jit.Constructor(cons.id, cons.fields.map { f =>
          if f.tpe == t then jit.Ptr else transform(f.tpe)
        })
      })
    case core.ValueType.Boxed(tpe, capt) => transform(tpe)
  }
  def capabilityParamsFor(cparams: List[core.Id])(using DC: core.DeclarationContext, C: ErrorReporter): List[jit.LhsOperand] = Nil
  def transform(t: core.BlockType)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Type = t match {
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      jit.Function((vparams map transform) ++ (bparams map transform) ++ (cparams map {_ => jit.Base.Label}),
        transform(result),
        Purity.Effectful) // FIXME
    case core.BlockType.Interface(symbols.builtins.RegionSymbol, _) => jit.Region
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
  def transform(b: core.Block)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Term = b match {
    case core.Block.BlockVar(id, annotatedTpe, annotatedCapt) => jit.Var(id, transform(annotatedTpe))
    case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      jit.Abs(((vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams), transform(body))
    case core.Block.Member(block, field, annotatedTpe) =>
      // `block` does not have side-effects, so we can defer it until the member call
      val core.BlockType.Interface(ifceTag, targs) = core.Type.inferType(block) : @unchecked
      val args = DC.getInterface(ifceTag).properties.collectFirst {
        case core.Property(`field`, core.BlockType.Function(tparams, cparams, vparams, bparams, result)) =>
          (vparams map { t => jit.Var(TmpValue(), transform(t)) })
            ++ (bparams map { t => jit.Var(TmpBlock(), transform(t)) })
            ++ capabilityParamsFor(cparams)
      }.get
      jit.Abs(args, jit.Invoke(transform(block), ifceTag, field, args))
    case core.Block.Unbox(pure) => transform(pure)
    case core.Block.New(core.Implementation(interface, operations)) =>
      val core.Declaration.Interface(id, tparams, properties) = DC.getInterface(interface.name)
      jit.New(interface.name, operations map { case core.Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
        (name, jit.Clause(
          ((resume.toList ++ vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams), // TODO resume is first? or last?
          transform(body)))
      })
  }
  def transform(p: core.Param)(using core.DeclarationContext, ErrorReporter): jit.LhsOperand = p match {
    case Param.ValueParam(id, tpe) => jit.Var(id, transform(tpe))
    case Param.BlockParam(id, tpe, capt) => jit.Var(id, transform(tpe))
  }
  def transformClause(b: core.BlockLit)(using core.DeclarationContext, ErrorReporter): jit.Clause = b match {
    case core.BlockLit(tparams, cparams, vparams, bparams, body) =>
      jit.Clause(((vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams),
        transform(body))
  }
  def transform(e: core.Expr)(using core.DeclarationContext, ErrorReporter): jit.Term = e match {
    case core.DirectApp(b, targs, vargs, bargs) => jit.App(transform(b), (vargs map transform) ++ (bargs map transform))
    case core.Run(s) => transform(s)
    case pure: core.Pure => transform(pure)
  }
  def transform(e: core.Pure)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Term = e match {
    case core.Pure.ValueVar(id, annotatedType) => jit.Var(id, transform(annotatedType))
    case core.Pure.Literal(value: Int, ValueType.Data(symbols.builtins.IntSymbol, Nil)) => jit.Literal.Int(value)
    case core.Pure.Literal(value: Long, ValueType.Data(symbols.builtins.IntSymbol, Nil)) => jit.Literal.Int(value.toInt)
    case core.Pure.Literal(value: Double, ValueType.Data(symbols.builtins.DoubleSymbol, Nil)) => jit.Literal.Double(value)
    case core.Pure.Literal(value: String, ValueType.Data(symbols.builtins.StringSymbol, Nil)) => jit.Literal.String(value)
    case core.Pure.Literal(_, ValueType.Data(symbols.builtins.UnitSymbol, Nil)) => jit.Literal.Unit
    case core.Pure.Literal(value: Boolean, ValueType.Data(symbols.builtins.BooleanSymbol, Nil)) =>
      jit.Literal.Int(if value then 1 else 0)
    case core.Pure.Literal(value, annotatedType) =>
      C.abort(s"Unsupported ${core.PrettyPrinter.format(annotatedType)} literal" +
        s" (or unsupported scala representation as ${value.getClass}).")
    case core.Pure.PureApp(b, targs, vargs) => jit.App(transform(b), vargs map transform)
    case core.Pure.Make(data, tag, vargs) => jit.Construct(data.name, tag, vargs map transform)
    case core.Pure.Select(target, field, annotatedType: ValueType.Data) =>
      val dField = DC.getField(field)
      val dConstructor = dField.constructor
      jit.Project(transform(target), annotatedType.name, dConstructor.id, dConstructor.fields.indexOf(dField))
    case core.Pure.Select(target, field, annotatedType) =>
      C.abort(s"Unsupported record select on value of type ${core.PrettyPrinter.format(annotatedType)}")
    case core.Pure.Box(b, annotatedCapture) => transform(b)
  }
  def transform(stmt: core.Stmt)(using core.DeclarationContext, ErrorReporter): jit.Term = stmt match {
    case core.Stmt.Scope(definitions, body) =>
      jit.LetRec(definitions map transform, transform(body))
    case core.Stmt.Return(expr) => transform(expr)
    case core.Stmt.Val(id, binding, body) =>
      jit.Let(List(jit.Definition(
          jit.Var(id, transform(core.Type.inferType(binding))),
          transform(binding))),
        transform(body))
    case core.Stmt.App(callee, targs, vargs, bargs) =>
      jit.App(transform(callee), (vargs map transform) ++ (bargs map transform))
    case core.Stmt.If(cond, thn, els) =>
      jit.IfZero(transform(cond), transform(els), transform(thn))
    case core.Stmt.Match(scrutinee, clauses, default) =>
      val tClauses = clauses.map{
        case (id, b) => (id, transformClause(b))
      }
      val tDefault = default.map{ d => jit.Clause(Nil, transform(d)) }.getOrElse{
        jit.Clause(Nil, jit.Primitive("non-exhaustive match", Nil, Nil, jit.Literal.Unit))
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
    case core.Stmt.Try(core.BlockLit(tparams, cparams, vparams, bparams, body), handlers) =>
      assert(vparams.isEmpty, "Only block parameters for body of Try")
      val promptIds = cparams.map{ _ => TmpValue() }
      val tBody = transform(body)
      jit.LetRec(promptIds.map{ x =>
        jit.Definition(jit.Var(x, jit.Base.Label), jit.FreshLabel())
      } ++ (cparams zip promptIds zip (handlers zip bparams)).map{ case ((c, p), (h, bc)) =>
        jit.Definition(jit.Var(bc.id, transform(bc.tpe)), jit.New(c, h.operations.map {
          case core.Operation(name, tparams, cparams, vparams, bparams, resume, obody) =>
            val args = (((vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams))
            val rtpe = resume.map(_.tpe) match {
              case Some(core.BlockType.Function(_, _, List(r), _, _)) => transform(r)
              case _ => jit.Top
            }
            val r = jit.Var(TmpValue(), rtpe)
            (name, jit.Clause(args, jit.DOp(jit.Var(p, jit.Base.Label), name, args, jit.Clause(List(r), r), rtpe)))
        }))
      }, handlers.zip(promptIds).foldRight(tBody){ case ((core.Implementation(itpe, ops), id), b) =>
        jit.DHandle(jit.HandlerType.Deep, jit.Var(id, jit.Base.Label),
          ops.map{ case core.Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
            val params = ((resume.toList ++ vparams ++ bparams) map transform) ++ capabilityParamsFor(cparams)
            (name, jit.Clause(params,
              transform(body)))
          },
          None, b)
      })
    case core.Stmt.Try(body, handlers) => ???
    case core.Stmt.Hole() => jit.Primitive("hole", Nil, Nil, jit.Literal.Unit)
  }
  def transform(d: core.Definition)(using core.DeclarationContext, ErrorReporter): jit.Definition = d match {
    case core.Definition.Def(id, block) => jit.Definition(jit.Var(id, transform(core.Type.inferType(block))), transform(block))
    case core.Definition.Let(id, binding) => jit.Definition(jit.Var(id, transform(core.Type.inferType(binding))), transform(binding))
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
    case Extern.Include(_, contents) =>
      C.abort("Extern includes are not supported in the JIT backend.")
    case e => C.abort(s"Unsupported extern: ${e}")
  }
  def transform(coreMod: core.ModuleDecl, mod: effekt.symbols.Module)(using C: Context): jit.Program = coreMod match {
    case core.ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      given DC: core.DeclarationContext = new core.DeclarationContext(coreMod.declarations, coreMod.externs)
      val main = C.checkMain(mod)
      val tDefinitions = (externs map transform) ++ (definitions map transform)
      jit.Program(tDefinitions, jit.App(jit.Var(main, jit.Function(Nil, jit.Base.Unit, jit.Purity.Effectful)), Nil))
  }

}
