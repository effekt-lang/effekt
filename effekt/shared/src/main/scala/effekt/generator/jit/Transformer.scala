package effekt
package generator
package jit
import effekt.core
import effekt.generator.jit
import effekt.context.Context
import scala.collection.mutable
import effekt.core.ValueType
import effekt.util.messages.ErrorReporter

object Transformer {

  import effekt.core.Param

  def transform(t: core.Type)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Type = t match {
    case valueType: core.ValueType => transform(valueType)
    case blockType: core.BlockType => transform(blockType)
  }
  def transform(t: core.ValueType)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Type = t match {
    case core.ValueType.Var(name) => jit.Top // Type variables are erased
    case core.ValueType.Data(name, targs) =>
      val core.Declaration.Data(id, tparams, constructors) = DC.getData(name)
      jit.Data(name, constructors map { cons =>
        jit.Constructor(cons.id, cons.fields.map { f =>
          transform(f.tpe)
        })
      })
    case core.ValueType.Boxed(tpe, capt) => transform(tpe)
  }
  def transform(t: core.BlockType)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Type = t match {
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      jit.Function((vparams map transform) ++ (bparams map transform) ++ (cparams map {_ => jit.Base.Label}),
        transform(result),
        Purity.Effectful) // FIXME
    case core.BlockType.Interface(name, targs) =>
      val core.Declaration.Interface(id, tparams, properties) = DC.getInterface(name)
      jit.Codata(name, properties.map{ prop =>
        val core.BlockType.Function(tparams, cparams, vparams, bparams, result) = prop.tpe : @unchecked
        jit.Method(prop.id, ((vparams ++ bparams) map transform) ++ (cparams map { _ => jit.Ptr }), transform(result))
      })
  }
  def transform(b: core.Block)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Term = b match {
    case core.Block.BlockVar(id, annotatedTpe, annotatedCapt) => jit.Var(id, transform(annotatedTpe))
    case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      jit.Abs(((vparams ++ bparams) map transform) ++ (cparams map { x => jit.Var(x,jit.Ptr) }), transform(body))
    case core.Block.Member(block, field, annotatedTpe) => ???
    case core.Block.Unbox(pure) => transform(pure)
    case core.Block.New(core.Implementation(interface, operations)) =>
      val core.Declaration.Interface(id, tparams, properties) = DC.getInterface(interface.name)
      jit.New(interface.name, operations map { case core.Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
        (name, jit.Clause(
          ((resume.toList ++ vparams ++ bparams) map transform) ++ (cparams map { x => jit.Var(x, jit.Ptr) }), // TODO resume is first? or last?
          transform(body)))
      })
  }
  def transform(p: core.Param)(using core.DeclarationContext, ErrorReporter): jit.LhsOperand = p match {
    case Param.ValueParam(id, tpe) => jit.Var(id, transform(tpe))
    case Param.BlockParam(id, tpe, capt) => jit.Var(id, transform(tpe))
  }
  def transformClause(b: core.BlockLit)(using core.DeclarationContext, ErrorReporter): jit.Clause = b match {
    case core.BlockLit(tparams, cparams, vparams, bparams, body) =>
      ???
  }
  def transform(e: core.Expr)(using core.DeclarationContext, ErrorReporter): jit.Term = e match {
    case core.DirectApp(b, targs, vargs, bargs) => jit.App(transform(b), (vargs map transform) ++ (bargs map transform))
    case core.Run(s) => transform(s)
    case pure: core.Pure => transform(pure)
  }
  def transform(e: core.Pure)(using DC: core.DeclarationContext, C: ErrorReporter): jit.Term = e match {
    case core.Pure.ValueVar(id, annotatedType) => jit.Var(id, transform(annotatedType))
    case core.Pure.Literal(value: Int, annotatedType) => jit.Literal.Int(value)
    case core.Pure.Literal(value: Double, annotatedType) => jit.Literal.Double(value)
    case core.Pure.Literal(value: String, annotatedType) => jit.Literal.String(value)
    case core.Pure.Literal(value, annotatedType) =>
      C.abort(s"Unsupported ${core.PrettyPrinter.format(annotatedType)} literal (or wrong scala representation).")
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
      jit.Let(definitions map transform, transform(body))
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
    case core.Stmt.Region(body) =>
      val r = jit.Var(???, transform(core.Type.inferType(body)))
      jit.Reset(???, ???, transform(body), Clause(List(r), r))
    case core.Stmt.Alloc(id, init, region, body) => ???
    case core.Stmt.Var(id, init, capture, body) => ???
    case core.Stmt.Get(id, annotatedCapt, annotatedTpe) => ???
    case core.Stmt.Put(id, annotatedCapt, value) => ???
    case core.Stmt.Try(body, handlers) =>
      jit.Let(handlers.map{ i =>
        jit.Definition(jit.Var(i.tpe.name, jit.Base.Label), jit.FreshLabel())
      }, handlers.foldRight(transform(body)){ (i, b) =>
        jit.DHandle(jit.HandlerType.Deep, jit.Var(i.tpe.name, jit.Base.Label),
          ???,
          None, b)
      })
    case core.Stmt.Hole() => jit.Primitive("hole", Nil, Nil, jit.Literal.Unit)
  }
  def transform(d: core.Definition)(using core.DeclarationContext, ErrorReporter): jit.Definition = d match {
    case core.Definition.Def(id, block) => jit.Definition(jit.Var(id, transform(core.Type.inferType(block))), transform(block))
    case core.Definition.Let(id, binding) => jit.Definition(jit.Var(id, transform(core.Type.inferType(binding))), transform(binding))
  }
  def transform(coreMod: core.ModuleDecl, mod: effekt.symbols.Module)(using C: Context): jit.Program = coreMod match {
    case core.ModuleDecl(path, imports, declarations, externs, definitions, exports) =>
      given DC: core.DeclarationContext = new core.DeclarationContext(coreMod.declarations)
      val main = C.checkMain(mod)
      val tDefinitions = definitions map transform
      jit.Program(tDefinitions, jit.App(jit.Var(main, jit.Function(Nil, jit.Base.Unit, jit.Purity.Effectful)), Nil))
  }

}
