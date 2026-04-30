package effekt
package source

import scala.util.DynamicVariable

import effekt.util.messages.EffektMessages
import effekt.context.Context
import effekt.symbols.{BlockType, ValueType, builtins, Name}
import effekt.context.Annotations
import effekt.context.Try

object GenerateImplicitArgs {

  // Stencils
  // ========

  sealed trait ImplicitStencil {
    def name: String
    def kind: String
    def explanation: Option[String] = None
  }
  case class ImplicitBlockLiteral(name: String, callTarget: symbols.CallTarget) extends ImplicitStencil {
    def kind = "block argument"
  }
  case class BoxedStencil(name: String, block: ImplicitStencil) extends ImplicitStencil {
    def kind = "value argument"
    override def explanation: Option[String] = Some(
      """An implicit argument of a boxed type will be instantiated by boxing the block
        |  that an implicit block argument of the same name would be instantiated to""".stripMargin
        + (block.explanation match {
        case Some(e) => ":\n" + e
        case None => ".\n"
      }))
  }
  case class ImplicitVar(kind: String, name: String, content: symbols.Symbol) extends ImplicitStencil
  case class SourcePosition(constructor: symbols.CallTarget) extends ImplicitStencil {
    def name = "sourcePosition"
    def kind = "value argument"
    override def explanation =
      Some(
        """Implicit sourcePosition will call
          |  SourcePosition(file, start_line, start_col, end_line, end_col)
          | with the respective values for the source position of the call.
          |""".stripMargin)
  }
  case class CallId() extends ImplicitStencil {
    def name = "callId"
    def kind = "value argument"
    override def explanation = Some("Implicit callId will generate a unique Int for each call to this function in the source code.")
  }
  case class Error(underlying: ImplicitStencil,
                   index: Int, msgs: EffektMessages) extends ImplicitStencil {
    export underlying.{name, kind}
    override def explanation: Option[String] = underlying.explanation
  }


  // Termination measure / type size
  // ===============================

  def typeSize(tpe: symbols.Type): Int = tpe match {
    case tpe: symbols.BlockType => typeSize(tpe)
    case tpe: symbols.ValueType => typeSize(tpe)
  }
  def typeSize(tpe: symbols.BlockType): Int = tpe match {
    case BlockType.FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
      tparams.length + cparams.length + vparams.map(typeSize).sum + bparams.map(typeSize).sum + typeSize(result)
    case BlockType.InterfaceType(typeConstructor, args) =>
      1 + args.map(typeSize).sum
  }
  def typeSize(tpe: symbols.ValueType): Int = tpe match {
    case ValueType.BoxedType(tpe, capture) => 1 + typeSize(tpe)
    case ValueType.ValueTypeRef(tvar) => 5
    case ValueType.ValueTypeApp(constructor, args) => 1 + args.map(typeSize).sum
  }
  def typeSize(effs: Effects)(using Context): Int =
    effs.effs.map{ r => 1 + r.args.unspan.map { t =>
      Context.resolvedType(t) match {
        case v: ValueType => 1 + typeSize(v)
        case b: BlockType => 1 + typeSize(b)
      }
    }.sum }.sum

  private val recursionStack: DynamicVariable[Map[String, (Int, Int)]] = DynamicVariable(Map.empty)

  val maxRecurse = 10
  /**
   * Wrapper for recursive type-checking of generated implicits.
   * Should fail for infinite recursion.
   */
  def recursionGuard[R](stencil: ImplicitStencil, kind: String, index: Int, inst: source.Tree, tpe: symbols.Type)(body: => R)(using Context): R = {
    val instTpe = tpe match {
      case tpe: symbols.BlockType => Context.unification(tpe)
      case tpe: symbols.ValueType => Context.unification(tpe)
    }
    val tpeSize = typeSize(instTpe)
    val newValue = inst match {
      case source.BlockLiteral(_, _, _, source.Return(source.Call(source.IdTarget(id), _, _, _, _), _), _) =>
        val (depth, lastSize) = recursionStack.value.getOrElse((id.name), (0, tpeSize))
        if (tpeSize >= lastSize && depth > maxRecurse) {
          Context.abort(s"Aborted recursive generation of implicit parameter ${id.name} after ${maxRecurse} levels with non-decreasing types at the same name.")
        }
        recursionStack.value.updated((id.name), (if tpeSize >= lastSize then depth + 1 else depth, tpeSize))
      case _ => recursionStack.value
    }
    recursionStack.withValue(newValue) {
      Try {
        body
      } match {
        case Left(msgs) =>
          // Note: Recursion
          Context.abort(util.messages.ImplicitInstantiationError(
            Error(stencil, index, msgs), tpe, Context.rangeOf(Context.focus)))
        case Right(r) => r
      }
    }
  }

  // Initial generation
  // ==================

  def resolveImplicitValue(vparam: symbols.ValueParam, scope: Option[symbols.scopes.Scope])(using Context): Option[ImplicitStencil] = {
    if (vparam.isImplicit && scope.isDefined) {
      Some(generateImplicitValueArg(vparam, symbols.scopes.Scoping(Nil, scope.get)))
    } else None
  }
  def resolveImplicitBlock(bparam: symbols.BlockParam, scope: Option[symbols.scopes.Scope])(using Context): Option[ImplicitStencil] = {
    if (bparam.isImplicit && scope.isDefined) {
      Some(generateImplicitBlockArg(bparam, symbols.scopes.Scoping(Nil, scope.get)))
    } else None
  }

  /**
   * Generate source for the given implicit value parameter, matching on the name.
   *
   * Will usually return a source.Var with the same name, but can act differently for special cases.
   * Special cases so far:
   * - sourcePosition inserts a call to SourcePosition with the components of the current source position
   */
  def generateImplicitValueArg(p: symbols.ValueParam, scope: symbols.scopes.Scoping)(using Context): ImplicitStencil = {
    (p.name.name, p.tpe) match {
      case ("sourcePosition", _) =>
        // This generates a dummy source to be name-resolved (the actual arguments will be generated later)
        val candidates = ???
        SourcePosition(symbols.CallTarget(candidates, Some(scope.scope)))
      case ("callId", _) => CallId()
      case (name, Some(symbols.BoxedType(t, capt))) =>
        // try filling boxed types by instantiating a block argument and boxing it
        BoxedStencil(name, generateImplicitBlockArg(symbols.BlockParam(p.name, Some(t), symbols.Capture.CaptureParam(p.name), true, NoSource), scope))
      case _ =>
        ImplicitVar("value argument", p.name.name, ???)
    }
  }

  /**
   * Generate source for the given implicit block parameter, matching on the name.
   *
   * Will usually return an eta-expanded (based on annotated type) call to a function with the same name.
   */
  def generateImplicitBlockArg(p: symbols.BlockParam, scope: symbols.scopes.Scoping)(using Context): ImplicitStencil =
    p.tpe.get match {
      case BlockType.FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
        val gtparams = tparams.map { p => IdDef(p.name.name, Span.missing) }
        val gvparams: List[ValueParam] =
          vparams.zipWithIndex.map { (p, i) =>
            ValueParam(IdDef(s"arg${i}", Span.missing), Some(source.ValueTypeTree(p, Span.missing)), false, Span.missing)
          }
        val gbparams: List[BlockParam] =
          bparams.zipWithIndex.map { (p, i) =>
            BlockParam(IdDef(s"block_arg${i}", Span.missing), Some(source.BlockTypeTree(p, Span.missing)), false, Span.missing)
          }
        ImplicitBlockLiteral(p.name.name, ???)
      case BlockType.InterfaceType(typeConstructor, args) =>
        // TODO eta-exapnd here, too ?
        ImplicitVar("block argument", p.name.name, ???)
    }

  // Instantiation (during typer)
  // ============================

  /**
   * Counter for integer ids for calls in the source code, to be passed as callId.
   */
  private var nextCallId: Long = 0

  private def generateResolvedId(sym: symbols.Symbol)(using Context): (source.IdDef, source.IdRef) = {
    val d = source.IdDef(sym.name.name, source.Span.missing)
    val u = source.IdRef(Nil, sym.name.name, source.Span.missing)
    Context.annotate(Annotations.Symbol, d, sym)
    Context.annotate(Annotations.Symbol, u, sym)
    (d, u)
  }

  /**
   * Called from [[Typer]] to get a fresh instance of the given implicit block argument.
   *
   * Also annotates all symbols for the returned code correctly where necessary.
   */
  def instantiateImplicitBlock(b: ImplicitStencil, tpe: symbols.BlockType)(using Context): source.Term = {
    if(!Context.messaging.hasErrors) {
      (b, tpe) match {
        case (e @ Error(s, i, msgs), _) =>
          Context.abort(util.messages.ImplicitInstantiationError(
            e, tpe, Context.rangeOf(Context.focus)))

        case (ImplicitBlockLiteral(name, sym),
          symbols.BlockType.FunctionType(tps, cps, vps, bps, res, effs)) =>
              // We need to refresh the whole binding structure, so we don't have duplicate stuff in the tree.
              // Doing this in a very specialized way here.
              // It annotates the correct concrete types for *this* invocation.
              val (ftpsyms, ftparams, ftargs) = tps.map { x =>
                val sym = symbols.TypeParam(x.name)
                val (p, a) = generateResolvedId(sym)
                (sym, p,
                  source.TypeRef(a, Many(Nil, source.Span.missing), source.Span.missing))
              }.unzip3
              val (fvpsyms, fvparams, fvargs) = vps.zipWithIndex.map { (t, i) =>
                val sym = symbols.ValueParam(Name.local(s"value_arg${i}"), Some(t), false, NoSource)
                val (p, a) = generateResolvedId(sym)
                (sym,
                  source.ValueParam(p, Some(source.ValueTypeTree(sym.tpe.get, source.Span.missing)), false, source.Span.missing): source.ValueParam,
                  source.ValueArg(None, source.Var(a, source.Span.missing), source.Span.missing))
              }.unzip3
              val (fbsyms, fbparams, fbargs) = bps.zipWithIndex.map { (t, i) =>
                val name = Name.local(s"block_arg${i}")
                val sym = symbols.BlockParam(name, Some(t), symbols.Capture.CaptureParam(name), false, NoSource)
                val (p, a) = generateResolvedId(sym)
                (sym,
                  source.BlockParam(p, Some(source.BlockTypeTree(sym.tpe.get, source.Span.missing)), false, source.Span.missing): source.BlockParam,
                  source.Var(a, source.Span.missing))
              }.unzip3
              val target = source.IdTarget(source.IdRef(Nil, name, source.Span.missing))
              Context.assignSymbol(target.id, sym)
              source.BlockLiteral(ftparams, fvparams, fbparams,
                source.Return(source.Call(target, ftargs, fvargs, fbargs,
                  source.Span.missing), source.Span.missing), source.Span.missing)

        case (ImplicitVar(kind, name, sym), _) =>
          val v = Var(IdRef(Nil, name, Span.missing), Span.missing)
          Context.assignSymbol(v.id, sym)
          v

        case _ =>
          Context.panic("Unexpected type for implicit stencil for a block argument.")
      }
    } else {
      Context.abort("Not instantiating implicit block argument since there are errors.")
    }
  }

  /**
   * Called from [[Typer]] to get a fresh instance of the given implicit value argument.
   *
   * Also annotates all symbols for the returned code correctly where necessary.
   */
  def instantiateImplicitValue(v: ImplicitStencil, tpe: symbols.ValueType)(using Context): source.ValueArg = {
    def intArg(v: Long, name: Option[String] = None): source.ValueArg =
      ValueArg(name, Literal(v, builtins.TInt, Span.missing), Span.missing)

    v match {
      case e @ Error(s, i, msgs) =>
        Context.abort(util.messages.ImplicitInstantiationError(
          e, tpe, Context.rangeOf(Context.focus)))

      case ImplicitVar(kind, name, sym) =>
        val content = Var(IdRef(Nil, name, Span.missing), Span.missing)
        Context.assignSymbol(content.id, sym)
        source.ValueArg(Some(name), content, Span.missing) // TODO Is it a problem if this is used more than once?

      case SourcePosition(sym) =>
        // this generates the version with the correct current positions,
        val pos = Context.focus.span
        val from = pos.source.offsetToPosition(pos.from)
        val to = pos.source.offsetToPosition(pos.to)
        val target = IdTarget(IdRef(Nil, "SourcePosition", Span.missing))
        val content: Call = Call(target, Nil, List(
          ValueArg(None, Literal(pos.source.name, builtins.TString, Span.missing), Span.missing),
          intArg(from.line), intArg(from.column), intArg(to.line), intArg(to.column)), Nil, Span.missing)

        // copying over the annotations generated by Namer.
        Context.assignSymbol(target.id, sym)

        // and returns the result
        source.ValueArg(Some(v.name), content, Span.missing)

      case CallId() =>
        val id = nextCallId
        nextCallId = nextCallId + 1
        intArg(id, Some(v.name))

      case BoxedStencil(name, block) =>
        val symbols.BoxedType(btpe, _) = tpe: @unchecked
        source.ValueArg(Some(v.name),
          source.Box(Maybe.None(Span.missing),
            instantiateImplicitBlock(block, btpe), Span.missing),
          Span.missing)

      case ImplicitBlockLiteral(_, _) => Context.panic("Cannot instantiate block literal as an implicit value argument.")
    }
  }

}
