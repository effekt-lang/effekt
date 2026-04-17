package effekt
package source

import scala.collection.mutable
import scala.util.DynamicVariable

import effekt.util.messages.EffektMessages
import effekt.context.Context
import effekt.symbols.scopes.Scope
import effekt.symbols.{BlockSymbol, BlockType, ValueType, Callable, ImplicitContext, builtins, Name}
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
  case class ImplicitBlockLiteral(name: String, content: source.BlockLiteral) extends ImplicitStencil {
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
  case class ImplicitVar(kind: String, name: String, content: source.Var) extends ImplicitStencil
  case class SourcePosition(content: source.Call) extends ImplicitStencil {
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

  // Initial generation (during namer)
  // =================================

  /**
   * Map for caching the result of [[lookupPotentialImplicits]] (to prevent infinite recursion in Namer)
   */
  val foundImplicits: mutable.HashMap[(Scope, BlockSymbol), ImplicitContext] = mutable.HashMap.empty


  /**
   * Generate source for the given implicit value parameter, matching on the name.
   *
   * Will usually return a source.Var with the same name, but can act differently for special cases.
   * Special cases so far:
   * - sourcePosition inserts a call to SourcePosition with the components of the current source position
   */
  def generateImplicitValueArg(p: symbols.ValueParam)(using Context): ImplicitStencil = {
    (p.name.name, p.tpe) match {
      case ("sourcePosition", _) =>
        // This generates a dummy source to be name-resolved (the actual arguments will be generated later)
        SourcePosition(Call(IdTarget(IdRef(Nil, "SourcePosition", Span.missing)), Nil, List(
          ValueArg(None, Literal("<dummy>", builtins.TString, Span.missing), Span.missing),
          ValueArg(None, Literal(-1L, builtins.TInt, Span.missing), Span.missing),
          ValueArg(None, Literal(-1L, builtins.TInt, Span.missing), Span.missing),
          ValueArg(None, Literal(-1L, builtins.TInt, Span.missing), Span.missing),
          ValueArg(None, Literal(-1L, builtins.TInt, Span.missing), Span.missing),
        ), Nil, Span.missing))
      case ("callId", _) => CallId()
      case (name, Some(symbols.BoxedType(t, capt))) =>
        // try filling boxed types by instantiating a block argument and boxing it
        BoxedStencil(name, generateImplicitBlockArg(symbols.BlockParam(p.name, Some(t), symbols.Capture.CaptureParam(p.name), true, NoSource)))
      case _ => ImplicitVar("value argument", p.name.name, Var(IdRef(Nil, p.name.name, Span.missing), Span.missing))
    }
  }

  /**
   * Generate source for the given implicit block parameter, matching on the name.
   *
   * Will usually return an eta-expanded (based on annotated type) call to a function with the same name.
   */
  def generateImplicitBlockArg(p: symbols.BlockParam)(using Context): ImplicitStencil =
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
        ImplicitBlockLiteral(p.name.name, BlockLiteral(gtparams, gvparams, gbparams,
          Return(Call(IdTarget(IdRef(Nil, p.name.name, Span.missing)), Nil,
            gvparams.map { x => ValueArg(None, Var(IdRef(Nil, x.id.name, Span.missing), Span.missing), Span.missing) },
            gbparams.map { x => Var(IdRef(Nil, x.id.name, Span.missing), Span.missing) },
            Span.missing),
            Span.missing), Span.missing))
      case BlockType.InterfaceType(typeConstructor, args) =>
        // TODO eta-exapnd here, too ?
        ImplicitVar("block argument", p.name.name, Var(IdRef(Nil, p.name.name, Span.missing), Span.missing))
    }

  /**
   * Returns an initial [[ImplicitContext]] for each of the [[BlockSymbol]]s.
   *
   * Is called from [[Namer]] to still be in the correct context to name-resolve the implicit arguments
   * in [[Namer.resolveImplicits]].
   */
  def lookupPotentialImplicits(forCandidates: List[Set[BlockSymbol]], scope: Scope)(using Context): Map[BlockSymbol, ImplicitContext] = {
    forCandidates.flatMap { level =>
      level.flatMap { b =>
        def findCached(b: BlockSymbol, scope: Scope): Option[ImplicitContext] = {
          foundImplicits.get((scope, b)).orElse {
            scope match {
              case Scope.Global(_, _) => None
              case Scope.Named(_, _, outer) => findCached(b, outer)
              case Scope.Local(_, _, _, outer) => findCached(b, outer)
            }
          }
        }

        findCached(b, scope).map(b -> _).orElse {
          b match {
            // walks up scopes, because block parameters should be eta-expanded below
            case c: Callable =>
              val r = ImplicitContext(
                c.vparams.zipWithIndex.collect { case (p, i) if p.isImplicit => i -> generateImplicitValueArg(p) }.toMap,
                c.bparams.zipWithIndex.collect { case (p, i) if p.isImplicit => i -> generateImplicitBlockArg(p) }.toMap)
              foundImplicits.put((scope, b), r)
              Some(b -> r)
            case _ => None
          }
        }
      }
    }.toMap
  }

  // Instantiation (during typer)
  // ============================

  /**
   * Counter for integer ids for calls in the source code, to be passed as callId.
   */
  private var nextCallId: Long = 0

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

        case (ImplicitBlockLiteral(name, source.BlockLiteral(tparams, vparams, bparams, source.Return(source.Call(fn, targs, vargs, bargs, _), _), _)),
          symbols.BlockType.FunctionType(tps, cps, vps, bps, res, effs)) =>
              // We need to refresh the whole binding structure, so we don't have duplicate stuff in the tree.
              // Doing this in a very specialized way here.
              // It annotates the correct concrete types for *this* invocation.
              val ftpsyms = tparams.map { x => symbols.TypeParam(Name.local(x.name)) }
              val ftparams = (tparams zip ftpsyms).map { (x, sym) =>
                val r = source.IdDef(x.name, source.Span.missing)
                Context.annotate(Annotations.Symbol, r, sym)
                r
              }
              val ftargs = ftpsyms.map { x =>
                val r = source.TypeRef(source.IdRef(Nil, x.name.name, source.Span.missing), Many(Nil, source.Span.missing), source.Span.missing)
                Context.annotate(Annotations.Symbol, r, x)
                r
              }
              val fvpsyms = (vparams zip vps).map { (x, t) => symbols.ValueParam(Name.local(x.id.name), Some(t), false, NoSource) }
              val fvparams = (vparams zip fvpsyms).map { (x, sym) =>
                val r: source.ValueParam = source.ValueParam(source.IdDef(x.id.name, source.Span.missing),
                  Some(source.ValueTypeTree(sym.tpe.get, source.Span.missing)), false, source.Span.missing)
                Context.annotate(Annotations.Symbol, r, sym)
                Context.annotate(Annotations.Symbol, r.id, sym)
                r
              }
              val fvargs = fvpsyms.map { x =>
                val r = source.Var(source.IdRef(Nil, x.name.name, source.Span.missing), source.Span.missing)
                Context.annotate(Annotations.Symbol, r, x)
                Context.annotate(Annotations.Symbol, r.id, x)
                source.ValueArg(None, r, source.Span.missing)
              }
              val fbpsyms = (bparams zip bps).map { (x, t) => symbols.BlockParam(Name.local(x.id.name), Some(t), x.symbol.capture, false, NoSource) }
              val fbparams = (bparams zip fbpsyms).map { (x, sym) =>
                val r: source.BlockParam = source.BlockParam(source.IdDef(x.id.name, source.Span.missing),
                  Some(source.BlockTypeTree(sym.tpe.get, source.Span.missing)), false, source.Span.missing)
                Context.annotate(Annotations.Symbol, r, sym)
                Context.annotate(Annotations.Symbol, r.id, sym)
                r
              }
              val fbargs = fbpsyms.map { x =>
                val r = source.Var(source.IdRef(Nil, x.name.name, source.Span.missing), source.Span.missing)
                Context.annotate(Annotations.Symbol, r, x)
                Context.annotate(Annotations.Symbol, r.id, x)
                r
              }
              val ffn = fn match {
                case source.IdTarget(id) =>
                  val r = source.IdTarget(source.IdRef(Nil, id.name, source.Span.missing))
                  Context.annotate(Annotations.Symbol, r.id,
                    id.symbol match {
                      case symbols.CallTarget(syms, impls) =>
                        symbols.CallTarget(syms, impls) // needs to be refreshed for recursive uses
                    })
                  r
                case _ => Context.panic("Implicit block argument should be an (eta-expanded) name, not an expression")
              }
              source.BlockLiteral(ftparams, fvparams, fbparams,
                source.Return(source.Call(ffn, ftargs, fvargs, fbargs,
                  source.Span.missing), source.Span.missing), source.Span.missing)

        case (ImplicitVar(kind, name, b), _) =>
          b // TODO Is it a problem if this is used more than once?

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
    v match {
      case e @ Error(s, i, msgs) =>
        Context.abort(util.messages.ImplicitInstantiationError(
          e, tpe, Context.rangeOf(Context.focus)))

      case ImplicitVar(kind, name, content) =>
        source.ValueArg(Some(name), content, Span.missing) // TODO Is it a problem if this is used more than once?

      case SourcePosition(content) =>
        // this generates the version with the correct current positions,
        val pos = Context.focus.span
        val from = pos.source.offsetToPosition(pos.from)
        val to = pos.source.offsetToPosition(pos.to)
        val code: Call = Call(IdTarget(IdRef(Nil, "SourcePosition", Span.missing)), Nil, List(
          ValueArg(None, Literal(pos.source.name, builtins.TString, Span.missing), Span.missing),
          ValueArg(None, Literal(from.line.toLong, builtins.TInt, Span.missing), Span.missing),
          ValueArg(None, Literal(from.column.toLong, builtins.TInt, Span.missing), Span.missing),
          ValueArg(None, Literal(to.line.toLong, builtins.TInt, Span.missing), Span.missing),
          ValueArg(None, Literal(to.column.toLong, builtins.TInt, Span.missing), Span.missing),
        ), Nil, Span.missing)

        // copying over the annotations generated by Namer.
        val (x: IdTarget, y: IdTarget) = (content.target, code.target): @unchecked
        Context.copyAnnotations(x, y)
        Context.copyAnnotations(x.id, y.id)

        // and returns the result
        source.ValueArg(Some(v.name), code, Span.missing)

      case CallId() =>
        val id = nextCallId
        nextCallId = nextCallId + 1
        source.ValueArg(Some(v.name), Literal(id, builtins.TInt, Span.missing), Span.missing)

      case BoxedStencil(name, block) =>
        val symbols.BoxedType(btpe, _) = tpe: @unchecked
        source.ValueArg(Some(v.name),
          source.Box(Maybe.None(Span.missing),
            instantiateImplicitBlock(block, btpe), Span.missing),
          Span.missing)

      case ImplicitBlockLiteral(_, _) => Context.panic("Cannot instantiate block literal as an implicit value argument.")
    }
  }

  // Running other phases on it
  // ==========================

  /**
   * Run body on each of the stencil code parts, generating an Error context if errors occur.
   *
   * This is used to pass ImplicitStencils through other phases (currently, Namer).
   */
  def runPhaseOn[A](i: Int, s: ImplicitStencil)(body: source.Term => Either[EffektMessages, Unit]): ImplicitStencil = {
    def runOn(s: ImplicitStencil): Either[EffektMessages, Unit] =
      s match {
        case ImplicitBlockLiteral(name, content) => body(content)
        case ImplicitVar(kind, name, content) => body(content)
        case SourcePosition(content) => body(content)
        case BoxedStencil(_, block) => runOn(block)
        case CallId() => Right(())
        case Error(_, _, _) => Right(())
      }
    runOn(s) match {
      case Left(msgs) => Error(s, i, msgs)
      case Right(()) => s
    }
  }

}
