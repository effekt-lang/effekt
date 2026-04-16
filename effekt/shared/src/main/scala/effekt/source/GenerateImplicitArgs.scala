package effekt
package source

import scala.collection.mutable
import scala.util.DynamicVariable

import effekt.util.messages.{ErrorReporter, EffektMessages}
import effekt.context.Context
import effekt.symbols.scopes.Scope
import effekt.symbols.{BlockSymbol, BlockType, Callable, ImplicitContext, builtins, Name}
import effekt.context.Annotations

object GenerateImplicitArgs {

  import effekt.symbols.ValueType

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
  def recursionGuard[R](inst: source.Term, tpe: symbols.BlockType)(body: () => R)(using Context): R = {
    val instBlockTpe = Context.unification(tpe)
    val tpeSize = typeSize(instBlockTpe)
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
      body()
    }
  }

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
  def generateImplicitValueArg(p: symbols.ValueParam)(using Context): Either[EffektMessages, ValueArg] = {
    Right(ValueArg(Some(p.name.name), p.name.name match {
      case "sourcePosition" =>
        val pos = Context.focus.span
        val from = pos.source.offsetToPosition(pos.from)
        val to = pos.source.offsetToPosition(pos.to)
        Call(IdTarget(IdRef(Nil, "SourcePosition", Span.missing)), Nil, List(
          ValueArg(None, Literal(pos.source.name, builtins.TString, Span.missing), Span.missing),
          ValueArg(None, Literal(from.line.toLong, builtins.TInt, Span.missing), Span.missing),
          ValueArg(None, Literal(from.column.toLong, builtins.TInt, Span.missing), Span.missing),
          ValueArg(None, Literal(to.line.toLong, builtins.TInt, Span.missing), Span.missing),
          ValueArg(None, Literal(to.column.toLong, builtins.TInt, Span.missing), Span.missing),
        ), Nil, Span.missing)
      case _ => Var(IdRef(Nil, p.name.name, Span.missing), Span.missing)
    }, Span.missing))
  }

  /**
   * Generate source for the given implicit block parameter, matching on the name.
   *
   * Will usually return an eta-expanded (based on annotated type) call to a function with the same name.
   */
  def generateImplicitBlockArg(p: symbols.BlockParam)(using Context): Either[EffektMessages, Term] =
    p.tpe.get match {
      case BlockType.FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
        val gtparams = tparams.map { p => IdDef(p.name.name, Span.missing) }
        val gvparams: List[ValueParam] =
          vparams.zipWithIndex.map { (p, i) => ValueParam(IdDef(s"arg${i}", Span.missing), Some(ReifiedType(p)), false, Span.missing) }
        val gbparams: List[BlockParam] =
          bparams.zipWithIndex.map { (p, i) => BlockParam(IdDef(s"block_arg${i}", Span.missing), Some(ReifiedType(p)), false, Span.missing) }
        Right(BlockLiteral(gtparams, gvparams, gbparams,
          Return(Call(IdTarget(IdRef(Nil, p.name.name, Span.missing)), Nil,
            gvparams.map { x => ValueArg(None, Var(IdRef(Nil, x.id.name, Span.missing), Span.missing), Span.missing) },
            gbparams.map { x => Var(IdRef(Nil, x.id.name, Span.missing), Span.missing) },
            Span.missing),
            Span.missing), Span.missing))
      case BlockType.InterfaceType(typeConstructor, args) =>
        // TODO eta-exapnd here, too ?
        Right(Var(IdRef(Nil, p.name.name, Span.missing), Span.missing))
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
                c.vparams.zipWithIndex.collect { case (p, i) if p.isImplicit => i -> p.name.name }.toMap,
                c.bparams.zipWithIndex.collect { case (p, i) if p.isImplicit => i -> generateImplicitBlockArg(p) }.toMap,
                c.bparams.zipWithIndex.collect { case (p, i) if p.isImplicit => i -> p.name.name }.toMap)
              foundImplicits.put((scope, b), r)
              Some(b -> r)
            case _ => None
          }
        }
      }
    }.toMap
  }

  /**
   * Called from [[Typer]] to get a fresh instance of the given implicit block argument.
   *
   * Also annotates all symbols for the returned code correctly where necessary.
   */
  def instantiateImplicitBlock(b: source.Term, tpe: symbols.BlockType)(using Context): source.Term = {
    if(!Context.messaging.hasErrors) {
      (b, tpe) match {
        case (a, symbols.BlockType.FunctionType(tps, cps, vps, bps, res, effs)) =>
          a match {
            case source.BlockLiteral(tparams, vparams, bparams, source.Return(source.Call(fn, targs, vargs, bargs, _), _), _) =>
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
                val r: source.ValueParam = source.ValueParam(source.IdDef(x.id.name, source.Span.missing), Some(source.ReifiedType(sym.tpe.get)), false, source.Span.missing)
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
                val r: source.BlockParam = source.BlockParam(source.IdDef(x.id.name, source.Span.missing), Some(source.ReifiedType(sym.tpe.get)), false, source.Span.missing)
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
            case _ => Context.panic("Unexpected implicit value for implicit block parameter")
          }
        case (a, symbols.BlockType.InterfaceType(tCons, tArgs)) =>
          a // TODO Is it a problem if this is used more than once?
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
  def instantiateImplicitValue(v: source.ValueArg, tpe: symbols.ValueType)(using Context): source.ValueArg = {
    v // TODO Is it a problem if this is used more than once?
  }

}
