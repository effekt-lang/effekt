package effekt.source
import scala.collection.mutable
import effekt.util.messages.{ErrorReporter, EffektMessages}
import effekt.context.Context
import effekt.symbols
import effekt.symbols.scopes.Scope
import effekt.symbols.{BlockSymbol, BlockType, Callable, ImplicitContext, builtins}

object GenerateImplicitArgs {

  // for caching (to prevent infinite recursion here)
  val foundImplicits: mutable.HashMap[(Scope, BlockSymbol), ImplicitContext] = mutable.HashMap.empty

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
}
