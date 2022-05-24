package effekt
package source

import effekt.context.{ Context, ContextOps, Annotations }
import effekt.symbols._
import effekt.context.assertions._
import effekt.source.Tree.Rewrite
import effekt.substitutions._

/**
 * Transformation on source trees that translates programs into explicit capability-passing style
 *
 * That is, block parameters are introduced to bind capabilities and arguments are introduced at
 * the call sites. Resume is currently _not_ introduced as a block parameter.
 */
object CapabilityPassing extends Phase[Typechecked, Typechecked] with Rewrite {

  val phaseName = "capability-passing"

  def run(input: Typechecked)(implicit C: Context) =
    val transformedTree = rewrite(input.tree)
    Some(input.copy(tree = transformedTree))

  override def defn(implicit C: Context) = {
    case f @ FunDef(id, tps, vps, bps, ret, body) =>
      val sym = f.symbol
      val effs = Context.functionTypeOf(sym).effects.controlEffects

      C.withCapabilities(effs) { caps =>
        f.copy(bparams = bps ++ caps, body = rewrite(body))
      }
  }

  override def expr(implicit C: Context) = {

    // an effect call -- translate to method call
    case c @ Call(fun: IdTarget, targs, vargs, bargs) if fun.definition.isInstanceOf[Operation] =>
      val op = fun.definition.asEffectOp

      val tpe @ FunctionType(tparams, _, _, _, _, _) = C.functionTypeOf(op)

      // substitution of type params to inferred type arguments
      val subst = (tparams zip C.typeArguments(c)).toMap

      // Do not provide capabilities for builtin effects and also
      // omit the capability for the effect itself (if it is an effect operation)
      val self = subst.substitute(op.appliedEffect)
      val others = op.otherEffects.controlEffects map subst.substitute

      val transformedValueArgs = vargs.map { a => rewrite(a) }
      val transformedBlockArgs = bargs.map { a => rewrite(a) }

      val capabilityArgs = others.map { e => InterfaceArg(C.capabilityReferenceFor(e)) }
      val receiver = C.capabilityReferenceFor(self)

      val target = MemberTarget(receiver, fun.id).inheritPosition(fun)
      // C.annotateCalltarget(target, tpe)
      Call(target, targs, transformedValueArgs, transformedBlockArgs ++ capabilityArgs)

    // the target is a mutable variable --> rewrite it to an expression first, then rewrite again
    case c @ Call(fun: IdTarget, targs, vargs, bargs) if fun.definition.isInstanceOf[VarBinder] =>

      val target = visit[source.CallTarget](fun) { _ =>
        val access = Var(fun.id).inheritPosition(fun)
        // heal the missing type
        // TODO refactor this
        C.annotate(Annotations.InferredValueType, access, C.valueTypeOf(fun.definition))
        ExprTarget(access)
      }
      rewrite(visit(c) { c => Call(target, targs, vargs, bargs) })

    // a "regular" function call
    // assumption: typer removed all ambiguous references, so there is exactly one
    case c @ Call(fun: IdTarget, targs, vargs, bargs) =>

      val sym: Symbol = fun.definition
      val FunctionType(tparams, _, _, _, _, effs) = C.functionTypeOf(sym)

      // substitution of type params to inferred type arguments
      val subst = (tparams zip C.typeArguments(c)).toMap
      val effects = effs.controlEffects.toList.map(subst.substitute)

      val valueArgs = vargs.map { a => rewrite(a) }
      val blockArgs = bargs.map { a => rewrite(a) }
      val capabilityArgs = effects.map { e => InterfaceArg(C.capabilityReferenceFor(e)) }

      Call(fun, targs, valueArgs, blockArgs ++ capabilityArgs)

    // TODO share code with Call case above
    case c @ Call(ExprTarget(expr), targs, vargs, bargs) =>
      val transformedExpr = rewrite(expr)
      val BoxedType(FunctionType(tparams, cps, vps, bps, ret, effs), _) = C.inferredTypeOf(expr)

      val subst = (tparams zip C.typeArguments(c)).toMap
      val effects = effs.controlEffects.toList.map(subst.substitute)

      val valueArgs = vargs.map { a => rewrite(a) }
      val blockArgs = bargs.map { a => rewrite(a) }
      val capabilityArgs = effects.map { e => InterfaceArg(C.capabilityReferenceFor(e)) }

      Call(ExprTarget(transformedExpr), targs, valueArgs, blockArgs ++ capabilityArgs)

    case TryHandle(prog, handlers) =>

      // here we need to use the effects on the handlers!
      val effects = handlers.map(_.effect.resolve)

      val (caps, body) = C.withCapabilities(effects) { caps =>
        (caps, rewrite(prog))
      }
      val hs = (handlers zip caps).map {
        case (h, cap) => visit(h) {
          case h @ Handler(eff, _, clauses) =>
            val cls = clauses.map { cl =>
              visit(cl) {
                case OpClause(id, params, body, resume: IdDef) =>

                  // OpClause also binds a block parameter for resume, which is _not_ annotated here
                  OpClause(id, params, rewrite(body), resume)
              }
            }

            // here we annotate the synthesized capability
            Handler(eff, Some(cap), cls)
        }
      }
      TryHandle(body, hs)
  }

  override def rewrite(b: source.FunctionArg)(implicit C: Context): source.FunctionArg = visit(b) {
    case b @ source.FunctionArg(tps, vps, bps, body) =>
      // here we use the blocktype as inferred by typer (after substitution)
      val effs = C.functionTypeOf(b).effects.controlEffects
      C.withCapabilities(effs) { caps =>
        source.FunctionArg(tps, vps, bps ++ caps, rewrite(body))
      }
  }

  /**
   * Copies all annotations and position information from source to target
   */
  override def visit[T <: Tree](source: T)(block: T => T)(implicit C: Context): T = {
    val target = block(source)
    target.inheritPosition(source)
    C.copyAnnotations(source, target)
    target
  }
}
trait CapabilityPassingOps extends ContextOps { Context: Context =>

  /**
   * Used to map each lexically scoped capability to its termsymbol
   */
  private var capabilities: Map[InterfaceType, symbols.BlockParam] = Map.empty

  /**
   * Override the dynamically scoped `in` to also reset transformer state
   */
  override def in[T](block: => T): T = {
    val capsBefore = capabilities
    val result = super.in(block)
    capabilities = capsBefore
    result
  }

  /**
   * runs the given block, binding the provided capabilities, so that
   * "resolveCapability" will find them.
   */
  private[source] def withCapabilities[R](effs: List[InterfaceType])(block: List[source.BlockParam] => R): R = Context in {


    // create a fresh cabability-symbol for each bound effect
    val caps = effs.map { eff =>
      val sym = BlockParam(eff.name.rename(_ + "$capability"), eff)
      assignType(sym, eff)
      (sym, eff)
    }
    // additional block parameters for capabilities
    val params = caps.map { case (sym, eff) =>
      val id = IdDef(sym.name.name)
      assignSymbol(id, sym)
      source.BlockParam(id, source.CapabilityType(eff))
    }
    // update state with capabilities
    val newCapabilities = caps.map { case (sym, eff) => (eff -> sym) }.toMap
    capabilities = capabilities ++ newCapabilities

    // run block
    block(params)
  }

  private[source] def capabilityReferenceFor(e: InterfaceType): IdRef =
    capabilities.get(e).map { c =>
      val id = IdRef(c.name.name)
      assignSymbol(id, c)
      //Var(id)
      id
    } getOrElse {
      Context.panic(s"Compiler error: cannot find capability for ${e}, got capabilities for ${capabilities}")
    }
}
