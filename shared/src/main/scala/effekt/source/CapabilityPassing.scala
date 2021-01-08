package effekt
package source

import effekt.context.{ Context, ContextOps, Annotations }
import effekt.symbols._
import effekt.context.assertions.SymbolAssertions
import effekt.source.Tree.Rewrite

/**
 * Transformation on source trees that translates programs into explicit capability-passing style
 *
 * That is, block parameters are introduced to bind capabilities and arguments are introduced at
 * the call sites. Resume is currently _not_ introduced as a block parameter.
 */
class CapabilityPassing extends Phase[ModuleDecl, ModuleDecl] with Rewrite {

  val phaseName = "capability-passing"

  def run(mod: ModuleDecl)(implicit C: Context): Option[ModuleDecl] = Context in {
    Some(rewrite(mod))
  }

  override def defn(implicit C: Context) = {
    case f @ FunDef(id, tparams, params, ret, body) =>
      val sym = f.symbol
      val effs = sym.effects.userEffects

      C.withCapabilities(effs) { caps =>
        f.copy(params = params ++ caps, body = rewrite(body))
      }
  }

  override def expr(implicit C: Context) = {

    // an effect call -- translate to method call
    case c @ Call(fun: IdTarget, targs, args) if fun.definition.isInstanceOf[EffectOp] =>
      val op = fun.definition.asEffectOp

      val tpe @ BlockType(tparams, params, ret / effs) = C.blockTypeOf(op)

      // Do not provide capabilities for builtin effects and also
      // omit the capability for the effect itself (if it is an effect operation)
      val effects = op.otherEffects.userDefined
      val transformedArgs = (args zip params).map { case (a, p) => rewrite(a, p) }
      val capabilityArgs = effects.toList.map { e => CapabilityArg(C.capabilityReferenceFor(e)) }

      val receiver = C.capabilityReferenceFor(op.effect)

      val target = MemberTarget(receiver, fun.id).inheritPosition(fun)
      C.annotateCalltarget(target, tpe)
      Call(target, targs, transformedArgs ++ capabilityArgs)

    // the target is a mutable variable --> rewrite it to an expression first, then rewrite again
    case c @ Call(fun: IdTarget, targs, args) if fun.definition.isInstanceOf[VarBinder] =>

      val target = visit[source.CallTarget](fun) { _ =>
        val access = Var(fun.id).inheritPosition(fun)
        // heal the missing type
        // TODO refactor this
        C.annotate(Annotations.TypeAndEffect, access, Effectful(C.valueTypeOf(fun.definition), Pure))
        ExprTarget(access)
      }
      rewrite(Call(target, targs, args))

    // a "regular" function call
    // assumption: typer removed all ambiguous references, so there is exactly one
    case c @ Call(fun: IdTarget, targs, args) =>

      val sym: Symbol = fun.definition
      val BlockType(tparams, params, ret / effs) = C.blockTypeOf(sym)

      val effects = effs.userDefined
      val transformedArgs = (args zip params).map { case (a, p) => rewrite(a, p) }
      val capabilityArgs = effects.toList.map { e => CapabilityArg(C.capabilityReferenceFor(e)) }

      Call(fun, targs, transformedArgs ++ capabilityArgs)

    // TODO share code with Call case above
    case c @ Call(ExprTarget(expr), targs, args) =>
      val transformedExpr = rewrite(expr)
      val (FunType(BlockType(tparams, params, ret / effs), _) / _) = C.inferredTypeOf(expr)

      val effects = effs.userDefined
      val transformedArgs = (args zip params).map { case (a, p) => rewrite(a, p) }
      val capabilityArgs = effects.toList.map { e => CapabilityArg(C.capabilityReferenceFor(e)) }

      Call(ExprTarget(transformedExpr), targs, transformedArgs ++ capabilityArgs)

    case f @ source.Lambda(id, params, body) =>
      val sym = f.symbol
      val effs = sym.effects.userEffects

      C.withCapabilities(effs) { caps =>
        f.copy(params = params ++ caps, body = rewrite(body))
      }

    case TryHandle(prog, handlers) =>

      val effects = handlers.map(_.definition)
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

  def rewrite(arg: ArgSection, param: List[symbols.Type])(implicit C: Context): ArgSection =
    visit(arg) { arg =>
      (arg, param) match {
        case (ValueArgs(as), _) =>
          ValueArgs(as.map(rewrite))
        case (BlockArg(ps, body), List(p: BlockType)) =>
          C.withCapabilities(p.ret.effects.userEffects) { caps =>
            BlockArg(ps ++ caps, rewrite(body))
          }
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
  private var capabilities: Map[Effect, symbols.Capability] = Map.empty

  /**
   * Override the dynamically scoped `in` to also reset transformer state
   */
  override def in[T](block: => T): T = {
    //    val effectsBefore = stateEffects
    val capsBefore = capabilities
    val result = super.in(block)
    //    stateEffects = effectsBefore
    capabilities = capsBefore
    result
  }

  /**
   * runs the given block, binding the provided capabilities, so that
   * "resolveCapability" will find them.
   */
  private[source] def withCapabilities[R](effs: List[UserEffect])(block: List[source.CapabilityParam] => R): R = Context in {

    // create a fresh cabability-symbol for each bound effect
    val caps = effs.map { eff =>
      val tpe = CapabilityType(eff)
      val sym = CapabilityParam(eff.name.rename(_ + "$capability"), tpe)
      assignType(sym, tpe)
      sym
    }
    // additional block parameters for capabilities
    val params = caps.map { sym =>
      val id = IdDef(sym.name.localName)
      assignSymbol(id, sym)
      source.CapabilityParam(id, source.CapabilityType(sym.effect))
    }
    // update state with capabilities
    capabilities = capabilities ++ caps.map { c => (c.effect -> c) }.toMap

    // run block
    block(params)
  }

  private[source] def capabilityReferenceFor(e: Effect): IdRef =
    capabilities.get(e).map { c =>
      val id = IdRef(c.name.localName)
      assignSymbol(id, c)
      //Var(id)
      id
    } getOrElse {
      Context.panic(s"Compiler error: cannot find capability for ${e}")
    }
}
