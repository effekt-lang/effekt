package effekt
package source

import scala.collection.mutable.ListBuffer
import effekt.context.{ Context, ContextOps }
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
    case c @ Call(fun, targs, args) if c.definition.isInstanceOf[EffectOp] =>
      val op = c.definition.asEffectOp

      // if this is an effect call, we do not want to provide capabilities for the effect itself
      val ownEffect = Effects(List(op.effect))

      val BlockType(tparams, params, ret / effs) = C.blockTypeOf(op)

      // Do not provide capabilities for builtin effects and also
      // omit the capability for the effect itself (if it is an effect operation)
      val effects = (effs -- ownEffect).userDefined
      val transformedArgs = (args zip params).map { case (a, p) => rewrite(a, p) }
      val capabilityArgs = effects.toList.map { e => CapabilityArg(C.capabilityReferenceFor(e)) }

      val receiver = C.capabilityReferenceFor(op.effect)

      MethodCall(receiver, fun, targs, transformedArgs ++ capabilityArgs)

    // a "regular" function call
    // assumption: typer removed all ambiguous references, so there is exactly one
    case c @ Call(fun, targs, args) =>

      val sym: Symbol = c.definition
      val BlockType(tparams, params, ret / effs) = C.blockTypeOf(sym)

      val effects = effs.userDefined
      val transformedArgs = (args zip params).map { case (a, p) => rewrite(a, p) }
      val capabilityArgs = effects.toList.map { e => CapabilityArg(C.capabilityReferenceFor(e)) }

      Call(fun, targs, transformedArgs ++ capabilityArgs)

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
