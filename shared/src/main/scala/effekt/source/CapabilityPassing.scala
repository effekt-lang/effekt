package effekt
package source

import effekt.context.{ Context, ContextOps, Annotations }
import effekt.symbols._
import effekt.context.assertions.SymbolAssertions
import effekt.source.Tree.Rewrite
import effekt.substitutions._

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

      C.withCapabilities(f, effs) { caps =>
        f.copy(params = params ++ caps, body = rewrite(body))
      }
  }

  override def expr(implicit C: Context) = {

    // an effect call -- translate to method call
    case c @ Call(fun: IdTarget, targs, args) if fun.definition.isInstanceOf[EffectOp] =>
      val op = fun.definition.asEffectOp

      val tpe @ BlockType(tparams, params, ret / _) = C.blockTypeOf(op)

      val (_ / effs) = C.inferredTypeOf(c)

      // substitution of type params to inferred type arguments
      val subst = (tparams zip C.typeArguments(c)).toMap

      // Do not provide capabilities for builtin effects and also
      // omit the capability for the effect itself (if it is an effect operation)
      val self = subst.substitute(op.appliedEffect)
      val others = subst.substitute(op.otherEffects.userDefined)

      val transformedArgs = (args zip params).map { case (a, p) => rewrite(a, p) }

      val capabilityArgs = others.toList.map { e => CapabilityArg(C.capabilityReferenceFor(e)) }
      C.annotate(Annotations.CapabilityArguments, c, capabilityArgs)

      val receiver = C.capabilityReferenceFor(self)
      C.annotate(Annotations.CapabilityReceiver, c, receiver)

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
      rewrite(visit(c) { c => Call(target, targs, args) })

    // a "regular" function call
    // assumption: typer removed all ambiguous references, so there is exactly one
    case c @ Call(fun: IdTarget, targs, args) =>

      val sym: Symbol = fun.definition
      val BlockType(tparams, params, ret / effs) = C.blockTypeOf(sym)

      // substitution of type params to inferred type arguments
      val subst = (tparams zip C.typeArguments(c)).toMap
      val effects = effs.userDefined.toList.map(subst.substitute)

      val transformedArgs = (args zip params).map { case (a, p) => rewrite(a, p) }
      val capabilityArgs = effects.toList.map { e => CapabilityArg(C.capabilityReferenceFor(e)) }
      C.annotate(Annotations.CapabilityArguments, c, capabilityArgs)

      Call(fun, targs, transformedArgs ++ capabilityArgs)

    // TODO share code with Call case above
    case c @ Call(ExprTarget(expr), targs, args) =>
      val transformedExpr = rewrite(expr)
      val (FunType(BlockType(tparams, params, ret / effs), _) / _) = C.inferredTypeOf(expr)

      val subst = (tparams zip C.typeArguments(c)).toMap
      val effects = effs.userDefined.toList.map(subst.substitute)

      val transformedArgs = (args zip params).map { case (a, p) => rewrite(a, p) }
      val capabilityArgs = effects.toList.map { e => CapabilityArg(C.capabilityReferenceFor(e)) }
      C.annotate(Annotations.CapabilityArguments, c, capabilityArgs)

      Call(ExprTarget(transformedExpr), targs, transformedArgs ++ capabilityArgs)

    case f @ source.Lambda(id, params, body) =>
      val sym = f.symbol
      val effs = sym.effects.userEffects

      C.withCapabilities(f, effs) { caps =>
        f.copy(params = params ++ caps, body = rewrite(body))
      }

    case h @ TryHandle(prog, handlers) =>

      // here we need to use the effects on the handlers!
      val effects = handlers.map(_.effect.resolve)

      // TODO annotate each handler with capabilities separately
      val (caps, body) = C.withCapabilities(h, effects) { caps =>
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
        case (b @ BlockArg(ps, body), List(p: BlockType)) =>
          // here we use the blocktype as inferred by typer (after substitution)
          val effs = C.blockTypeOf(b).ret.effects.userEffects
          C.withCapabilities(b, effs) { caps =>
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
    val capsBefore = capabilities
    val result = super.in(block)
    capabilities = capsBefore
    result
  }

  /**
   * runs the given block, binding the provided capabilities, so that
   * "capabilityReferenceFor" will find them.
   */
  private[source] def withCapabilities[R](tree: source.Tree, effs: List[Effect])(block: List[source.CapabilityParam] => R): R = Context in {

    // create a fresh cabability-symbol for each bound effect
    val caps = effs.map { eff =>
      val tpe = CapabilityType(eff)
      val sym = CapabilityParam(eff.name.rename(_ + "$capability"), tpe)
      assignType(sym, tpe)
      sym
    }
    // additional block parameters for capabilities
    val params = caps.map { sym =>
      // TODO this is just an approximate position for now...
      val pos = tree
      val id = IdDef(sym.name.localName).inheritPosition(pos)
      assignSymbol(id, sym)
      val tpe = source.CapabilityType(sym.effect).inheritPosition(pos)
      source.CapabilityParam(id, tpe).inheritPosition(pos)
    }

    annotate(Annotations.CapabilityBinder, tree, params)

    // update state with capabilities
    capabilities = capabilities ++ caps.map { c => (c.effect -> c) }.toMap

    // run block
    block(params)
  }

  private[source] def capabilityReferenceFor(eff: Effect): IdRef =
    capabilities.get(eff).map { c =>
      val id = IdRef(c.name.localName)
      assignSymbol(id, c)
      //Var(id)
      id
    } getOrElse {
      Context.panic(s"Compiler error: cannot find capability for ${eff}")
    }
}
