package effekt
package source

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.Tree.Rewrite


/**
 * Transformation on source trees that translates programs into explicit capability-passing style
 *
 * That is, block parameters are introduced to bind capabilities and arguments are introduced at
 * the call sites. Resume is currently _not_ introduced as a block parameter.
 *
 * Also applies elaboration which conceptually is done by Typer. After this phase
 *   - there are no `Do` nodes anymore (replaced by method calls)
 *   - `l.foo(x)` where `foo` is a function and not an operation is desugared to `foo(l, x)`
 *   - all method calls `l.op()` will have `op : Operation`
 *   - all regions are explicitly bound by `region this { ... }` constructs.
 *
 */
object ExplicitCapabilities extends Phase[Typechecked, Typechecked], Rewrite {

  val phaseName = "explicit-capabilities"

  def run(input: Typechecked)(using C: Context) =
    val rewritten = C.timed(phaseName, input.source.name) { rewrite(input.tree) }

    Some(input.copy(tree = rewritten))

  override def defn(using Context) = {
    case f @ FunDef(id, tps, vps, bps, ret, body) =>
      val capabilities = Context.annotation(Annotations.BoundCapabilities, f)
      val capParams = capabilities.map(definitionFor)
      f.copy(bparams = bps ++ capParams, body = rewrite(body))
  }

  override def expr(using Context) = {

    // an effect call -- translate to method call on the inferred capability
    case c @ Do(effect, id, targs, vargs, bargs) =>
      val transformedValueArgs = vargs.map(rewrite)
      val transformedBlockArgs = bargs.map(rewrite)

      // the receiver of this effect operation call
      val receiver = Context.annotation(Annotations.CapabilityReceiver, c)

      // for bidirectional effects
      val others = Context.annotation(Annotations.CapabilityArguments, c)

      // the remaining capabilities are provided as arguments
      val capabilityArgs = others.map(referenceToCapability)

      val typeArguments = Context.annotation(Annotations.TypeArguments, c)
      val typeArgs = typeArguments.map { e => ValueTypeTree(e) }

      // construct the member selection on the capability as receiver
      MethodCall(referenceToCapability(receiver).inheritPosition(id), id, typeArgs, transformedValueArgs, transformedBlockArgs ++ capabilityArgs)

    // the function is a field, desugar to select
    case c @ Call(fun: IdTarget, targs, List(receiver), Nil) if fun.definition.isInstanceOf[Field] =>
      Select(rewrite(receiver), fun.id)

    case c @ MethodCall(receiver, id, targs, vargs, bargs) =>
      val valueArgs = vargs.map { a => rewrite(a) }
      val blockArgs = bargs.map { a => rewrite(a) }

      val capabilities = Context.annotation(Annotations.CapabilityArguments, c)
      val capabilityArgs = capabilities.map(referenceToCapability)

      val typeArguments = Context.annotation(Annotations.TypeArguments, c)
      val typeArgs = typeArguments.map { e => ValueTypeTree(e) }

      val recv = rewrite(receiver)

      MethodCall(recv, id, typeArgs, valueArgs, blockArgs ++ capabilityArgs)

    case c @ Call(recv, targs, vargs, bargs) =>
      val receiver = rewrite(recv)
      val valueArgs = vargs.map { a => rewrite(a) }
      val blockArgs = bargs.map { a => rewrite(a) }

      val capabilities = Context.annotation(Annotations.CapabilityArguments, c)
      val capabilityArgs = capabilities.map(referenceToCapability)

      val typeArguments = Context.annotation(Annotations.TypeArguments, c)
      val typeArgs = typeArguments.map { e => ValueTypeTree(e) }

      Call(receiver, typeArgs, valueArgs, blockArgs ++ capabilityArgs)

    case h @ TryHandle(prog, handlers) =>
      val body = rewrite(prog)

      val capabilities = Context.annotation(Annotations.BoundCapabilities, h)

      assert(capabilities.size == handlers.size)

      // here we use the invariant that the order of capabilities is the same as the order of handlers

      val hs = (handlers zip capabilities).map {
        case (h, cap) => visit(h) {
          // here we annotate the synthesized capability
          case h @ Handler(_, impl) => Handler(Some(definitionFor(cap)), rewrite(impl))
        }
      }

      TryHandle(body, hs)

    case n @ source.New(impl @ Implementation(interface, clauses)) => {
      val cs = clauses map {
        case op @ OpClause(id, tparams, vparams, bparams, ret, body, resume) => {
          val capabilities = Context.annotation(Annotations.BoundCapabilities, op)
          val capabilityParams = capabilities.map(definitionFor)
          OpClause(id, tparams, vparams, bparams ++ capabilityParams, ret, rewrite(body), resume)
        }
      }
      val newImpl = Implementation(interface, cs)
      val tree = source.New(newImpl)
      Context.copyAnnotations(impl, newImpl)
      tree
    }

    case b @ source.BlockLiteral(tps, vps, bps, body) =>
      val capabilities = Context.annotation(Annotations.BoundCapabilities, b)
      val capParams = capabilities.map(definitionFor)
      source.BlockLiteral(tps, vps, bps ++ capParams, rewrite(body))
  }

  def referenceToCapability(capability: BlockParam)(using C: Context): Var =
    val id = IdRef(Nil, capability.name.name)
    C.assignSymbol(id, capability)
    val ref: Var = Var(id)
    C.annotate(Annotations.InferredBlockType, ref, C.blockTypeOf(capability))
    ref

  def definitionFor(s: symbols.BlockParam)(using C: Context): source.BlockParam =
    val id = IdDef(s.name.name)
    C.assignSymbol(id, s)
    val tree: source.BlockParam = source.BlockParam(id, s.tpe.map { source.BlockTypeTree.apply })
    tree
}
