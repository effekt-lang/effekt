package effekt
package source

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.Tree.Rewrite
import effekt.source.SpannedOps._
import effekt.typer.UnboxInference

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
    case f @ FunDef(id, tps, vps, bps, ret, cpt, body, doc, span) =>
      val capabilities = Context.annotation(Annotations.BoundCapabilities, f)
      val capParams = capabilities.map(definitionFor)
      f.copy(bparams = (bps.unspan ++ capParams).spanned(bps.span), body = rewrite(body))
    case extDef @ ExternDef(capture, id, tparams, vparams, bparams, ret, bodies, doc, span) =>
      val rewrittenBodies = bodies.map { rewrite }
      extDef.copy(bodies = rewrittenBodies)
  }

  override def expr(using Context) = {

    // an effect call -- translate to method call on the inferred capability
    case c @ Do(id, targs, vargs, bargs, span) =>
      val implicitValueArgs = Context.annotation(Annotations.ImplicitValueArguments, c)
      val implicitBlockArgs = Context.annotation(Annotations.ImplicitBlockArguments, c)

      val transformedValueArgs = (vargs.unspan ++ implicitValueArgs).map(rewrite)
      val transformedBlockArgs = (bargs.unspan ++ implicitBlockArgs).map(rewrite)

      // the receiver of this effect operation call
      val receiver = Context.annotation(Annotations.CapabilityReceiver, c)

      // for bidirectional effects
      val others = Context.annotation(Annotations.CapabilityArguments, c)

      // the remaining capabilities are provided as arguments
      val capabilityArgs = others.map(referenceToCapability)

      val typeArguments = Context.annotation(Annotations.TypeArguments, c)
      val typeArgs = typeArguments.map { e => ValueTypeTree(e, Span.missing) }

      // construct the member selection on the capability as receiver
      MethodCall(referenceToCapability(receiver).inheritPosition(id), id, typeArgs,
        Many(transformedValueArgs, vargs.span), Many(transformedBlockArgs ++ capabilityArgs, bargs.span), span.synthesized)

    // the function is a field, desugar to select
    case c @ Call(fun: IdTarget, targs, Many(List(receiver), _), Many(Nil, _), span) if fun.definition.isInstanceOf[Field] =>
      Select(rewrite(receiver.value), fun.id, span.synthesized)

    case c @ MethodCall(receiver, id, targs, vargs, bargs, span) =>
      val implicitValueArgs = Context.annotation(Annotations.ImplicitValueArguments, c)
      val implicitBlockArgs = Context.annotation(Annotations.ImplicitBlockArguments, c)

      val valueArgs = (vargs.unspan ++ implicitValueArgs).map { a => rewrite(a) }
      val blockArgs = (bargs.unspan ++ implicitBlockArgs).map { a => rewrite(a) }

      val capabilities = Context.annotation(Annotations.CapabilityArguments, c)
      val capabilityArgs = capabilities.map(referenceToCapability)

      val typeArguments = Context.annotation(Annotations.TypeArguments, c)
      val typeArgs = typeArguments.map { e => ValueTypeTree(e, Span.missing) }

      // the shape of the call follows from the chosen candidate and the kind of the receiver
      c.definition match {
        case value: (ValueSymbol | RefBinder) if UnboxInference.isBlockLike(receiver) =>
          Call(ExprTarget(Unbox(referenceToValue(value), span.synthesized)), typeArgs, Many(valueArgs, vargs.span), Many(rewrite(receiver) :: blockArgs ++ capabilityArgs, bargs.span), span)
        case value: (ValueSymbol | RefBinder) =>
          Call(ExprTarget(Unbox(referenceToValue(value), span.synthesized)), typeArgs, Many(ValueArg.Unnamed(rewrite(receiver)) :: valueArgs, vargs.span), Many(blockArgs ++ capabilityArgs, bargs.span), span)
        case _: Operation =>
          MethodCall(rewrite(UnboxInference.asBlock(receiver)), id, typeArgs, Many(valueArgs, vargs.span), Many(blockArgs ++ capabilityArgs, bargs.span), span)
        case _: Field =>
          Select(rewrite(receiver), id, span)
        case _ if UnboxInference.isBlockLike(receiver) =>
          Call(IdTarget(id), typeArgs, Many(valueArgs, vargs.span), Many(rewrite(receiver) :: blockArgs ++ capabilityArgs, bargs.span), span)
        case _ =>
          Call(IdTarget(id), typeArgs, Many(ValueArg.Unnamed(rewrite(receiver)) :: valueArgs, vargs.span), Many(blockArgs ++ capabilityArgs, bargs.span), span)
      }

    case c @ Call(recv, targs, vargs, bargs, span) =>
      val receiver = rewrite(recv)
      val implicitValueArgs = Context.annotation(Annotations.ImplicitValueArguments, c)
      val implicitBlockArgs = Context.annotation(Annotations.ImplicitBlockArguments, c)
      val valueArgs = (vargs.unspan ++ implicitValueArgs).map { a => rewrite(a) }
      val blockArgs = (bargs.unspan ++ implicitBlockArgs).map { a => rewrite(a) }

      val capabilities = Context.annotation(Annotations.CapabilityArguments, c)
      val capabilityArgs = capabilities.map(referenceToCapability)

      val typeArguments = Context.annotation(Annotations.TypeArguments, c)
      val typeArgs = typeArguments.map { e => ValueTypeTree(e, Span.missing) }

      Call(receiver, typeArgs, Many(valueArgs, vargs.span), Many(blockArgs ++ capabilityArgs, bargs.span), span)

    case h @ TryHandle(prog, handlers, span) =>
      val body = rewrite(prog)

      val capabilities = Context.annotation(Annotations.BoundCapabilities, h)

      assert(capabilities.size == handlers.size)

      // here we use the invariant that the order of capabilities is the same as the order of handlers

      val hs = (handlers zip capabilities).map {
        case (h, cap) => visit(h) {
          // here we annotate the synthesized capability
          case h @ Handler(_, impl, span) => Handler(Some(definitionFor(cap)), rewrite(impl), span)
        }
      }

      TryHandle(body, hs, span)

    case n @ source.New(impl @ Implementation(interface, clauses, implSpan), newSpan) => {
      val cs = clauses map {
        case op @ OpClause(id, tparams, vparams, bparams, ret, body, resume, span) => {
          val capabilities = Context.annotation(Annotations.BoundCapabilities, op)
          val capabilityParams = capabilities.map(definitionFor)
          OpClause(id, tparams, vparams, bparams ++ capabilityParams, ret, rewrite(body), resume, span)
        }
      }
      val newImpl = Implementation(interface, cs, implSpan)
      val tree = source.New(newImpl, newSpan)
      Context.copyAnnotations(impl, newImpl)
      tree
    }

    case b @ source.BlockLiteral(tps, vps, bps, body, span) =>
      val capabilities = Context.annotation(Annotations.BoundCapabilities, b)
      val capParams = capabilities.map(definitionFor)
      source.BlockLiteral(tps, vps, bps ++ capParams, rewrite(body), span)
  }

  override def rewrite(body: ExternBody[Term, Stmt])(using context.Context): ExternBody[Term, Stmt] =
    body match {
      case b @ source.ExternBody.StringExternBody(ff, body, span) =>
        val rewrittenTemplate =
          body.copy(
            args = body.args.map { rewrite }
          )
        b.copy(template = rewrittenTemplate)
      case b @ source.ExternBody.EffektExternBody(ff, body, span) =>
        val rewrittenBody = rewrite(body)
        b.copy(body = rewrittenBody)
      case u: source.ExternBody.Unsupported => u
    }

  def referenceToCapability(capability: BlockParam)(using C: Context): Var =
    val id = IdRef(Nil, capability.name.name, Span.missing)
    C.assignSymbol(id, capability)
    val ref: Var = Var(id, Span.missing)
    C.annotate(Annotations.InferredBlockType, ref, C.blockTypeOf(capability))
    ref

  def referenceToValue(value: ValueSymbol | RefBinder)(using C: Context): Var =
    val id = IdRef(Nil, value.name.name, Span.missing)
    C.assignSymbol(id, value)
    val ref: Var = Var(id, Span.missing)
    val tpe = value match {
      case v: ValueSymbol => C.valueTypeOf(v)
      case r: RefBinder => C.stateTypeOf(r)
    }
    C.annotate(Annotations.InferredValueType, ref, tpe)
    ref

  def definitionFor(s: symbols.BlockParam)(using C: Context): source.BlockParam =
    val id = IdDef(s.name.name, Span.missing)
    C.assignSymbol(id, s)
    // the capability's type as solved (its declared type may hold unification variables)
    val tree: source.BlockParam = source.BlockParam(id, C.blockTypeOption(s).orElse(s.tpe).map { source.BlockTypeTree(_, Span.missing) }, isImplicit=false, Span.missing)
    tree
}
