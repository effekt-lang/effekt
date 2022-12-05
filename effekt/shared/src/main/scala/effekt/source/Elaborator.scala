package effekt
package source

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.Tree.{ Query, Rewrite }

import kiama.util.Source

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
object Elaborator extends Phase[Typechecked, Typechecked] {

  val phaseName = "capability-passing"

  def run(input: Typechecked)(implicit C: Context) = {
    val transformedTree = ExplicitCapabilities.rewrite(input.tree)

    AnnotateCaptures(input.source).annotate(transformedTree)

    val treeExplicitRegions = ExplicitRegions.rewrite(transformedTree)

    Some(input.copy(tree = treeExplicitRegions))
  }
}

trait ElaborationOps extends ContextOps { Context: Context =>

  private[source] def freshReferenceTo(s: symbols.BlockParam): IdRef =
    val id = IdRef(s.name.name)
    assignSymbol(id, s)
    id

  private[source] def definitionFor(s: symbols.BlockParam): source.BlockParam =
    val id = IdDef(s.name.name)
    assignSymbol(id, s)
    val tree: source.BlockParam = source.BlockParam(id, source.BlockTypeTree(s.tpe))
    tree

}

object ExplicitCapabilities extends Rewrite {

  override def defn(using Context) = {
    case f @ FunDef(id, tps, vps, bps, ret, body) =>
      val capabilities = Context.annotation(Annotations.BoundCapabilities, f)
      val capParams = capabilities.map(Context.definitionFor)

      f.copy(bparams = bps ++ capParams, body = rewrite(body))
  }

  override def expr(using Context) = {

    // an effect call -- translate to method call on the inferred capability
    case c @ Do(effect, id, targs, vargs) =>
      val transformedValueArgs = vargs.map { a => rewrite(a) }

      // the receiver of this effect operation call
      val receiver = Context.annotation(Annotations.CapabilityReceiver, c)

      // for bidirectional effects
      val others = Context.annotation(Annotations.CapabilityArguments, c)

      // the remaining capabilities are provided as arguments
      val capabilityArgs = others.map(referenceToCapability)

      val typeArguments = Context.annotation(Annotations.TypeArguments, c)
      val typeArgs = typeArguments.map { e => ValueTypeTree(e) }

      // construct the member selection on the capability as receiver
      MethodCall(referenceToCapability(receiver).inheritPosition(id), id, typeArgs, transformedValueArgs, capabilityArgs)

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
          case h @ Handler(_, impl) => Handler(Some(Context.definitionFor(cap)), rewrite(impl))
        }
      }

      TryHandle(body, hs)

    case b @ source.BlockLiteral(tps, vps, bps, body) =>
      val capabilities = Context.annotation(Annotations.BoundCapabilities, b)
      val capParams = capabilities.map(Context.definitionFor)
      source.BlockLiteral(tps, vps, bps ++ capParams, rewrite(body))
  }

  def referenceToCapability(capability: BlockParam)(using Context): Var =
    val ref: Var = Var(Context.freshReferenceTo(capability))
    Context.annotate(Annotations.InferredBlockType, ref, Context.blockTypeOf(capability))
    ref
}

object ExplicitRegions extends Rewrite {
  override def defn(using Context) = {
    case f @ FunDef(id, tps, vps, bps, ret, body) =>
      f.copy(body = wrapInRegion(f, body))
  }

  override def expr(using Context) = {
    case f @ TryHandle(body, handler) =>
      TryHandle(wrapInRegion(f, body), handler.map(rewrite))

    case b @ BlockLiteral(tps, vps, bps, body) =>
      BlockLiteral(tps, vps, bps, wrapInRegion(b, body))
  }

  def wrapInRegion(outerTree: source.Tree, body: source.Stmt)(using Context): source.Stmt = {
    val transformedBody = rewrite(body)

    val caps = Context.annotation(Annotations.InferredCapture, body).captures
    val self = Context.annotation(Annotations.SelfRegion, outerTree)

    val usesMutableState = caps contains self.capture

    if (usesMutableState) {
      // synthesize a `region this { ... }`
      val regionId = IdDef("this").inheritPosition(outerTree)
      // assign symbol
      Context.annotate(Annotations.Symbol, regionId, self)

      // Also copy the other annotations from transformed body to the synthesized region
      //   captures of the overall region should not include self
      val region = Region(regionId, transformedBody)
      val result = Return(region)
      Context.copyAnnotations(transformedBody, region)
      Context.copyAnnotations(transformedBody, result)

      Context.annotate(Annotations.InferredCapture, region, CaptureSet(caps - self.capture))
      Context.annotate(Annotations.InferredCapture, result, CaptureSet(caps - self.capture))
      result
    } else {
      transformedBody
    }
  }
}

/**
 * Computes the capture of each subexpression, provided that Typer already solved the constraints.
 *
 * TODO remove self-regions
 * TODO annotate unbox in Typer and use it here
 */
class AnnotateCaptures(src: Source) extends Query[Unit, CaptureSet] {

  // We collect all captures while traversing the tree.
  // They are then annotated to the source file for LSP to query.
  private var allCaptures: List[(source.Tree, symbols.CaptureSet)] = Nil

  def empty: CaptureSet = CaptureSet.empty
  def combine(r1: CaptureSet, r2: CaptureSet): CaptureSet = r1 ++ r2

  override def expr(using Context, Unit) = {
    case source.Var(id) => id.symbol match {
      case b: BlockSymbol => captureOf(b)
      case x: ValueSymbol => CaptureSet.empty
    }

    case e @ source.Assign(id, expr) =>
      query(expr) ++ captureOf(id.symbol.asBlockSymbol)

    case l @ source.Box(annotatedCapture, block) =>
      query(block)
      CaptureSet.empty

    case source.Unbox(term) =>
      val capt = Context.inferredTypeOption(term) match {
        case Some(BoxedType(_, capture: CaptureSet)) => capture
        case _ => Context.panic(pp"Should have an inferred a concrete capture set for ${term}")
      }
      query(term) ++ capt

    case t @ source.Do(effect, op, targs, vargs) =>
      val cap = Context.annotation(Annotations.CapabilityReceiver, t)
      combineAll(vargs.map(query)) ++ CaptureSet(cap.capture)

    case t @ source.TryHandle(prog, handlers) =>
      val progCapture = query(prog)
      val selfRegion = Context.annotation(Annotations.SelfRegion, t)
      val boundCapture = boundCapabilities(t) ++ CaptureSet(selfRegion.capture)
      val usedCapture = combineAll(handlers.map(query))
      (progCapture -- boundCapture) ++ usedCapture

    case c @ source.Call(target, targs, vargs, bargs) =>
      // TODO what's with unboxed value references???
      //  maybe that's solved by inserting explicit box and unbox in Elaboration
      val tcaps = target match {
        case IdTarget(id) => captureOf(id.symbol.asBlockSymbol)
        case ExprTarget(receiver) => query(receiver)
      }

      tcaps ++ combineAll(vargs map query) ++ combineAll(bargs map query)

    case b @ source.BlockLiteral(tps, vps, bps, body) =>
      val selfRegion = Context.annotation(Annotations.SelfRegion, b)
      query(body) -- boundCapabilities(b) -- CaptureSet(selfRegion.capture :: bps.map(_.symbol.capture))
  }

  override def defn(using Context, Unit) = {
    /**
     * For functions we check that the self region does not leave as part of the return type.
     */
    case tree @ source.FunDef(id, tps, vps, bps, ret, body) =>
      val selfRegion = Context.annotation(Annotations.SelfRegion, tree)
      query(body) -- boundCapabilities(tree) -- CaptureSet(selfRegion.capture :: bps.map(_.symbol.capture))
//
//    // TODO explicitly annotate the self region
//    case tree @ VarDef(id, annot, region, binding) =>
//      ???
  }

  def boundCapabilities(t: Tree)(using Context): CaptureSet =
    val bound = Context.annotation(Annotations.BoundCapabilities, t)
    CaptureSet(bound.map(_.capture))

  def captureOf(b: BlockSymbol)(using Context): CaptureSet =
    asConcreteCaptureSet(Context.captureOf(b))

  def asConcreteCaptureSet(c: Captures)(using Context): CaptureSet = c match {
    case c: CaptureSet => c
    case _ => Context.panic("All capture unification variables should have been replaced by now.")
  }

  def Context(using ctx: Context): Context = ctx

  def annotate(tree: source.ModuleDecl)(using Context): Unit =
    given Unit = ();
    query(tree)
    Context.annotate(Annotations.CaptureForFile, src, allCaptures)

  override def visit[T <: Tree](t: T)(visitor: T => CaptureSet)(using Context, Unit): CaptureSet =
    val capt = visitor(t)
    Context.annotate(Annotations.InferredCapture, t, capt)
    allCaptures = (t, capt) :: allCaptures
    capt

}
