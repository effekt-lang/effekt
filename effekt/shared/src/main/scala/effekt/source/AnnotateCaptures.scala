package effekt
package source

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.Tree.Query

import kiama.util.Source


/**
 * Computes the capture of each subexpression, provided that Typer already solved the constraints.
 *
 * TODO annotate unbox in Typer and use it here
 */
object AnnotateCaptures extends Phase[Typechecked, Typechecked], Query[Unit, CaptureSet] {

  val phaseName = "annotate-captures"

  def run(input: Typechecked)(using C: Context) =
    // reset allCaptures since multiple runs of this phase may pollute it with outdated information
    allCaptures = Nil
    annotate(input.tree, input.source)
    Some(input)

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

    case t @ source.Do(effect, op, targs, vargs, bargs) =>
      val cap = Context.annotation(Annotations.CapabilityReceiver, t)
      combineAll(vargs.map(query)) ++ combineAll(bargs.map(query)) ++ CaptureSet(cap.capture)

    case t @ source.TryHandle(prog, handlers) =>
      val progCapture = query(prog)
      val boundCapture = boundCapabilities(t)
      val usedCapture = combineAll(handlers.map(query))
      (progCapture -- boundCapture) ++ usedCapture

    case t @ source.Region(id, body) =>
      query(body) -- captureOf(id.symbol.asBlockSymbol)

    case c @ source.Call(target, targs, vargs, bargs) =>
      // TODO what's with unboxed value references???
      //  maybe that's solved by inserting explicit box and unbox in Elaboration
      val tcaps = target match {
        case IdTarget(id) => captureOf(id.symbol.asBlockSymbol)
        case ExprTarget(receiver) => query(receiver)
      }

      tcaps ++ combineAll(vargs map query) ++ combineAll(bargs map query)

    case b @ source.BlockLiteral(tps, vps, bps, body) =>
      query(body) -- boundCapabilities(b) -- CaptureSet(bps.map(_.symbol.capture))
  }

  override def stmt(using Context, Unit) = {
    // local state
    case source.DefStmt(tree @ VarDef(id, annot, binding), rest) =>
      query(binding) ++ (query(rest) -- CaptureSet(tree.symbol.capture))
  }

  override def defn(using Context, Unit) = {
    case tree @ source.FunDef(id, tps, vps, bps, ret, body) =>
      val cpt = query(body) -- boundCapabilities(tree) -- CaptureSet(bps.map(_.symbol.capture))
      // TODO Why do we need to update the annotation on the symbol here? Is the inferred capture for recursive functions
      //   wrong? Problematic example: examples/benchmarks/tree.effekt (chooseHandler has the empty set, but should have {this})
      Context.annotate(Annotations.Captures, tree.symbol, cpt)
      cpt

    // regions
    case tree @ RegDef(id, annot, region, binding) =>
      val regSymbol = region.symbol.asBlockSymbol
      val regCapture = captureOf(regSymbol)
      Context.annotate(Annotations.Captures, tree.symbol, regCapture)
      query(binding) ++ regCapture
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

  def annotate(tree: source.ModuleDecl, src: Source)(using Context): Unit =
    given Unit = ();
    query(tree)
    Context.annotate(Annotations.CaptureForFile, src, allCaptures)

  override def visit[T <: Tree](t: T)(visitor: T => CaptureSet)(using Context, Unit): CaptureSet =
    val capt = visitor(t)
    Context.annotate(Annotations.InferredCapture, t, capt)
    allCaptures = (t, capt) :: allCaptures
    capt
}
