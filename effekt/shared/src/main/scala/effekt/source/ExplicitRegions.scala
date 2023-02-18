package effekt
package source

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.Tree.Rewrite


object ExplicitRegions extends Phase[Typechecked, Typechecked], Rewrite {

  val phaseName = "explicit-regions"

  def run(input: Typechecked)(using C: Context) = Some(input.copy(tree = rewrite(input.tree)))

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
