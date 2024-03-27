package effekt
package core
import effekt.context.Context
import effekt.source.FeatureFlag

/**
 * [[Phase]] that resolves feature-flag annotated externs to unique extern (string-based) defs or new definitions.
 *
 * @param supportedFeatureFlags are a *prioritized* list (i.e., earlier ones always match).
 * Unsupported externs are replaced with a definition according to [[defaultExternBody]].
 */
case class ResolveExternDefs(supportedFeatureFlags: List[String]) extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "ResolveExternDefs"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, ModuleDecl(path, includes, declarations, externs, definitions, exports)) =>
      val resolved = externs.map(resolve)
      val newExterns = resolved.collect { case Resolved.TransformedExtern(e) => e }
      val newDefs = resolved.collect { case Resolved.NewDef(d) => d }
      Some(CoreTransformed(source, tree, mod, ModuleDecl(path, includes, declarations, newExterns, newDefs ++ definitions, exports)))
  }

  enum Resolved {
    case NewDef(d: Definition)
    case TransformedExtern(e: Extern)
    case Dropped
  }

  def defaultExternBody: ExternBody = ExternBody.EffektExternBody(FeatureFlag.Default, Stmt.Hole())

  def findPreferred(bodies: List[ExternBody], supported: List[String]): ExternBody = {
    bodies.filter { b => b.featureFlag.matches(supported) }
      .minByOption { b => supported.indexOf(b.featureFlag) }
      .getOrElse{ defaultExternBody }
  }

  def resolve(e: Extern)(using C: Context): Resolved = e match {
    case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, bodies) =>
      findPreferred(bodies, C.compiler.supportedFeatureFlags) match {
        case body@ExternBody.StringExternBody(featureFlag, contents) =>
          Resolved.TransformedExtern(Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, List(body)))
        case ExternBody.EffektExternBody(featureFlag, body) =>
          Resolved.NewDef(Definition.Def(id, Block.BlockLit(tparams, cparams, vparams, bparams, body)))
      }
    case Extern.Include(featureFlag, contents) if featureFlag.matches(C.compiler.supportedFeatureFlags) =>
      Resolved.TransformedExtern(Extern.Include(featureFlag, contents))
    case Extern.Include(_, _) => Resolved.Dropped // This include is not meant for us
  }
}
