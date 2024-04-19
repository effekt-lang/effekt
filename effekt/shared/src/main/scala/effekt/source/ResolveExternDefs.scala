package effekt.source

import effekt.Phase
import effekt.PhaseResult.Typechecked
import effekt.context.{ Context, Annotations }
import effekt.source.Tree.Rewrite

object ResolveExternDefs extends Phase[Typechecked, Typechecked] {

  val phaseName = "resolve-extern-defs"

  override def run(input: Typechecked)(using C: Context) = input match {
    case Typechecked(source, tree, mod) => Some(Typechecked(source, rewrite(tree), mod))
  }

  def supported(using Context): List[String] = Context.compiler.supportedFeatureFlags

  def defaultExternBody: ExternBody =
    ExternBody.EffektExternBody(FeatureFlag.Default, Return(Hole(Return(UnitLit()))))

  def rewrite(decl: ModuleDecl)(using Context): ModuleDecl = decl match {
    case ModuleDecl(path, includes, defs) =>
      ModuleDecl(path, includes, defs.flatMap(rewrite))
  }

  def findPreferred(bodies: List[ExternBody])(using Context): ExternBody = {
    // IMPORTANT: Should be deterministic.
    bodies.filter { b => b.featureFlag.matches(supported) }
      .minByOption { b =>
        supported.indexOf(b.featureFlag) match {
          case -1 => supported.length
          case p => p
        }
      }.getOrElse {
        defaultExternBody
      }
  }

  def rewrite(defn: Def)(using Context): Option[Def] = {
    defn match {
      case Def.ExternDef(capture, id, tparams, vparams, bparams, ret, bodies) =>
        findPreferred(bodies) match {
          case body@ExternBody.StringExternBody(featureFlag, template) =>
            val d = Def.ExternDef(capture, id, tparams, vparams, bparams, ret, List(body))
            Context.copyAnnotations(defn, d)
            Some(d)
          case ExternBody.EffektExternBody(featureFlag, body) =>
            val d = Def.FunDef(id, tparams, vparams, bparams, Some(ret), body)
            Context.copyAnnotations(defn, d)
            Context.annotate(Annotations.BoundCapabilities, d, Nil) // TODO ??
            Some(d)
        }

      case Def.ExternInclude(featureFlag, path, contents, id) if featureFlag.matches(supported) =>
        Some(defn)
      case Def.ExternInclude(_, _, _, _) => None // Drop, not for this backend

      // recurse into namespaces
      case Def.NamespaceDef(id, defs) =>
        val d = Def.NamespaceDef(id, defs.flatMap(rewrite))
        Context.copyAnnotations(defn, d)
        Some(d)
      case defn => Some(defn)
    }
  }

}
