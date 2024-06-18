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

  def defaultExternBody(warning: String)(using Context): ExternBody =
    ExternBody.Unsupported(Context.plainMessage(warning, kiama.util.Severities.Warning))

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
        val featureFlags = bodies.map(_.featureFlag)
        defaultExternBody(s"Extern definition is not supported as it is only defined for feature flags ${featureFlags.mkString(", ")}," +
          s"but the current backend only supports ${Context.compiler.supportedFeatureFlags.mkString(", ")}.")
      }
  }

  def rewrite(defn: Def)(using Context): Option[Def] = Context.focusing(defn) {
      case Def.ExternDef(capture, id, tparams, vparams, bparams, ret, bodies) =>
        findPreferred(bodies) match {
          case body@ExternBody.StringExternBody(featureFlag, template) =>
            if (featureFlag.isDefault) {
              Context.warning(s"Extern definition ${id} contains extern string without feature flag. This will likely not work in other backends, "
                + s"please annotate it with a feature flag (Supported by the current backend: ${Context.compiler.supportedFeatureFlags.mkString(", ")})")
            }

            val d = Def.ExternDef(capture, id, tparams, vparams, bparams, ret, List(body))
            Context.copyAnnotations(defn, d)
            Some(d)
          case ExternBody.EffektExternBody(featureFlag, body) =>
            val d = Def.FunDef(id, tparams, vparams, bparams, Some(ret), body)
            Context.copyAnnotations(defn, d)
            Context.annotate(Annotations.BoundCapabilities, d, Nil) // TODO ??
            Some(d)
          case u: ExternBody.Unsupported =>
            val d = Def.ExternDef(capture, id, tparams, vparams, bparams, ret, List(u))
            Context.copyAnnotations(defn, d)
            Some(d)
        }

      case Def.ExternInclude(featureFlag, path, contents, id) if featureFlag.matches(supported) =>
        if (featureFlag.isDefault) {
          val supported = Context.compiler.supportedFeatureFlags.mkString(", ")
          Context.warning("Found extern include without feature flag. It is likely that this will fail in other backends, "
            + s"please annotate it with a feature flag (Supported in current backend: ${supported})")
        }

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
