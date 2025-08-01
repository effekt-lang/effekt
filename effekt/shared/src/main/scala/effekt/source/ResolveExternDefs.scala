package effekt.source

import effekt.Phase
import effekt.PhaseResult.Typechecked
import effekt.context.{ Context, Annotations }
import effekt.source.SpannedOps._

object ResolveExternDefs extends Phase[Typechecked, Typechecked] {

  val phaseName = "resolve-extern-defs"

  override def run(input: Typechecked)(using C: Context) = input match {
    case Typechecked(source, tree, mod) => Some(Typechecked(source, rewrite(tree), mod))
  }

  def supported(using Context): List[String] = Context.compiler.supportedFeatureFlags

  def defaultExternBody(warning: String)(using Context): ExternBody =
    ExternBody.Unsupported(Context.plainMessage(warning, kiama.util.Severities.Warning))

  def rewrite(decl: ModuleDecl)(using Context): ModuleDecl = decl match {
    case ModuleDecl(path, includes, defs, doc, span) =>
      ModuleDecl(path, includes, defs.flatMap(rewrite), doc, span)
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
      case Def.ExternDef(capture, id, tparams, vparams, bparams, ret, bodies, doc, span) =>
        findPreferred(bodies) match {
          case body@ExternBody.StringExternBody(featureFlag, template, span) =>
            if (featureFlag.isDefault) {
              Context.warning(s"Extern definition ${id} contains extern string without feature flag. This will likely not work in other backends, "
                + s"please annotate it with a feature flag (Supported by the current backend: ${Context.compiler.supportedFeatureFlags.mkString(", ")})")
            }

            val d = Def.ExternDef(capture, id, tparams, vparams, bparams, ret, List(body), doc, span)
            Context.copyAnnotations(defn, d)
            Some(d)
          case ExternBody.EffektExternBody(featureFlag, body, span) =>
            val d = Def.FunDef(id, tparams, vparams, bparams, Some(ret).spanned(ret.span), body, doc, span)
            Context.copyAnnotations(defn, d)
            Context.annotate(Annotations.BoundCapabilities, d, Nil) // TODO ??
            Some(d)
          case u: ExternBody.Unsupported =>
            val d = Def.ExternDef(capture, id, tparams, vparams, bparams, ret, List(u), doc, span)
            Context.copyAnnotations(defn, d)
            Some(d)
        }

      case Def.ExternInclude(featureFlag, path, contents, id, doc, span) if featureFlag.matches(supported) =>
        if (featureFlag.isDefault) {
          val supported = Context.compiler.supportedFeatureFlags.mkString(", ")
          Context.warning("Found extern include without feature flag. It is likely that this will fail in other backends, "
            + s"please annotate it with a feature flag (Supported in current backend: ${supported})")
        }

        Some(defn)
      case Def.ExternInclude(_, _, _, _, _, _) => None // Drop, not for this backend

      // recurse into namespaces
      case Def.NamespaceDef(id, defs, doc, span) =>
        val d = Def.NamespaceDef(id, defs.flatMap(rewrite), doc, span)
        Context.copyAnnotations(defn, d)
        Some(d)
      case defn => Some(defn)
  }

}
