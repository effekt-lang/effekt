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

  def defaultExternBody(warning: String)(using Context): ExternBody[Nothing, Nothing] =
    ExternBody.Unsupported(Context.plainMessage(warning, kiama.util.Severities.Warning))

  def rewrite(decl: ModuleDecl)(using Context): ModuleDecl = decl match {
    case ModuleDecl(path, includes, defs, doc, span) =>
      ModuleDecl(path, includes, defs.flatMap(rewrite), doc, span)
  }

  def findPreferred[S, E](bodies: List[ExternBody[S, E]])(using Context): ExternBody[S, E] = {
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
      case Def.ExternDef(id, tparams, vparams, bparams, capture, ret, bodies, doc, span) =>
        findPreferred(bodies.unspan) match {
          case body@ExternBody.StringExternBody(featureFlag, template, span) =>
            if (featureFlag.isDefault) {
              Context.warning(s"Extern definition ${id} contains extern string without feature flag. This will likely not work in other backends, "
                + s"please annotate it with a feature flag (Supported by the current backend: ${Context.compiler.supportedFeatureFlags.mkString(", ")})")
            }

            val d = Def.ExternDef(id, tparams, vparams, bparams, capture, ret, Many(List(body), bodies.span), doc, span)
            Context.copyAnnotations(defn, d)
            Some(d)
          case ExternBody.EffektExternBody(featureFlag, body, span) =>
            val d = Def.FunDef(id, tparams, vparams, bparams, Maybe.Some(capture, capture.span), Maybe.Some(ret, ret.span), body, doc, span)
            Context.copyAnnotations(defn, d)
            Context.annotate(Annotations.BoundCapabilities, d, Nil) // TODO ??
            Some(d)
          case u: ExternBody.Unsupported =>
            val d = Def.ExternDef(id, tparams, vparams, bparams, capture, ret, Many(List(u), bodies.span), doc, span)
            Context.copyAnnotations(defn, d)
            Some(d)
        }

      case Def.ExternType(id, tparams, bodies, info, span) =>
        findPreferred(bodies.unspan) match {
          case body@ExternBody.StringExternBody(featureFlag, template, span) =>
            if (featureFlag.isDefault) {
              Context.warning(s"Extern definition ${id} contains extern string without feature flag. This will likely not work in other backends, "
                + s"please annotate it with a feature flag (Supported by the current backend: ${Context.compiler.supportedFeatureFlags.mkString(", ")})")
            }

            val d = Def.ExternType(id, tparams, Many(List(body), bodies.span), info, span)
            Context.copyAnnotations(defn, d)
            Some(d)
          case u: ExternBody.Unsupported =>
            val d = Def.ExternType(id, tparams, Many(List(u), bodies.span), info, span)
            Context.copyAnnotations(defn, d)
            Some(d)
        }

      case Def.ExternInterface(id, tparams, bodies, info, span) =>
        findPreferred(bodies.unspan) match {
          case body@ExternBody.StringExternBody(featureFlag, template, span) =>
            if (featureFlag.isDefault) {
              Context.warning(s"Extern definition ${id} contains extern string without feature flag. This will likely not work in other backends, "
                + s"please annotate it with a feature flag (Supported by the current backend: ${Context.compiler.supportedFeatureFlags.mkString(", ")})")
            }

            val d = Def.ExternInterface(id, tparams, Many(List(body), bodies.span), info, span)
            Context.copyAnnotations(defn, d)
            Some(d)
          case u: ExternBody.Unsupported =>
            val d = Def.ExternInterface(id, tparams, Many(List(u), bodies.span), info, span)
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
