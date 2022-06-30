package effekt
package typer

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.{ Def, ExprTarget, IdTarget, MatchPattern, Tree }
import effekt.source.Tree.{ Query, Visit }
import effekt.typer.typeMapToSubstitution

object PostTyper extends Phase[Typechecked, Typechecked] {

  val phaseName = "post-typer"

  def run(input: Typechecked)(implicit C: Context) =
    val Typechecked(source, tree, mod) = input
    Wellformedness.check(mod, tree)

    if (Context.buffer.hasErrors) { None }
    else { Some(input) }
}

class WFContext(var effectsInScope: List[Interface]) {
  def addEffect(i: Interface) = effectsInScope = (i :: effectsInScope).distinct
}

/**
 * Performs additional wellformed-ness checks that are easier to do on already
 * inferred and annotated trees.
 *
 * Checks:
 * - exhaustivity
 * - lexical scoping of effects
 * - escape of capabilities through types
 */
object Wellformedness extends Visit[WFContext] {

  def check(mod: Module, tree: source.ModuleDecl)(using Context): Unit = {

    // Effects that are lexically in scope at the top level
    // We use dependencies instead of imports, since types might mention effects of transitive imports.
    val toplevelEffects = mod.dependencies.foldLeft(mod.effects.toList) { case (effs, mod) =>
      effs ++ mod.effects.toList
    }

    given WFContext = WFContext(extractInterfaces(toplevelEffects))

    query(tree)
  }

  override def expr(using Context, WFContext) = {

    /**
     * For handlers we check that the return type does not mention any bound capabilities
     */
    case tree @ source.TryHandle(prog, handlers) => {
      val bound = Context.annotation(Annotations.BoundCapabilities, tree).map(_.capture).toSet
      val usedEffects = Context.annotation(Annotations.InferredEffect, tree)
      val tpe = Context.inferredTypeOf(prog)
      val selfRegion = Context.getSelfRegion(tree)

      val free = freeCapture(tpe)
      val escape = free intersect bound

      if (escape.nonEmpty) {
        Context.at(prog) {
          Context.error(pp"The return type ${tpe} of the handled statement is not allowed to refer to any of the bound capabilities, but mentions: ${CaptureSet(escape)}")
        }
      }

      if (free contains selfRegion) {
        Context.at(prog) {
          Context.error(pp"The return type ${tpe} of the handler body must not mention the self region.")
        }
      }

      // also check prog and handlers
      scoped { query(prog) }

      val typesInEffects = freeTypeVars(usedEffects)

      handlers foreach { h =>
        scoped { query(h) }

        h.clauses.foreach { cl =>
          val existentials = Context.annotation(Annotations.TypeParameters, cl)
          existentials.foreach { t =>
            if (typesInEffects.contains(t)) {
              Context.error(pp"Type variable ${t} escapes its scope as part of the effect types: ${usedEffects}")
            }
          }
        }
      }
    }

    case tree @ source.Match(scrutinee, clauses) =>
      val tpe = Context.inferredTypeOf(scrutinee)
      Context.at(tree) { checkExhaustivity(tpe, clauses.map { _.pattern }) }
      query(scrutinee)
      clauses foreach { cl => scoped { query(cl) }}
  }

  override def defn(using Context, WFContext) = {
    /**
     * For functions we check that the self region does not leave as part of the return type.
     */
    case tree @ source.FunDef(id, tps, vps, bps, ret, body) =>
      val selfRegion = Context.getSelfRegion(tree)
      val returnType = Context.inferredTypeOf(body)

      if (freeCapture(returnType) contains selfRegion) {
        Context.at(body) {
          Context.error(pp"The return type ${returnType} of the function body must not mention the region of the function itself.")
        }
      }

      scoped { query(body) }

    /**
     * Interface definitions bring an effect in scope that can be handled
     */
    case d @ source.InterfaceDef(id, tparams, ops, isEffect) =>
      val effectDecl = d.symbol
      withEffect(effectDecl)
  }

  override def query(b: source.FunctionArg)(using Context, WFContext): Unit = visit(b) {
    case tree @ source.FunctionArg(tps, vps, bps, body) =>
      val selfRegion = Context.getSelfRegion(tree)
      val returnType = Context.inferredTypeOf(body)

      if (freeCapture(returnType) contains selfRegion) {
        Context.at(body) {
          Context.error(s"The return type ${returnType} of the function body must not mention the region of the function itself.")
        }
      }

      scoped { query(body) }
  }

  /**
   * This is a quick and dirty implementation of coverage checking. Both performance, and error reporting
   * can be improved a lot.
   */
  def checkExhaustivity(sc: ValueType, cls: List[MatchPattern])(using Context, WFContext): Unit = {
    import source.{ MatchPattern, AnyPattern, IgnorePattern, TagPattern }

    val catchall = cls.exists { p => p.isInstanceOf[AnyPattern] || p.isInstanceOf[IgnorePattern] }

    if (catchall)
      return ;

    sc match {
      case TypeConstructor(t: DataType) =>
        t.variants.foreach { variant =>
          checkExhaustivity(variant, cls)
        }

      case TypeConstructor(t: Record) =>
        val (related, unrelated) = cls.collect { case p: TagPattern => p }.partitionMap {
          case p if p.definition == t => Left(p.patterns)
          case p => Right(p)
        }

        if (related.isEmpty) {
          Context.error(s"Non exhaustive pattern matching, missing case for ${sc}")
        }

        (t.fields.map { f => f.tpe } zip related.transpose) foreach {
          case (t, ps) => checkExhaustivity(t, ps)
        }
      case other =>
        ()
    }
  }

  object TypeConstructor {
    def unapply(tpe: ValueType): Option[TypeConstructor] = tpe match
      case ValueTypeApp(TypeConstructor(tpe), args) => Some(tpe)
      case constructor: TypeConstructor => Some(constructor)
      case _ => None
  }

  override def scoped(action: => Unit)(using C: Context, ctx: WFContext): Unit = {
    val before = ctx.effectsInScope
    action
    ctx.effectsInScope = before
  }

  override def visit[T <: Tree](t: T)(visitor: T => Unit)(using Context, WFContext): Unit = {
    // Make sure that all annotated effects are well-scoped with regard to lexical scoping
    Context.inferredEffectOption(t).foreach { effects => Context.at(t) { wellscoped(effects) } }
    visitor(t)
  }

  def effectsInScope(using ctx: WFContext) = ctx.effectsInScope

  def withEffect(e: Interface)(using ctx: WFContext): Unit =
    ctx.addEffect(e)

  // TODO extend check to also check in value types
  //   (now that we have first class functions, they could mention effects).
  def wellscoped(effects: Effects)(using Context, WFContext): Unit = {
    def checkInterface(eff: Interface): Unit =
      if (!(effectsInScope contains eff)) Context.abort(pp"Effect ${eff} leaves its defining lexical scope as part of the inferred type.")

    def checkEffect(eff: InterfaceType): Unit = eff match {
      case e: Interface => checkInterface(e)
      case BlockTypeApp(e: Interface, args) => checkInterface(e)
      case b: BuiltinEffect => ()
      case BlockTypeApp(b: BuiltinEffect, args) => ()
    }

    effects.toList foreach checkEffect
  }

  def extractInterfaces(e: List[InterfaceType]): List[Interface] = e.collect {
    case i: Interface => i
    case BlockTypeApp(i: Interface, _) => i
  }.distinct

  // Can only compute free capture on concrete sets
  def freeCapture(o: Any): Set[Capture] = o match {
    case t: symbols.Capture   => Set(t)
    case FunctionType(tparams, cparams, vparams, bparams, ret, eff) =>
      // TODO what with capabilities introduced for eff--those are bound in ret?
      freeCapture(vparams) ++ freeCapture(bparams) ++ freeCapture(ret) ++ freeCapture(eff) -- cparams.toSet
    case CaptureSet(cs) => cs
    case x: CaptUnificationVar => sys error s"Cannot compute free variables for unification variable ${x}"
    case x: UnificationVar => sys error s"Cannot compute free variables for unification variable ${x}"
    case _: Symbol | _: String => Set.empty // don't follow symbols
    case t: Iterable[t] =>
      t.foldLeft(Set.empty[Capture]) { case (r, t) => r ++ freeCapture(t) }
    case p: Product =>
      p.productIterator.foldLeft(Set.empty[Capture]) { case (r, t) => r ++ freeCapture(t) }
    case _ =>
      Set.empty
  }

  def freeTypeVars(o: Any): Set[TypeVar] = o match {
    case t: symbols.TypeVar => Set(t)
    case FunctionType(tps, cps, vps, bps, ret, effs) =>
      freeTypeVars(vps) ++ freeTypeVars(bps) ++ freeTypeVars(ret) ++ freeTypeVars(effs) -- tps.toSet
    case e: Effects            => freeTypeVars(e.toList)
    case _: Symbol | _: String => Set.empty // don't follow symbols
    case t: Iterable[t] =>
      t.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case p: Product =>
      p.productIterator.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypeVars(t) }
    case _ =>
      Set.empty
  }

  def Context(implicit ctx: Context): Context = ctx
}
