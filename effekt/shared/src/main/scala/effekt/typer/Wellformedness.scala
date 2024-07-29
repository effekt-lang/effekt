package effekt
package typer

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.{ Def, ExprTarget, IdTarget, MatchGuard, MatchPattern, Tree }
import effekt.source.Tree.{ Query, Visit }

import scala.collection.mutable

class WFContext(var effectsInScope: List[Interface]) {
  def addEffect(i: Interface) = effectsInScope = (i :: effectsInScope).distinct
}

/**
 * Performs additional wellformed-ness checks that are easier to do on already
 * inferred and annotated trees.
 *
 * Checks:
 * - exhaustivity
 * - escape of capabilities through types
 */
object Wellformedness extends Phase[Typechecked, Typechecked], Visit[WFContext] {

  val phaseName = "wellformedness"

  def run(input: Typechecked)(using C: Context) =
    val Typechecked(source, tree, mod) = input
    C.timed(phaseName, source.name) { check(mod, tree) }

    if (Context.messaging.hasErrors) { None }
    else { Some(input) }

  def check(mod: Module, tree: source.ModuleDecl)(using Context): Unit = {

    // Effects that are lexically in scope at the top level
    // We use dependencies instead of imports, since types might mention effects of transitive imports.
    val toplevelEffects = mod.dependencies.foldLeft(mod.effects) { case (effs, mod) =>
      effs ++ mod.effects
    }

    given WFContext = WFContext(toplevelEffects.distinct)

    query(tree)
  }

  override def stmt(using Context, WFContext) = {
    case stmt @ source.DefStmt(tree @ source.VarDef(id, annot, binding), rest) =>
      val tpe = Context.inferredTypeOf(rest)

      query(rest)
      query(binding)

      val free = freeCapture(tpe)
      val capt = tree.symbol.capture

      if free contains capt then Context.at(stmt) {
        Context.error(pp"Local variable ${id} escapes through the returned value of type ${tpe}.")
      }
  }

  override def expr(using Context, WFContext) = {

    /**
     * For handlers we check that the return type does not mention any bound capabilities
     */
    case tree @ source.TryHandle(prog, handlers) =>
      val bound = Context.annotation(Annotations.BoundCapabilities, tree).map(_.capture).toSet
      val usedEffects = Context.annotation(Annotations.InferredEffect, tree)
      val tpe = Context.inferredTypeOf(prog)

      val free = freeCapture(tpe)
      val escape = free intersect bound

      if (escape.nonEmpty) {
        Context.at(prog) {
          Context.error(pp"The return type ${tpe} of the handled statement is not allowed to refer to any of the bound capabilities, but mentions: ${CaptureSet(escape)}")
        }
      }

      // also check prog and handlers
      scoped { query(prog) }

      val typesInEffects = freeTypeVars(usedEffects)

      handlers foreach { h =>
        scoped { query(h) }

        h.clauses.foreach { cl =>
          val existentials = cl.tparams.map(_.symbol.asTypeParam)
          existentials.foreach { t =>
            if (typesInEffects.contains(t)) {
              Context.error(pp"Type variable ${t} escapes its scope as part of the effect types: ${usedEffects}")
            }
          }
        }
      }

    case tree @ source.Region(id, body) =>
      val reg = tree.symbol
      val tpe = Context.inferredTypeOf(body)

      val free = freeCapture(tpe)

      if (free contains reg.capture) {
        Context.at(body) {
          Context.error(pp"The return type ${tpe} of the statement is not allowed to refer to region ${reg}")
        }
      }

      scoped { query(body) }

    case tree @ source.Match(scrutinee, clauses, default) => Context.at(tree) {
      // TODO copy annotations from default to synthesized defaultClause (in particular positions)
      val defaultClause = default.toList.map(body => source.MatchClause(source.IgnorePattern(), Nil, body))
      ExhaustivityChecker.checkExhaustive(scrutinee, clauses ++ defaultClause)

      query(scrutinee)
      clauses foreach { cl => scoped { query(cl) }}
      default foreach query
    }

    case tree @ source.BlockLiteral(tps, vps, bps, body) =>
      scoped { query(body) }
  }

  override def defn(using C: Context, WF: WFContext) = {
    case tree @ source.FunDef(id, tps, vps, bps, ret, body) =>
      scoped { query(body) }

    /**
     * Interface definitions bring an effect in scope that can be handled
     */
    case d @ source.InterfaceDef(id, tparams, ops, isEffect) =>
      WF.addEffect(d.symbol)
  }

  override def scoped(action: => Unit)(using C: Context, ctx: WFContext): Unit = {
    val before = ctx.effectsInScope
    action
    ctx.effectsInScope = before
  }

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
