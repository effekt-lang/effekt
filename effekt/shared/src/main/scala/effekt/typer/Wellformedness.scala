package effekt
package typer

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.{ CallTarget, Def, ExprTarget, IdTarget, MatchGuard, MatchPattern, Tree }
import effekt.source.Tree.{ Query, Visit }
import effekt.util.messages.ErrorReporter

case class WFContext(typesInScope: Set[TypeVar], capturesInScope: Set[Capture])

def binding(types: Set[TypeVar] = Set.empty, captures: Set[Capture] = Set.empty)(prog: WFContext ?=> Unit)(using WF: WFContext, C: Context): Unit =
  prog(using WFContext(WF.typesInScope ++ types, WF.capturesInScope ++ captures))

/**
 * Performs additional wellformed-ness checks that are easier to do on already
 * inferred and annotated trees.
 *
 * Checks:
 * - exhaustivity
 * - escape of capabilities through types
 * - scope extrusion of type parameters (of existentials and higher-rank types)
 *
 * Note: Resources (like io and global) are not treated as being free (alternatively, we could also prepopulate the WFContext)
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

    given WFContext = WFContext(Set.empty, Set.empty)

    query(tree)
  }

  override def stmt(using Context, WFContext) = {
    case stmt @ source.DefStmt(tree @ source.VarDef(id, annot, binding), rest) =>
      val tpe = Context.inferredTypeOf(rest)

      val free = freeCapture(tpe)
      val capt = tree.symbol.capture

      if free contains capt then Context.at(stmt) {
        Context.error(pp"Local variable ${id} escapes through the returned value of type ${tpe}.")
      }
  }

  enum Wellformed {
    case Yes
    case No(captures: Set[Capture], types: Set[TypeVar])
  }

  def wellformed(o: Type)(using ctx: WFContext): Wellformed =
    val captures = freeCapture(o) -- ctx.capturesInScope
    val types = freeTypes(o) -- ctx.typesInScope
    if captures.isEmpty && types.isEmpty then Wellformed.Yes
    else Wellformed.No(captures, types)

  def wellformed(o: Type, tree: Tree)(msgCapture: Set[Capture] => String)(msgType: Set[TypeVar] => String)(using ctx: WFContext, E: ErrorReporter): Unit = wellformed(o) match {
    case Wellformed.No(captures, types) =>
      if captures.nonEmpty then E.at(tree) { E.abort(msgCapture(captures)) }
      if types.nonEmpty then E.at(tree) { E.abort(msgType(types)) }
    case _ => ()
  }

  def reportCaptures(wf: Wellformed)(msg: Set[Capture] => String)(using E: ErrorReporter): Unit =
    wf match {
      case Wellformed.No(captures, types) if captures.nonEmpty => E.error(msg(captures))
      case _ => ()
    }

  def showTypes(types: Set[TypeVar]): String = types.map(TypePrinter.show).mkString(", ")
  def showCaptures(captures: Set[Capture]): String = pp"${CaptureSet(captures)}"

  override def expr(using Context, WFContext) = {

    /**
     * For handlers we check that the return type does not mention any bound capabilities
     */
    case tree @ source.TryHandle(prog, handlers) =>
      val bound = Context.annotation(Annotations.BoundCapabilities, tree).map(_.capture).toSet
      val usedEffects = Context.annotation(Annotations.InferredEffect, tree)
      val tpe = Context.inferredTypeOf(prog)

      val free = freeCapture(tpe)

      // first check prog and handlers (to report the "tightest" error)
      binding(captures = bound) { query(prog) }

      handlers foreach { query }

      // Now check the return type ...
      wellformed(tpe, prog) {
        captures => pp"The return type ${tpe} of the handled statement is not allowed to refer to any of the bound capabilities, but mentions: ${showCaptures(captures)}"
      } {
        types => pp"Type variable(s) ${showTypes(types)} escape their scope through the return type of the handled statement."
      }

      // ... and the handled effects
      usedEffects.effects.foreach { tpe =>
        wellformed(tpe, prog) {
          captures => pp"Capture(s) ${showCaptures(captures)} escape their scope as part of the effect ${tpe}"
        } {
          types => pp"Type variable(s) ${showTypes(types)} escape their scope as part of the effect ${tpe}"
        }
      }

    case tree @ source.Region(id, body) =>
      val reg = tree.symbol
      val tpe = Context.inferredTypeOf(body)

      binding(captures = Set(reg.capture)) { query(body) }

      wellformed(tpe, body) {
        captures => pp"The return type ${tpe} of the region body is not allowed to refer to region: ${showCaptures(captures)}"
      } {
        types => pp"Type variable(s) ${showTypes(types)} escape their scope through the return type of the region body."
      }

    case tree @ source.Match(scrutinee, clauses, default) => Context.at(tree) {
      // TODO copy annotations from default to synthesized defaultClause (in particular positions)
      val defaultClause = default.toList.map(body => source.MatchClause(source.IgnorePattern(), Nil, body))
      ExhaustivityChecker.checkExhaustive(scrutinee, clauses ++ defaultClause)

      query(scrutinee)
      clauses foreach { query }
      default foreach query
    }

    case tree @ source.BlockLiteral(tps, vps, bps, body) =>
      val boundTypes = tps.map(_.symbol.asTypeParam).toSet[TypeVar]
      val boundCapts = bps.map(_.id.symbol.asBlockParam.capture).toSet
      binding(types = boundTypes, captures = boundCapts) { query(body) }

    case tree @ source.Call(target, targs, vargs, bargs) =>
      target match {
        case CallTarget.IdTarget(id) => ()
        case ExprTarget(e) => query(e)
      }
      val inferredTypeArgs = Context.typeArguments(tree)
      inferredTypeArgs.foreach { tpe =>
        wellformed(tpe, tree) {
          captures => pp"Captures ${showCaptures(captures)} escape through inferred type argument ${tpe}"
        } {
          types => pp"Type variable(s) ${showTypes(types)} escape their scope through inferred type argument ${tpe}"
        }
      }
      vargs.foreach(query)
      bargs.foreach(query)

    case tree @ source.MethodCall(receiver, id, targs, vargs, bargs) =>
      query(receiver)

      val inferredTypeArgs = Context.typeArguments(tree)
      inferredTypeArgs.foreach { tpe =>
        wellformed(tpe, tree) {
          captures => pp"Captures ${showCaptures(captures)} escape through inferred type argument ${tpe}"
        } {
          types => pp"Type variable(s) ${showTypes(types)} escape their scope through inferred type argument ${tpe}"
        }
      }
      vargs.foreach(query)
      bargs.foreach(query)
  }

  // TODO bring existentials in scope
  override def query(c: source.MatchClause)(using Context, WFContext): Unit = c match {
    case source.MatchClause(pattern, guards, body) =>
      guards.foreach(query)
      query(body)
  }


  override def defn(using C: Context, WF: WFContext) = {
    case tree @ source.FunDef(id, tps, vps, bps, ret, body) =>
      val boundTypes = tps.map(_.symbol.asTypeParam).toSet[TypeVar]
      val boundCapts = bps.map(_.id.symbol.asBlockParam.capture).toSet
      binding(types = boundTypes, captures = boundCapts) { query(body) }

    case tree @ source.ExternDef(capture, id, tps, vps, bps, ret, bodies) =>
      val boundTypes = tps.map(_.symbol.asTypeParam).toSet[TypeVar]
      val boundCapts = bps.map(_.id.symbol.asBlockParam.capture).toSet
      binding(types = boundTypes, captures = boundCapts) { bodies.foreach(query) }
  }

  // Can only compute free capture on concrete sets
  def freeCapture(o: Any): Set[Capture] = o match {
    // we do not treat resources as free
    case t: symbols.CaptureParam => Set(t)
    case t: symbols.LexicalRegion => Set(t)

    case FunctionType(tparams, cparams, vparams, bparams, ret, eff) =>
      // TODO what with capabilities introduced for eff--those are bound in ret?
      freeCapture(vparams) ++ freeCapture(bparams) ++ freeCapture(ret) ++ freeCapture(eff) -- cparams.toSet
    case CaptureSet(cs) => freeCapture(cs)
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

  def freeTypes(o: Any): Set[TypeVar] = o match {
    case t: symbols.TypeVar => Set(t)
    case FunctionType(tps, cps, vps, bps, ret, effs) =>
      freeTypes(vps) ++ freeTypes(bps) ++ freeTypes(ret) ++ freeTypes(effs) -- tps.toSet
    case e: Effects            => freeTypes(e.toList)
    case _: Symbol | _: String => Set.empty // don't follow symbols
    case t: Iterable[t] =>
      t.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypes(t) }
    case p: Product =>
      p.productIterator.foldLeft(Set.empty[TypeVar]) { case (r, t) => r ++ freeTypes(t) }
    case _ =>
      Set.empty
  }

  def Context(implicit ctx: Context): Context = ctx
}
