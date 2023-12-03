package effekt
package core

import effekt.context.Context

import scala.collection.mutable
import effekt.core.Variables
import effekt.core.Variables.{ all, bound, free }

import effekt.core.normal.scope

class LambdaLifting(m: core.ModuleDecl)(using Context) extends core.Tree.Rewrite {

  val locals = Locals(m)

  /**
   * fixes the order of free variables, can vary from compilation to compilation
   */
  case class Info(values: List[Variable.Value], blocks: List[Variable.Block]) {
    def valueParams: List[core.ValueParam] = values.map { case Variable.Value(id, tpe) => core.ValueParam(id, tpe) }
    def blockParams: List[core.BlockParam] = blocks.map { case Variable.Block(id, tpe, capt) => core.BlockParam(id, tpe, capt) }
    def captureParams: List[core.Capture] = blocks.map {
      case Variable.Block(id, tpe, cs) if cs.size == 1 => cs.head
      case Variable.Block(id, tpe, cs) => Context.panic(s"Since we only close over block parameters, the capture set should be a single variable (but got ${cs})")
    }

    def valueArgs   = values.map { case Variable.Value(id, tpe) => core.ValueVar(id, tpe) }
    def blockArgs   = blocks.map { case Variable.Block(id, tpe, capt) => core.BlockVar(id, tpe, capt) }
    def captureArgs = blocks.map { case Variable.Block(id, tpe, cs) => cs }
  }
  val infos: Map[Id, Info] = locals.transitiveClosure.map {
    case (id, vars) => (id, Info(
      vars.toList.collect { case x: Variable.Value => x },
      vars.toList.collect { case f: Variable.Block => f }
    ))
  }.toMap
  val lifted: mutable.ListBuffer[core.Definition] = mutable.ListBuffer.empty

  // only needs adaptation if it is a closure
  def needsCallsiteAdaptation(id: Id) = infos.get(id) match {
    case Some(vars) => vars != Variables.empty
    case None => false
  }

  // we adapt the type of the reference since now it closes over less variables but receives more as arguments
  // e.g. (Int) => Unit at {io, f}    ===>   (Int, f: Exc) => Unit at {io}
  def adaptReference(b: BlockVar): BlockVar = b match
    case b if !needsCallsiteAdaptation(b.id) => b
    case BlockVar(id, BlockType.Function(tps, cps, vps, bps, ret), annotatedCapt) =>
      val info = infos(id)
      val additionalValues = info.values.map { x => x.tpe }
      val (additionalCaptures, additionalBlocks, removedCaptures) = info.blocks.map {
        case Variable.Block(id, tpe, capt) => (id, tpe, capt)
      }.unzip3
      val newType = BlockType.Function(tps, cps ++ additionalCaptures, vps ++ additionalValues, bps ++ additionalBlocks, ret)
      // TODO what if the block parameters have been renamed somewhere---subtracting from capture won't help then.
      val newCapture = annotatedCapt -- removedCaptures.flatten
      BlockVar(id, newType, newCapture)

    case other => Context.panic("Cannot lambda lift non-functions.")

  override def stmt = {
    case core.Scope(defs, body) =>
      scope(defs.flatMap {
        // we lift named local definitions to the toplevel
        case Definition.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
          lifted.append(Definition.Def(id,
            Renamer.rename(BlockLit(tparams,
              // Here we add cparams for the closed over bparams
              cparams ++ infos(id).captureParams,
              vparams ++ infos(id).valueParams,
              bparams ++ infos(id).blockParams,
              rewrite(body)))))
          Nil
        case other => List(rewrite(other))
      }, rewrite(body))

    case core.App(b: BlockVar, targs, vargs, bargs) if needsCallsiteAdaptation(b.id) =>
      core.App(adaptReference(b), targs, vargs.map(rewrite) ++ infos(b.id).valueArgs, bargs.map(rewrite) ++ infos(b.id).blockArgs)
  }

  override def block = {
    // Here we now need to eta expand
    // e.g. f : (Int) => Unit @ {io,exc}   ===>   { (n) => f(n, exc) }
    //   the type of f after transformation is `(Int, Exc) => Unit @ {io}`
    case f @ core.BlockVar(id, core.BlockType.Function(tps, cps, vps, bps, res), capt) if needsCallsiteAdaptation(id) =>
      val vparams: List[core.ValueParam] = vps map { tpe => core.ValueParam(Id("x"), tpe) }
      val bparams: List[core.BlockParam] = (cps zip bps) map { case (capt, tpe) => core.BlockParam(Id("f"), tpe, Set(capt)) }

      val targs = tps map { tpe => core.ValueType.Var(tpe) }
      val vargs = vparams.map { p => core.ValueVar(p.id, p.tpe) } ++ infos(id).valueArgs
      val bargs = (bparams zip cps).map { case (p, c) => core.BlockVar(p.id, p.tpe, Set(c)) } ++ infos(id).blockArgs
      core.BlockLit(tps, cps, vparams, bparams, core.App(adaptReference(f), targs, vargs, bargs))
  }

  override def expr = {
    case core.DirectApp(b: BlockVar, targs, vargs, bargs) if needsCallsiteAdaptation(b.id) =>
      core.DirectApp(b, targs, vargs.map(rewrite) ++ infos(b.id).valueArgs, bargs.map(rewrite) ++ infos(b.id).blockArgs)
    case core.PureApp(b: BlockVar, targs, vargs) if needsCallsiteAdaptation(b.id) =>
      core.PureApp(b, targs, vargs.map(rewrite) ++ infos(b.id).valueArgs)
  }
}

object LambdaLifting extends Phase[CoreTransformed, CoreTransformed] {

  val phaseName: String = "core-lambdalifting"

  def run(input: CoreTransformed)(using Context): Option[CoreTransformed] =
    input match {
      case CoreTransformed(source, tree, mod, core) =>
        Some(CoreTransformed(source, tree, mod, lift(core)))
    }

  def lift(m: core.ModuleDecl)(using Context): core.ModuleDecl =
    val lifting = new LambdaLifting(m)
    val transformed = lifting.rewrite(m)
    transformed.copy(definitions = transformed.definitions ++ lifting.lifted)
}

/**
 * Free variable computation that annotates Val and Def trees with the free variables of
 * their continuation / body, respectively.
 *
 * Use like:
 *
 *     val locals = new Locals(mod);
 *     ...
 *     locals(myValDef) // Variables(Set(), Set(f17))
 *
 * WARNING: the mapping is performed by object identity, so rewriting the tree looses the annotations.
 * WARNING: since the local-context is lost, do NOT use it by querying on demand (e.g. `locals.query(myTree)`)
 */
class Locals(mod: ModuleDecl)(using Context) extends core.Tree.Query[Variables, Variables] {



  // DB
  // --
  import effekt.context.{Annotations, Annotation}

  private val LocallyFree = Annotation[core.Tree, core.Variables](
    "LocallyFree",
    "the free variables of the tree, only considering local and not toplevel definitions"
  )

  private val db = Annotations.empty

  def apply(t: core.Val | core.Definition): core.Variables = db.apply(LocallyFree, t)

  // Monoid
  // ------
  def empty = Variables.empty
  def combine = _ ++ _

  // Scoping
  // -------
  def freeBlock(id: Id)(using L: Variables): Variables = L.filter(v => v.id == id)
  def freeValue(id: Id)(using L: Variables): Variables = L.filter(v => v.id == id)
  def binding(bound: Variables)(prog: Variables ?=> Variables)(using L: Variables): Variables =
    prog(using L ++ bound) -- bound

  override def pure(using Variables) = {
    case core.ValueVar(id, annotatedType) => freeValue(id)
  }

  override def block(using Variables) = {
    case core.BlockVar(id, annotatedTpe, annotatedCapt) => freeBlock(id)
    case core.BlockLit(tparams, cparams, vparams, bparams, body) =>
      binding(all(vparams, bound) ++ all(bparams, bound)) { query(body) }
  }

  override def operation(using Variables) = {
    case core.Operation(name, tparams, cparams, vparams, bparams, resume, body) =>
      binding(all(vparams, bound) ++ all(bparams, bound) ++ all(resume, bound)) { query(body) }
  }

  override def defn(using Variables) = {
    case d @ core.Definition.Def(id, block) =>
      val freeInDefinition = binding(bound(d)) { query(block) }
      // we annotate free variables for each definition (Def)
      db.update(LocallyFree, d, freeInDefinition)
      freeInDefinition
  }

  override def stmt(using Variables) = {
    case Stmt.Scope(defs, body) =>
      var stillFree = Variables.empty
      var boundSoFar = Variables.empty
      defs.foreach { d =>
        boundSoFar = boundSoFar ++ bound(d)
        stillFree = stillFree ++ binding(boundSoFar) { query(d) }
      }
      stillFree ++ binding(boundSoFar) { query(body) }

    case d @ Stmt.Val(id, rhs, body) =>
      query(rhs) ++ binding(Variables.value(id, rhs.tpe)) {
        // we annotate the free variables of the continuation
        val freeInBody = query(body)
        db.update(LocallyFree, d, freeInBody)
        freeInBody
      }

    case core.Alloc(id, init, region, body) =>
      val bound = Variables.block(id, Type.TState(init.tpe), Set(region))
      query(init) ++ freeBlock(region) ++ binding(bound) { query(body) }
    case core.Var(id, init, capture, body) =>
      val bound = Variables.block(id, Type.TState(init.tpe), Set(capture))
      query(init) ++ binding(bound) { query(body) }
    case core.Get(id, annotatedCapt, annotatedTpe) => freeBlock(id)
    case core.Put(id, annotatedCapt, value) => freeBlock(id)
  }

  // Initialize
  // ----------
  mod.definitions.foreach(d => query(d)(using Variables.empty))

  // maps block ids to their transitive closure
  val transitiveClosure: mutable.Map[Id, Variables] = mutable.Map.empty

  // compute transitive closure
  val freeVariablesOfDefs = db.annotationsAt(LocallyFree).collect {
    case (Annotations.Key(core.Definition.Def(id, b: core.BlockLit)), vars) => id -> vars
  }

  // saturate free variables transitively
  def resolveFreeVariables(vars: Variables): Variables =
    vars.flatMap {
      case x: Variable.Value => Set(x)
      case f: Variable.Block => resolve(f.id).getOrElse(Set(f))
    }

  def resolve(id: Id): Option[Variables] =
    transitiveClosure.get(id) match {
      case Some(value) => Some(value)
      case None =>
        freeVariablesOfDefs.get(id).map { before =>
          transitiveClosure.update(id, Variables.empty)
          val result = resolveFreeVariables(before)
          transitiveClosure.update(id, result)
          result
        }
    }


  freeVariablesOfDefs.keySet.foreach { resolve }

}
