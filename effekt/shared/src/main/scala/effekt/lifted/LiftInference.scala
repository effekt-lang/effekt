package effekt
package lifted

import effekt.Phase
import effekt.context.Context
import effekt.lifted
import effekt.core
import effekt.symbols.{ Symbol, builtins }

import effekt.context.assertions.*

object LiftInference extends Phase[CoreTransformed, CoreLifted] {

  val phaseName = "lift-inference"

  def env(using env: Environment): Environment = env

  def run(input: CoreTransformed)(using Context): Option[CoreLifted] =
    given Environment = Environment(Map.empty)
    val transformed = transform(input.core)
    Some(CoreLifted(input.source, input.tree, input.mod, transformed))

  // TODO either resolve and bind imports or use the knowledge that they are toplevel!
  def transform(mod: core.ModuleDecl)(using Environment, Context): ModuleDecl = {
    // TODO drop once we also ported lifted to use [[core.Definition]]
    val env = pretransform(mod.definitions)
    val definitions = mod.definitions.map(d => transform(d)(using env, Context))
    ModuleDecl(mod.path, mod.imports, mod.declarations.map(transform), mod.externs.map(transform), definitions, mod.exports)
  }

  def transform(param: core.Param): Param = param match {
    case core.ValueParam(id, tpe) => ValueParam(id, tpe)
    case core.BlockParam(id, tpe) => BlockParam(id, tpe)
  }

  def transform(tree: core.Block)(using Environment, Context): lifted.Block = tree match {
    case b @ core.BlockLit(tps, cps, vps, bps, body) => liftBlockLitTo(b)
    case core.Member(body, id, tpe) => Member(transform(body), id, tpe)
    case core.BlockVar(b, tpe, capt) => BlockVar(b, tpe)
    // TODO check whether this makes sense here.
    case core.Unbox(b) => Unbox(transform(b))

    case core.New(core.Implementation(interface, clauses)) =>
      val transformedMethods = clauses.map { case core.Operation(op, tps, cps, vps, bps, resume, body) =>
        // for now we reconstruct a block lit
        Operation(op, liftBlockLitTo(core.Block.BlockLit(tps, cps, vps, bps ++ resume.toList, body)))
      }
      New(Implementation(interface, transformedMethods))
  }

  def transform(tree: core.Declaration)(using Context): lifted.Decl = tree match {
    case core.Data(id, tparams, ctors) =>
      Data(id, ctors.map(c => c.id))

    case core.Interface(id, tparams, operations) =>
      Interface(id, operations.map(o => o.id))
  }

  def transform(tree: core.Extern)(using Environment, Context): lifted.Extern = tree match {
    case core.Extern.Def(id, tps, cps, vps, bps, ret, capt, body) =>
      val funTpe: core.BlockType.Function = core.BlockType.Function(tps, cps, vps.map(_.tpe), bps.map(_.tpe), ret)
      Extern.Def(id, funTpe, (vps ++ bps).map { p => transform(p) }, body)
    case core.Extern.Include(contents) =>
      Extern.Include(contents)
  }

  def transform(tree: core.Definition)(using Environment, Context): lifted.Definition = tree match {
    case core.Definition.Def(id, block) =>
      Definition.Def(id, block.tpe, transform(block))
    case core.Definition.Let(id, binding) =>
      Definition.Let(id, binding.tpe, transform(binding))
  }

  def transform(tree: core.Stmt)(using Environment, Context): Stmt = tree match {
    case core.Try(core.BlockLit(tparams, _, _, params, body), handler) =>

      val tpe = body.tpe

      // (1) Transform handlers first in unchanged environment.
      val transformedHandler = handler.map { transform }

      var environment = env

      // evidence for the region body itself
      val selfEvidence = EvidenceSymbol()

      environment = environment.adapt(selfEvidence)

      // introduce one evidence symbol per blockparam
      val transformedParams = params map {
        case p @ core.BlockParam(id, tpe) =>
          environment = environment.bind(id)
          transform(p)
      }

      // [[ try { {cap}... => s } with ... ]] = try { [ev]{cap}... => s } with ...
      val transformedBody = transform(body)(using environment, Context) // lift is provided by the handler runtime

      Try(lifted.BlockLit(tparams, EvidenceParam(selfEvidence) :: transformedParams, transformedBody), tpe, transformedHandler)

    case core.Try(_, _) => Context.panic("Should not happen. Handle always take block literals as body.")

    // [[ region { {cap}... => s } ]] = region { [ev]{cap}... => s }
    case core.Region(core.BlockLit(tparams, _, _, params, body)) =>
      var environment = env

      // evidence for the region body itself
      val selfEvidence = EvidenceSymbol()

      environment = environment.adapt(selfEvidence)

      // introduce one evidence symbol per blockparam
      val transformedParams = params map {
        case p @ core.BlockParam(id, tpe) =>
          environment = environment.bind(id)
          transform(p)
      }
      Region(lifted.BlockLit(tparams, EvidenceParam(selfEvidence) :: transformedParams, transform(body)(using environment, Context)), body.tpe)

    case core.Region(_) => Context.panic("Should not happen. Regions always take block literals as body.")

    case core.App(b: core.Block, targs, vargs, bargs) =>

      // evidence for the function itself
      val ev = env.evidenceFor(b)
      val vargsT = vargs map transform
      val (blockEv, bargsT) = transform(bargs)

      // adds evidence parameters for block arguments
      App(transform(b), targs, (ev :: blockEv) ++ vargsT ++ bargsT)

    case core.Scope(definitions, rest) =>
      val env = pretransform(definitions)
      val body = transform(rest)(using env, Context)

      Scope(definitions.map(d => transform(d)(using env, Context)), body)

    case core.Val(id, binding, body) =>
      Val(id, binding.tpe, transform(binding), transform(body))

    case core.State(id, init, region, body) =>
      State(id, transform(init), init.tpe, region, transform(body))

    case core.Match(scrutinee, clauses, default) =>
      Match(transform(scrutinee),
        clauses.map { case (c, b) => (c.asConstructor, transformBody(b)) },
        default.map { s => transform(s) })

    case core.If(cond, thn, els) =>
      If(transform(cond), transform(thn), transform(els))

    case core.Return(e) =>
      Return(transform(e))

    case core.Hole() => Hole
  }

  def transform(tree: core.Expr)(using Environment, Context): Expr = tree match {
    case core.Literal(value, tpe) =>
      Literal(value, tpe)

    case core.ValueVar(sym, tpe) =>
      ValueVar(sym, tpe)

    case core.DirectApp(b: core.Block, targs, vargs, bargs) =>
      val (ev, bargsT) = transform(bargs)
      PureApp(transform(b), targs, ev ++ vargs.map(transform) ++ bargsT)

    case core.PureApp(b: core.Block, targs, args: List[core.Expr]) =>
      PureApp(transform(b), targs, args map transform)

    case core.Select(target, field, tpe) =>
      Select(transform(target), field, tpe)

    case core.Box(b, _) =>
      Box(transform(b))

    case core.Run(s) =>
      Run(transform(s), s.tpe)
  }

  /**
   *  [[ (a){f,...} -> b ]] = [ev,ev_f,...](a){f,...} -> b
   */
  def liftBlockLitTo(b: core.BlockLit)(using Environment, Context): BlockLit = b match {
    case core.BlockLit(tps, cps, vps, bps, body) =>
      var environment = env

      // evidence for the block itself
      val selfEvidence = EvidenceSymbol()

      environment = environment.adapt(selfEvidence)

      // introduce one evidence symbol per blockparam
      val evidenceParams = bps map {
        case core.BlockParam(id, tpe) =>
          val ev = EvidenceSymbol()
          environment = environment.bind(id, ev)
          EvidenceParam(ev)
      }

      val transformedParams = EvidenceParam(selfEvidence) :: evidenceParams ++ vps.map(transform) ++ bps.map(transform)

      BlockLit(tps, transformedParams, transform(body)(using environment, Context))
  }

  /**
   * Don't transform the block itself, but only the body. Used for local abstractions like match clauses where
   * we know the evidence is Here.
   */
  def transformBody(tree: core.BlockLit)(using Environment, Context): BlockLit = tree match {
    case core.BlockLit(tps, cps, vps, bps, body) =>
      BlockLit(tps, (vps ++ bps) map transform, transform(body))
  }

  def transform(args: List[core.Block])(using Environment, Context): (List[Evidence], List[Argument]) = {
    var evidence: List[Evidence] = Nil
    val transformedArgs = args map {
      case b: core.BlockVar =>
        evidence = env.evidenceFor(b) :: evidence
        BlockVar(b.id, b.tpe)
      case b: core.Block =>
        evidence = Here() :: evidence
        transform(b)
    }
    (evidence.reverse, transformedArgs)
  }

  def transform(h: core.Implementation)(using Environment, Context): Implementation = h match {
    case core.Implementation(id, clauses) =>
      Implementation(id, clauses.map {
        // effect operations should never take any evidence as they are guaranteed (by design) to be evaluated in
        // their definition context.
        case core.Operation(op, tps, cps, vps, bps, resume, body) =>
          Operation(op, BlockLit(tps, (vps ++ bps ++ resume.toList) map transform, transform(body)))
      })
  }

  /**
   * Traverses the statement to look for function definitions.
   *
   * Important for mutually (and self) recursive functions.
   *
   * TODO add mutual blocks to core and lifted. This way we know exactly what to pretransform.
   */
  def pretransform(s: List[core.Definition])(using env: Environment, C: Context): Environment = s match {
    case core.Definition.Def(id, block) :: rest =>
      // will this ever be non-empty???
      val extendedEnv = env.bind(id, env.evidenceFor(block).scopes)
      pretransform(rest)(using extendedEnv, C)
    case _ => env
  }

  case class Environment(env: Map[Symbol, List[EvidenceSymbol]]) {
    def bind(s: Symbol) = copy(env = env + (s -> Nil))
    def bind(s: Symbol, ev: List[EvidenceSymbol]) = copy(env = env + (s -> ev))
    def bind(s: Symbol, init: EvidenceSymbol) = copy(env = env + (s -> List(init)))
    def adapt(a: EvidenceSymbol) = copy(env = env.map { case (s, as) => s -> (a :: as) })

    def evidenceFor(b: core.Block)(using Context): Evidence = b match {
      case core.BlockVar(_, tpe, _) if tpe == core.Type.TRegion => Here()
      case b: core.BlockVar => Evidence(env.getOrElse(b.id, Nil)) //.map { x => Evidence(x) }
      case b: core.BlockLit   => Here()
      case core.Member(b, id, tpe) => evidenceFor(b)
      // TODO check whether this makes any sense
      case b: core.Unbox      => Here()
      case b: core.New => Here()
    }
  }
}
