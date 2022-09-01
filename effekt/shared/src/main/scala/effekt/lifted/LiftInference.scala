package effekt
package lifted

import effekt.Phase
import effekt.context.Context
import effekt.lifted
import effekt.core
import effekt.symbols.{ Symbol, builtins }

object LiftInference extends Phase[CoreTransformed, CoreLifted] {

  val phaseName = "lift-inference"

  def env(using env: Environment): Environment = env

  def run(input: CoreTransformed)(using Context): Option[CoreLifted] =
    given Environment = Environment(Map.empty)
    val transformed = transform(input.core)
    Some(CoreLifted(input.source, input.tree, input.mod, transformed))

  // TODO either resolve and bind imports or use the knowledge that they are toplevel!
  def transform(mod: core.ModuleDecl)(using Environment, Context): ModuleDecl =
    ModuleDecl(mod.path, mod.imports, transform(mod.defs), mod.exports)

  def transform(param: core.Param): Param = param match {
    case core.ValueParam(id, tpe) => ValueParam(id, tpe)
    case core.BlockParam(id, tpe) => BlockParam(id, tpe)
  }

  // [[ a -> b ]] = [ev] -> a -> b
  def transform(tree: core.Block, self: Option[Symbol] = None)(using Environment, Context): Block = tree match {
    case b @ core.BlockLit(params, body) =>
      val id = ScopeId()
      ScopeAbs(id, liftBlockLitTo(b, id, self))
    case core.Member(body, id) => Member(transform(body), id)
    case core.Extern(params, body) => Extern(params.map { p => transform(p) }, body)
    case core.BlockVar(b) => BlockVar(b)
    // TODO check whether this makes sense here.
    case core.Unbox(b) => Unbox(transform(b))

    case core.New(core.Handler(interface, clauses)) =>
      val id = ScopeId()
      val transformedMethods = clauses.map { case (op, block) => (op, liftBlockLitTo(block, id)) }
      ScopeAbs(id, New(Handler(interface, transformedMethods)))
  }

  def transform(tree: core.Stmt)(using Environment, Context): Stmt = tree match {
    case core.Def(id, tpe, block, rest) =>
      Def(id, tpe, transform(block, Some(id)), transform(rest)(using env.bind(id), Context))

    case core.Val(id, tpe, binding, body) =>
      Val(id, tpe, transform(binding), transform(body))

    case core.Let(id, tpe, binding, body) =>
      Let(id, tpe, transform(binding), transform(body))

    case core.State(id, init, region, body) =>
      State(id, transform(init), region, transform(body))

    case core.Data(id, ctors, rest) =>
      Data(id, ctors, transform(rest))

    case core.Record(id, fields, rest) =>
      Record(id, fields, transform(rest))

    case core.Handle(body, handler) =>
      val transformedBody = transform(body) // lift is provided by the handler runtime
      val transformedHandler = handler.map { transform }
      Handle(transformedBody, transformedHandler)

    // right now the region construct is a noop. This can change in the future
    case core.Region(body) =>
      Region(transform(body))

    case core.App(b: core.Block, targs, args: List[core.Argument]) =>
      b match {
        case b : core.Extern => App(transform(b), targs, liftArguments(args))
        // TODO also for "pure" and "toplevel" functions
        case b: core.BlockVar if b.id.builtin => App(transform(b), targs, liftArguments(args))

        // [[ Eff.op(arg) ]] = Eff(ev).op(arg)
        case core.Member(b, field) => App(Member(ScopeApp(transform(b), env.evidenceFor(b)), field), targs, liftArguments(args))
        case b => App(ScopeApp(transform(b), env.evidenceFor(b)), targs, liftArguments(args))
      }

    // TODO either the implementation of match should provide evidence
    // or we should not abstract over evidence!
    case core.Match(scrutinee, clauses) =>
      Match(transform(scrutinee), clauses.map { case (p, b) => (transform(p), transformBody(b)) })

    case core.If(cond, thn, els) =>
      If(transform(cond), transform(thn), transform(els))

    case core.While(cond, body) =>
      While(transform(cond), transform(body))

    case core.Ret(e) =>
      Ret(transform(e))

    case core.Include(contents, rest) => Include(contents, transform(rest))

    case core.Hole                    => Hole
  }

  def transform(tree: core.Expr)(using Environment, Context): Expr = tree match {
    case l: core.Literal[_] => transform(l)
    case core.ValueVar(sym)   => ValueVar(sym)
    case core.DirectApp(b: core.Block, targs, args: List[core.Argument]) =>
      PureApp(transform(b), targs, liftArguments(args))
    // TODO also distinguish between pure and io expressions in lift
    case core.PureApp(b: core.Block, targs, args: List[core.Expr]) =>
      PureApp(transform(b), targs, liftArguments(args))
    case core.Select(target, field) => Select(transform(target), field)
    case core.Box(b)                   => Closure(transform(b))
    case core.Run(s) =>
      Run(transform(s))
  }

  def transform[T](tree: core.Literal[T]): Literal[T] = tree match {
    case core.UnitLit() => UnitLit()
    case core.IntLit(value: Int) => IntLit(value)
    case core.BooleanLit(value: Boolean) => BooleanLit(value)
    case core.DoubleLit(value: Double) => DoubleLit(value)
    case core.StringLit(value: String) => StringLit(value)
  }

  def liftBlockLitTo(b: core.BlockLit, scope: ScopeId, self: Option[Symbol] = None)(using Environment, Context): BlockLit = b match {
    case core.BlockLit(params, body) =>
      val ownBindings = self.map { sym => env.bind(sym) }.getOrElse { env }

      // recursive functions need to bind the own id
      val extendedEnv = params.foldLeft(ownBindings.adapt(ScopeVar(scope))) {
        case (env, core.BlockParam(p, tpe)) => env.bind(p)
        case (env, core.ValueParam(p, tpe)) => env
      }
      BlockLit(params.map { p => transform(p) }, transform(body)(using extendedEnv, Context))
  }

  /**
   * Don't transform the block itself, but only the body. Used for local abstractions like match clauses where
   * we know the evidence is Here.
   */
  def transformBody(tree: core.BlockLit)(using Environment, Context): BlockLit = tree match {
    case core.BlockLit(params, body) =>
      BlockLit(params.map { p => transform(p) }, transform(body))
  }

  def transform(p: core.Pattern): Pattern = p match {
    case core.IgnorePattern() => IgnorePattern()
    case core.AnyPattern() => AnyPattern()
    case core.TagPattern(tag, patterns) => TagPattern(tag, patterns.map { p => transform(p) })
    case core.LiteralPattern(l) => LiteralPattern(transform(l))
  }

  // apply lifts to the arguments if it is block variables
  // this is the same as eta expanding them, since then the lifts would be composed for the call
  def liftArguments(args: List[core.Argument])(using Environment, Context): List[Argument] = args map {
    case b: core.BlockVar =>
      val transformed = BlockVar(b.id)
      env.evidenceFor(b) match {
        case Here() => transformed
        case ev     => Lifted(ev, transformed)
      }
    case b: core.Block => transform(b)
    case e: core.Expr  => transform(e)
  }

  def transform(h: core.Handler)(using Environment, Context): Handler = h match {
    case core.Handler(id, clauses) =>
      Handler(id, clauses.map {
        // effect operations should never take any evidence as they are guaranteed (by design) to be evaluated in
        // their definition context.
        case (op, core.BlockLit(params, body)) => (op, BlockLit(params.map { p => transform(p) }, transform(body)))
      })
  }

  case class Environment(env: Map[Symbol, List[Scope]]) {
    def bind(s: Symbol) = copy(env = env + (s -> Nil))
    def bind(s: Symbol, init: Scope) = copy(env = env + (s -> List(init)))
    def adapt(a: Scope) = copy(env = env.map { case (s, as) => s -> (a :: as) })

    def evidenceFor(b: core.Block)(using Context): Scope = b match {
      case b: core.BlockVar if Context.blockTypeOf(b.id) == builtins.TRegion => Here()
      case b: core.BlockVar => env.getOrElse(b.id, Nil) match {
        case Nil      => Here()
        case s :: Nil => s
        case scopes   => Nested(scopes)
      }
      case b: core.BlockLit   => Here()
      case core.Member(b, id) => evidenceFor(b)
      case b: core.Extern     => sys error "Cannot provide scope evidence for built in function"
      // TODO check whether this makes any sense
      case b: core.Unbox      => Here()
      case b: core.New => Here()
    }
  }
}
