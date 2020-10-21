package effekt
package core

import effekt.symbols.Symbol
import effekt.context.Context

class LiftInference extends Phase[ToplevelDecl, ToplevelDecl] {

  def run(mod: ToplevelDecl)(implicit C: Context): Option[ToplevelDecl] =
    Some(transform(mod)(Environment(Map.empty), C))

  // TODO either resolve and bind imports or use the knowledge that they are toplevel!
  def transform(mod: ToplevelDecl)(implicit env: Environment, C: Context): ToplevelDecl =
    mod.copy(defs = transform(mod.defs))

  // [[ a -> b ]] = [ev] -> a -> b
  def transform(tree: Block, self: Option[Symbol] = None)(implicit env: Environment, C: Context): Block = tree match {
    case BlockLit(params, body) =>
      val id = ScopeId()

      val ownBindings = self.map { sym => env.bind(sym) }.getOrElse { env }

      // recursive functions need to bind the own id
      val extendedEnv = params.foldLeft(ownBindings.adapt(ScopeVar(id))) {
        case (env, BlockParam(p)) => env.bind(p)
        case (env, ValueParam(p)) => env
      }
      ScopeAbs(id, BlockLit(params, transform(body)(extendedEnv, C)))
    case Member(body, id) => ??? // Member(transform(body), id)
    case e                => e
  }

  def transform(tree: Stmt)(implicit env: Environment, C: Context): Stmt = tree match {
    case Def(id, block, rest) =>
      Def(id, transform(block, Some(id)), transform(rest)(env.bind(id), C))

    case Val(id, binding, body) =>
      Val(id, transform(binding), transform(body))

    case State(id, get, put, init, body) =>
      State(id, get, put, transform(init), transform(body))

    case Data(id, ctors, rest) =>
      Data(id, ctors, transform(rest))

    case Record(id, fields, rest) =>
      Record(id, fields, transform(rest))

    case Handle(body, handler) =>
      val transformedBody = transform(body) // lift is provided by the handler runtime
      val transformedHandler = handler.map { transform }
      Handle(transformedBody, transformedHandler)

    case App(b: Block, args: List[Argument]) => b match {
      case b: Extern => App(b, liftArguments(args))
      // TODO also for "pure" and "toplevel" functions
      case b: BlockVar if b.id.builtin => App(b, liftArguments(args))

      // [[ Eff.op(arg) ]] = Eff(ev).op(arg)
      case Member(b, field) => App(Member(ScopeApp(transform(b), env.evidenceFor(b)), field), liftArguments(args))
      case b => App(ScopeApp(transform(b), env.evidenceFor(b)), liftArguments(args))
    }

    // TODO either the implementation of match should provide evidence
    // or we should not abstract over evidence!
    case Match(scrutinee, clauses) =>
      Match(scrutinee, clauses.map { case (p, b) => (p, transformBody(b)) })

    case If(cond, thn, els) =>
      If(cond, transform(thn), transform(els))

    case While(cond, body) =>
      While(transform(cond), transform(body))

    case Ret(e) =>
      Ret(e)

    case Exports(path, exports) =>
      Exports(path, exports)

    case Include(contents, rest) => Include(contents, transform(rest))

    case Hole                    => Hole
  }

  /**
   * Don't transform the block itself, but only the body. Used for local abstractions like match clauses where
   * we know the evidence is Here.
   */
  def transformBody(tree: BlockLit)(implicit env: Environment, C: Context): BlockLit = tree match {
    case BlockLit(params, body) =>
      BlockLit(params, transform(body))
  }

  // apply lifts to the arguments if it is block variables
  // this is the same as eta expanding them, since then the lifts would be composed for the call
  def liftArguments(args: List[Argument])(implicit env: Environment, C: Context): List[Argument] = args map {
    case b: BlockVar => env.evidenceFor(b) match {
      case Here() => b
      case ev     => Lifted(ev, b)
    }
    case b: Block => transform(b)
    case other    => other
  }

  def transform(h: Handler)(implicit env: Environment, C: Context): Handler = h match {
    case Handler(id, clauses) =>
      Handler(id, clauses.map {
        // effect operations should never take any evidence as they are guaranteed (by design) to be evaluated in
        // their definition context.
        case (op, BlockLit(params, body)) => (op, BlockLit(params, transform(body)))
      })
  }

  case class Environment(env: Map[Symbol, List[Scope]]) {
    def bind(s: Symbol) = copy(env = env + (s -> Nil))
    def bind(s: Symbol, init: Scope) = copy(env = env + (s -> List(init)))
    def adapt(a: Scope) = copy(env = env.map { case (s, as) => s -> (a :: as) })

    def evidenceFor(b: Block): Scope = b match {
      case b: BlockVar => env.getOrElse(b.id, Nil) match {
        case Nil      => Here()
        case s :: Nil => s
        case scopes   => Nested(scopes)
      }
      case b: BlockLit   => Here()
      case Member(b, id) => evidenceFor(b)
      case b: Extern     => sys error "Cannot provide scope evidence for built in function"
      case b: Lifted     => sys error "Should not happen"
      case b: ScopeApp   => sys error "Should not happen"
      case b: ScopeAbs   => sys error "Should not happen"
    }
  }
}
