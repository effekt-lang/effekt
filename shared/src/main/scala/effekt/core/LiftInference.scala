package effekt
package core

import effekt.symbols.Symbol
import effekt.context.Context

class LiftInference extends Phase[ModuleDecl, ModuleDecl] {

  def run(mod: ModuleDecl)(implicit C: Context): Option[ModuleDecl] =
    Some(transform(mod)(Environment(Map.empty)))

  // TODO either resolve and bind imports or use the knowledge that they are toplevel!
  def transform(mod: ModuleDecl)(implicit env: Environment): ModuleDecl =
    mod.copy(defs = transform(mod.defs))

  def transform(tree: Block)(implicit env: Environment): Block = tree match {
    case BlockDef(params, body) =>
      val id = AdapterParam()
      val extendedEnv = params.foldLeft(env.adapt(AdaptVar(id))) {
        case (env, BlockParam(p)) => env.bind(p)
        case (env, ValueParam(p)) => env
      }
      AdapterDef(id, BlockDef(params, transform(body)(extendedEnv)))
    case Member(body, id) =>
      Member(transform(body), id)
    case e => e
  }

  def transform(tree: Stmt)(implicit env: Environment): Stmt = tree match {
    case Def(id, block, rest) =>
      Def(id, transform(block), transform(rest)(env.bind(id)))

    case Val(id, binding, body) =>
      Val(id, transform(binding), transform(body))

    case Var(id, binding, body) =>
      Var(id, transform(binding), transform(body))

    case Data(id, ctors, rest) =>
      Data(id, ctors, transform(rest))

    case Record(id, fields, rest) =>
      Record(id, fields, transform(rest))

    case Handle(body, handler) =>
      val transformedBody = AdapterApp(transform(body), List(Lift()))
      val transformedHandler = handler.map { transform }
      Handle(transformedBody, transformedHandler)

    case App(b: Block, args: List[Argument]) => App(env.supply(transform(b)), args)

    case Match(scrutinee, clauses) =>
      Match(scrutinee, clauses.map { case (p, b) => (p, env.supply(transform(b))) })

    case If(cond, thn, els) =>
      If(cond, transform(thn), transform(els))
    case While(cond, body) =>
      While(transform(cond), transform(body))
    case Ret(e) =>
      Ret(e)
    case Exports(path, exports) =>
      Exports(path, exports)

    case i: Include => i
    case Hole       => Hole
  }

  def transform(h: Handler)(implicit env: Environment): Handler = h match {
    case Handler(id, clauses) =>
      Handler(id, clauses.map { case (op, impl) => (op, transform(impl)) })
  }

  case class Environment(env: Map[Symbol, List[Adapter]]) {
    def bind(s: Symbol) = copy(env = env + (s -> Nil))
    def adapt(a: Adapter) = copy(env = env.map { case (s, as) => s -> (a :: as) })
    def supply(b: Block): Block = b match {
      case b: BlockVar       => AdapterApp(b, env.getOrElse(b.id, sys error s"${b} is not in scope"))
      // this is an abstraction followed by an application and can be statically reduced
      case AdapterDef(id, b) => b
      case b: BlockDef       => b
      case b: Extern         => b
      case Member(b, id)     => Member(supply(b), id)
      case b: AdapterApp     => sys error "should not happen"
    }

  }
}

//
// f...               f = f
// handle {           f = lift(f)
//   def g[ev7] {     f = lift(f)
//     handle {       f = lift(lift(f))
//       f
//     }
//   }
//
// }

//
// f...               f @ List(), ...
// handle {           f @ List(Lift), ...
//   def g[ev7] {     f @ List(ev7, Lift), ...
//     handle {       f @ List(Lift, ev7, Lift), ...
//       f[compose(Lift, ev7, Lift)]()
//     }
//   }
//
// }

// f...
// handle {       f @ Lift
//   map { ev3 => f @ ev3, Lift
//     handle {   f @ Lift, ev3, Lift
//       f[Lift, ev3, Lift]()
//     }
//   }
// }
