package effekt
package desugarer

/**
 *
 */
import effekt.context.Context
import effekt.context.assertions.{ SymbolAssertions, TypeAssertions }
import effekt.source._
import effekt.symbols._
import effekt.util.Task
import scopes._
import org.bitbucket.inkytonik.kiama.util.Source

/**
 * The output of this phase:
 *
 */
case class DesugarerState()

class Desugarer extends Phase[Module, Module] { namer =>

  def run(mod: Module)(implicit C: Context): Option[Module] = {
    Some(desugar(mod))
  }

  def desugar(mod: Module)(implicit C: Context): Module = {
    val module = mod.decl

    val newDefs = Context in {
      module.defs.map { d =>
        Context.focusing(d) { d =>
          desugar(d)
        }
      }
    }

    mod.clone(ModuleDecl(module.path, module.imports, newDefs), mod.source)
  }

  def desugar(defi: Def)(implicit C: Context): Def = defi match {
    case fun @ FunDef(id, tparams, params, ret, body) => {
      val newBody = desugar(body)
      FunDef(id, tparams, params, ret, newBody)
    }
    case d => d
  }

  def desugar(stmt: Stmt)(implicit C: Context): Stmt = stmt match {
    case DefStmt(d, rest) => DefStmt(desugar(d), desugar(rest))
    case ExprStmt(e, rest) => {
      // ここでeにresume(x)を含むなら、
      // DefStmt(ValDef("temp", None, x),
      // ExprStmt(<e内のresume(x)をresume{temp}に置き換えたExpr>,
      // rest))
      // を返す
      ExprStmt(desugar(e), desugar(rest))
    }
    case Return(e)    => Return(desugar(e))
    case BlockStmt(b) => BlockStmt(desugar(b))
  }

  def desugar(expr: Expr)(implicit C: Context): Expr = expr match {
    case v: Var             => v
    case Assign(id, expr)   => Assign(id, desugar(expr))
    case l: Literal[t]      => l
    case If(cond, thn, els) => If(desugar(cond), desugar(thn), desugar(els))
    case While(cond, body)  => While(desugar(cond), desugar(body))
    case MatchExpr(sc, clauses) =>
      val newClauses = clauses.map {
        case MatchClause(pattern, body) =>
          MatchClause(pattern, desugar(body))
      }
      MatchExpr(desugar(sc), newClauses)
    case Call(fun, targs, args) =>
      val newArgs = if (fun.name == "resume") {
        val newArg0 = args(0) match {
          // The number of arguments of resume must be 1.
          case ValueArgs(List(arg)) => {
            // TODO: Move arg to outer expr
            println("replced")
            BlockArg(ValueParams(Nil), Return(desugar(arg)))
          }
          case BlockArg(params, body) => BlockArg(params, desugar(body))
        }
        List(newArg0)
      } else {
        args.map {
          case ValueArgs(args)        => ValueArgs(args map desugar)
          case BlockArg(params, body) => BlockArg(params, desugar(body))
        }
      }
      Call(fun, targs, newArgs)
    case TryHandle(prog, handlers) =>
      val newHandlers = handlers.map {
        case Handler(id, clauses) =>
          val newClauses = clauses.map {
            case OpClause(id, params, body, resume) =>
              OpClause(id, params, desugar(body), resume)
          }
          Handler(id, newClauses)
      }
      TryHandle(desugar(prog), newHandlers)
    case Hole(stmts) => Hole(desugar(stmts))
  }
}
