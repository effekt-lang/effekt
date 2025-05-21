package effekt
package core

import effekt.context.Context
import effekt.lexer.TokenKind
import effekt.context.assertions.asDataType

object Mono extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "mono"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = {
    input match {
      case CoreTransformed(source, tree, mod, core) => {
        core match {
          case ModuleDecl(path, includes, declarations, externs, definitions, exports) => {
            val polys = findConstraints(definitions)
            polys.foreach(p => println(p))
            println()
          }
        }
      }
    }
    Some(input)
  }
}

// TODO: After solving the constraints it would be helpful to know
//       which functions have which tparams
//       so we can generate the required monomorphic functions

enum PolyType {
  case Var(val sym: symbols.Symbol)
  case Base(val tpe: symbols.Symbol)
}

type PolyConstraints = Map[symbols.Symbol, Set[PolyType]]
type PolyConstraintEntry = (symbols.Symbol, Set[PolyType])

def appendConstraint(map: PolyConstraints, sym: symbols.Symbol, tpe: ValueType): PolyConstraintEntry =
  val currentFlow = map.getOrElse(sym, Set())
  tpe match {
    // Ignore self cycling types A -> A
    case ValueType.Data(name, targs) if name != sym => (sym -> (currentFlow + PolyType.Base(name)))
    case ValueType.Var(name) if name != sym => (sym -> (currentFlow + PolyType.Var(name)))
    // TODO: What do we do with boxed types?
    case o@ValueType.Boxed(tpe, capt) => 
      println("Hit boxed type: " + o)
      (sym -> currentFlow)
    case _ => (sym -> currentFlow) // self cycling flow
  }

def findConstraintRec(value: Val, typeFlow: PolyConstraints): PolyConstraints =
  var newTypeFlow = typeFlow
  value.binding match {
    case App(callee, targ :: targs, vargs, bargs) =>
      callee match {
        case BlockVar(id, annotatedTpe, annotatedCapt) =>
          annotatedTpe match {
            case BlockType.Function(tparam :: tparams, cparams, vparams, bparams, result) =>
              newTypeFlow += appendConstraint(newTypeFlow, tparam, targ)
            case _ =>
          }
        case _ =>
      }
    case _ =>
  }
  value.body match {
    case v@Val(_, _, _, _) => findConstraintRec(v, newTypeFlow)
    case _ => newTypeFlow
  }

def findConstraints(definitions: List[Toplevel]): PolyConstraints =
  var typeFlow: PolyConstraints = Map()
  definitions.foreach {
    case Toplevel.Def(id, block) =>
      block match
        case BlockLit(tparam :: tparams, cparams, vparams, bparams, body) =>
          body match {
            case v@Val(id, annotatedTpe, binding, body) => 
              typeFlow ++= findConstraintRec(v, typeFlow)
            case Return(expr) => 
              typeFlow += appendConstraint(typeFlow, tparam, expr.tpe)
            case _ =>
          }
        case BlockLit(tparams, cparams, vparams, bparams, body) =>
          body match {
            case v@Val(id, annotatedTpe, binding, body) => 
              typeFlow ++= findConstraintRec(v, typeFlow)
            case _ =>
          }
        case _ => 
    case _ =>
  }
  typeFlow

object PolyGraphCalc {
  var visited: Set[symbols.Symbol] = Set()
  var recStack: Set[symbols.Symbol] = Set()

  def hasCycle(vertex: symbols.Symbol, adjacency: PolyConstraints): Boolean =  
    if (recStack.contains(vertex)) return true

    if (visited.contains(vertex)) return false

    visited += vertex
    recStack += vertex

    adjacency.foreach((v, edges) => if (hasCycle(v, adjacency)) return true)

    recStack -= vertex
    false

  def hasCycle(constraints: PolyConstraints): Boolean =
    visited = Set()
    recStack = Set()

    constraints.keys.foreach(v => if (hasCycle(v, constraints)) return true)

    false
}

