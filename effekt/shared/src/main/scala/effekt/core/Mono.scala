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
            val constraints = findConstraints(definitions)
            println("Constraints")
            constraints.foreach(c => println(c))
            println()
            if (!hasCycle(constraints)) {
              val solvedConstraints = solveConstraints(constraints)
              println("Solved constraints")
              solvedConstraints.foreach(sc => println(sc))
            } else {
              println("Cycle detected, skipping solveConstraints")
            }
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
  case Base(val tpe: symbols.Symbol)
  case Var(val sym: symbols.Symbol)

  def toSymbol: symbols.Symbol = this match {
    case Base(tpe) => tpe 
    case Var(sym) => sym
  }
}

type PolyConstraints = Map[symbols.Symbol, Set[PolyType]]
type PolyConstraintEntry = (symbols.Symbol, Set[PolyType])

def solveConstraints(constraints: PolyConstraints): PolyConstraints =
  var solved: PolyConstraints = Map()

  def solveConstraint(sym: symbols.Symbol, types: Set[PolyType]): Set[PolyType] =
    var polyTypes: Set[PolyType] = Set()
    types.foreach(t => {
      t match {
        case PolyType.Var(symbol) => polyTypes ++= solved.getOrElse(symbol, solveConstraint(symbol, constraints.getOrElse(symbol, Set())))
        case base => polyTypes += base 
      }
    })
    solved += (sym -> polyTypes)
    polyTypes

  constraints.foreach(solveConstraint)

  solved

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

def hasCycle(constraints: PolyConstraints): Boolean =
    var visited: Set[symbols.Symbol] = Set()
    var recStack: Set[symbols.Symbol] = Set()

    def hasCycleHelper(vertex: symbols.Symbol): Boolean =
      if (recStack.contains(vertex)) return true
      if (visited.contains(vertex)) return false

      visited += vertex
      recStack += vertex

      var cycleFound = false
      constraints.getOrElse(vertex, Set()).foreach(v => cycleFound |= hasCycleHelper(v.toSymbol))

      recStack -= vertex

      cycleFound
      
    var cycleFound = false
    constraints.keys.foreach(v => cycleFound |= !visited.contains(v) && hasCycleHelper(v))

    cycleFound
