package effekt
package core

import effekt.context.Context
import effekt.lexer.TokenKind
import effekt.context.assertions.asDataType

// import scala.collection.mutable

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

// TODO: We probably want some kind of graph data structure instead of the map, for better performance in cycle detection later.
//       This works for now
// TODO: Consider using unique IDs instead of Symbol for keys.
//       This works, but might give weird output when debugging
//       if the same symbol name is used twice
// TODO: After solving the constraints it would be helpful to know
//       which functions have which tparams
//       so we can generate the required monomorphic functions
type PolyConstraint = Map[symbols.Symbol, Set[symbols.Symbol]]
type PolyConstraintEntry = (symbols.Symbol, Set[symbols.Symbol])

def appendConstraint(map: PolyConstraint, sym: symbols.Symbol, tpe: ValueType): PolyConstraintEntry =
  val currentFlow = map.getOrElse(sym, Set())
  tpe match {
    // Ignore self cycling types A -> A
    case ValueType.Data(name, targs) if name != sym => (sym -> currentFlow.incl(name))
    case ValueType.Var(name) if name != sym => (sym -> (currentFlow.incl(name))) 
    // TODO: What do we do with boxed types?
    case o@ValueType.Boxed(tpe, capt) => 
      println("Hit boxed type: " + o)
      (sym -> currentFlow)
    case _ => (sym -> currentFlow) // self cycling flow
  }

def findConstraintRec(value: Val, typeFlow: PolyConstraint): PolyConstraint =
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

def findConstraints(definitions: List[Toplevel]): PolyConstraint =
  var typeFlow: PolyConstraint = Map()
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