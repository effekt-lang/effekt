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
            // Find constraints in the definitions
            val constraints = findConstraints(definitions)
            println("Constraints")
            constraints.foreach(c => println(c))
            println()

            // Filter out self referencing constraints
            val filtered = constraints.map((id, tpes) => (id, tpes.filter(tpe => tpe.toSymbol != id)))
            println("Filtered")
            filtered.foreach(f => println(f))
            println()

            // Solve if constraints are non-cyclic
            if (!hasCycle(filtered)) {
              val solvedConstraints = solveConstraints(filtered)
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
  case Base(val tpe: Id)
  case Var(val sym: Id)

  def toSymbol: Id = this match {
    case Base(tpe) => tpe 
    case Var(sym) => sym
  }
}

type PolyConstraints = Map[Id, Set[PolyType]]
type PolyConstraintEntry = (Id, Set[PolyType])

def solveConstraints(constraints: PolyConstraints): PolyConstraints =
  var solved: PolyConstraints = Map()

  def solveConstraint(sym: Id, types: Set[PolyType]): Set[PolyType] =
    var polyTypes: Set[PolyType] = Set()
    types.foreach {
      case PolyType.Var(symbol) => polyTypes ++= solved.getOrElse(symbol, solveConstraint(symbol, constraints.getOrElse(symbol, Set())))
      case base => polyTypes += base
    }
    solved += (sym -> polyTypes)
    polyTypes

  constraints.foreach(solveConstraint)

  solved

def combineConstraints(a: PolyConstraints, b: PolyConstraints): PolyConstraints = {
  a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Iterable.empty)) }
}

def findConstraints(definitions: List[Toplevel]): PolyConstraints =
  definitions.map(findConstraints).reduce(combineConstraints)

def findConstraints(toplevel: Toplevel): PolyConstraints = toplevel match {
  case Toplevel.Def(id, block) => findConstraints(block, List.empty)
  case Toplevel.Val(id, tpe, binding) => ???
}

def findConstraints(block: Block, targs: List[ValueType]): PolyConstraints = block match {
  case BlockLit(tparam :: tparams, cparams, vparams, bparams, body) => findConstraints(body, tparam :: tparams)
  case BlockLit(List(), cparams, vparams, bparams, body) => findConstraints(body, List.empty)
  case BlockVar(id, annotatedTpe, annotatedCapt) => findConstraints(annotatedTpe, targs)
  case New(impl) => ???
  case Unbox(pure) => ???
  case _ => Map.empty
}

def findConstraints(stmt: Stmt, tparams: List[Id]): PolyConstraints = stmt match {
  case App(callee, targs, vargs, bargs) => findConstraints(callee, targs)
  case Return(expr) if !tparams.isEmpty => Map(tparams.head -> Set(findPolyType(expr.tpe)))
  case Return(expr) => Map.empty
  case Val(id, annotatedTpe, binding, body) => combineConstraints(findConstraints(binding, tparams), findConstraints(body, tparams))
  // TODO: Let & If case is wrong, but placeholders are required as they are used in print
  case Let(id, annotatedTpe, binding, body) => Map.empty
  case If(cond, thn, els) => Map.empty
  case o => println(o); ???
}

def findConstraints(value: Val): PolyConstraints = value match {
  // TODO: List.empty might be wrong
  case Val(id, annotatedTpe, binding, body) => combineConstraints(findConstraints(binding, List.empty), findConstraints(body, List.empty))
}

def findConstraints(blockType: BlockType, targs: List[ValueType]): PolyConstraints = blockType match {
  case BlockType.Function(tparams, cparams, vparams, bparams, result) => tparams.zip(targs).map((id, tpe) => (id -> Set(findPolyType(tpe)))).toMap
  case BlockType.Interface(name, targs) => ???
}

def findPolyType(blockType: BlockType, targs: List[ValueType]): List[PolyType] = blockType match {
  case BlockType.Function(tparams, cparams, vparams, bparams, result) => ???
  case BlockType.Interface(name, targs) => ???
}

def findPolyType(valueType: ValueType): PolyType = valueType match {
  case ValueType.Boxed(tpe, capt) => ???
  case ValueType.Data(name, targs) => PolyType.Base(name)
  case ValueType.Var(name) => PolyType.Var(name)
}

def hasCycle(constraints: PolyConstraints): Boolean =
    var visited: Set[Id] = Set()
    var recStack: Set[Id] = Set()

    def hasCycleHelper(vertex: Id): Boolean =
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
