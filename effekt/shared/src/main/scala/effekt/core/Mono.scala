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

              println()
              val monomorphized = monomorphize(core)(using MonoContext(solvedConstraints))
              println("Mono definitions")
              monomorphized.definitions.foreach(println)
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

type PolyConstraints = Map[Id, Set[PolyType]]
type PolyConstraintsSolved = Map[Id, Set[PolyType.Base]]
type PolyConstraintSingle = Map[Id, PolyType.Base]

class MonoContext(val solvedConstraints: PolyConstraintsSolved)

var monoCounter = 0
def freshMonoName(baseId: Id, tpe: PolyType.Base): Id =
  monoCounter += 1
  Id(baseId.name.name + tpe.tpe.name.name + monoCounter)

def freshMonoName(baseId: Id, tpes: List[PolyType.Base]): Id =
  monoCounter += 1
  var tpesString = ""
  tpes.foreach(tpe => tpesString += tpe.tpe.name.name)
  Id(baseId.name.name + tpesString + monoCounter)

// TODO: The following two are awful and surely doing redundant work.
def generator(xs: List[Set[PolyConstraintSingle]]): List[Set[PolyConstraintSingle]] = xs.foldRight(List(Set.empty)) { (next, combinations) =>
  (for (a <- next; as <- combinations) yield as + a).toList
}

def gen(xs: PolyConstraintsSolved) = {
  (for ((id, constrs) <- xs) yield (for (c <- constrs) yield Map((id -> c)))).toList
}

def monomorphize(decl: ModuleDecl)(using ctx: MonoContext): ModuleDecl = decl match
  case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
    ModuleDecl(path, includes, declarations, externs, definitions flatMap monomorphize, exports)

def monomorphize(definition: Toplevel)(using ctx: MonoContext): List[Toplevel] = definition match {
  case Toplevel.Def(id, block) => monomorphize(block, id).map((newId, newBlock) => Toplevel.Def(newId, newBlock)).toList
  case Toplevel.Val(id, tpe, binding) => ???
}

def monomorphize(block: Block, baseId: Id)(using ctx: MonoContext): Map[Id, Block] = block match {
  case BlockLit(List(), cparams, vparams, bparams, body) => Map(baseId -> block)
  case BlockLit(tparams, cparams, vparams, bparams, body) => {
    // TODO: There is some redundancy here, but it works for now
    val relevantConstraints = tparams.map(tp => Map(tp -> ctx.solvedConstraints.getOrElse(tp, Set.empty)).filter((_, s) => !s.isEmpty))
    val splitConstraints = relevantConstraints.flatMap(gen)
    val combinations = generator(splitConstraints)
    val flattened = combinations.map(c => c.flatten.toMap)
    flattened.map(f => {
      val newId = freshMonoName(baseId, f.values.toList)
      (newId -> BlockLit(List(), cparams, vparams.map(monomorphize(_, f)), bparams, monomorphize(body, f)))
    }).toMap
  }
  case BlockVar(id, annotatedTpe, annotatedCapt) => ???
  case New(impl) => ???
  case Unbox(pure) => ???
}

def monomorphize(stmt: Stmt, replacementTparam: Map[Id, PolyType.Base]): Stmt = stmt match {
  case Return(expr) => Return(monomorphize(expr, replacementTparam))
  case _ => ???
}

def monomorphize(pure: Pure, replacementTparam: Map[Id, PolyType.Base]): Pure = pure match
  case ValueVar(id, annotatedType) => ValueVar(id, monomorphize(annotatedType, replacementTparam))
  case _ => ???

def monomorphize(vparam: ValueParam, replacementTparam: Map[Id, PolyType.Base]): ValueParam = vparam match {
  case ValueParam(id, tpe) => ValueParam(id, monomorphize(tpe, replacementTparam))
}

def monomorphize(valueType: ValueType, replacementTparam: Map[Id, PolyType.Base]): ValueType = valueType match
  case ValueType.Var(name) => replacementTparam.get(name).get.toValueType
  case ValueType.Data(name, targs) => ???
  case ValueType.Boxed(tpe, capt) => ???

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

  def toValueType: ValueType = this match {
    case Base(tpe) => ValueType.Data(tpe, List.empty)
    case Var(sym) => ValueType.Var(sym)
  }
}

def solveConstraints(constraints: PolyConstraints): PolyConstraintsSolved =
  var solved: PolyConstraintsSolved = Map()

  def solveConstraint(sym: Id, types: Set[PolyType]): Set[PolyType.Base] =
    var polyTypes: Set[PolyType.Base] = Set()
    types.foreach {
      case PolyType.Var(symbol) => polyTypes ++= solved.getOrElse(symbol, solveConstraint(symbol, constraints.getOrElse(symbol, Set())))
      case PolyType.Base(tpe) => polyTypes += PolyType.Base(tpe)
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
