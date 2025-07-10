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
            val constraints = findConstraints(definitions)(using new MonoContext)
            println("Constraints")
            constraints.foreach(c => println(c))
            println()

            // val solved = solveConstraint(constraints)
            // println("Solved")
            // solved.foreach(println)
            println()

            
          }
        }
      }
    }
    Some(input)
  }
}

type FunctionId = Id
case class Constraint(lower: Vector[TypeArg], upper: FunctionId)
type Constraints = List[Constraint]

// case class SolvedConstraint(lower: Vector[TypeArg.Base], upper: FunctionId | TypeArg.Var)
// type SolvedConstraints = List[SolvedConstraint]

enum TypeArg {
  case Base(val tpe: Id)
  case Var(funId: FunctionId, pos: Int)
}

// Type Id -> Var
type TypeParams = Map[Id, TypeArg.Var]

class MonoContext {
  var typingContext: TypeParams = Map()

  def extendTypingContext(tparam: Id, index: Int, functionId: FunctionId) =
    typingContext += (tparam -> TypeArg.Var(functionId, index))
}

def findConstraints(definitions: List[Toplevel])(using MonoContext): Constraints =
  definitions flatMap findConstraints

def findConstraints(definition: Toplevel)(using ctx: MonoContext): Constraints = definition match
  case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    findConstraints(body)
  case Toplevel.Def(id, block) => ???
  case Toplevel.Val(id, tpe, binding) => ???

def findConstraints(block: Block)(using ctx: MonoContext): Constraints = block match
  case BlockVar(id, annotatedTpe, annotatedCapt) => ???
  case BlockLit(tparams, cparams, vparams, bparams, body) => ???
  case Unbox(pure) => ???
  case New(impl) => ???

def findConstraints(stmt: Stmt)(using ctx: MonoContext): Constraints = stmt match
  case Let(id, annotatedTpe, binding, body) => findConstraints(binding) ++ findConstraints(body)
  case Return(expr) => findConstraints(expr)
  case Val(id, annotatedTpe, binding, body) => findConstraints(binding) ++ findConstraints(body)
  case App(callee: BlockVar, targs, vargs, bargs) => List(Constraint(targs.map(findId).toVector, callee.id))
  case If(cond, thn, els) => findConstraints(cond) ++ findConstraints(thn) ++ findConstraints(els)
  case o => println(o); ???

def findConstraints(expr: Expr)(using ctx: MonoContext): Constraints = expr match
  case DirectApp(b, List(), vargs, bargs) => List.empty
  case ValueVar(id, annotatedType) => List.empty
  case Literal(value, annotatedType) => List.empty
  case o => println(o); ???

def findId(vt: ValueType)(using ctx: MonoContext): TypeArg = vt match
  case ValueType.Boxed(tpe, capt) => ???
  case ValueType.Data(name, targs) => TypeArg.Base(name)
  case ValueType.Var(name) => ctx.typingContext(name)


// Old stuff

// type PolyConstraints = Map[Id, Set[PolyType]]
// type PolyConstraintsSolved = Map[Id, Set[PolyType.Base]]
// type PolyConstraintSingle = Map[Id, PolyType.Base]

// class MonoContext(val solvedConstraints: PolyConstraintsSolved, var monoDefs: Map[Id, Map[List[PolyType.Base], (Id, Block)]] = Map.empty)

// var monoCounter = 0
// def freshMonoName(baseId: Id, tpe: PolyType.Base): Id =
//   monoCounter += 1
//   Id(baseId.name.name + tpe.tpe.name.name + monoCounter)

// def freshMonoName(baseId: Id, tpes: List[PolyType.Base]): Id =
//   monoCounter += 1
//   var tpesString = ""
//   tpes.foreach(tpe => tpesString += tpe.tpe.name.name)
//   Id(baseId.name.name + tpesString + monoCounter)

// // TODO: The following two are awful and surely doing redundant work.
// def generator(xs: List[Set[PolyConstraintSingle]]): List[Set[PolyConstraintSingle]] = xs.foldRight(List(Set.empty)) { (next, combinations) =>
//   (for (a <- next; as <- combinations) yield as + a).toList
// }

// def gen(xs: PolyConstraintsSolved) = {
//   (for ((id, constrs) <- xs) yield (for (c <- constrs) yield Map((id -> c)))).toList
// }

// def monoVparams(vparams: List[ValueType]): List[PolyType.Base] = vparams map monoVparam

// def monoVparams(vparams: List[ValueParam]): List[PolyType.Base] = vparams.map(vp => monoVparam(vp.tpe))

// def monoVparam(valueType: ValueType): PolyType.Base = valueType match {
//   case ValueType.Boxed(tpe, capt) => ???
//   case ValueType.Data(name, targs) => PolyType.Base(name)
//   case ValueType.Var(name) => ???
// }

// def monomorphize(decl: ModuleDecl)(using ctx: MonoContext): ModuleDecl = decl match
//   case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
//     ModuleDecl(path, includes, declarations, externs, definitions flatMap monomorphize, exports)

// def monomorphize(definition: Toplevel)(using ctx: MonoContext): List[Toplevel] = definition match {
//   case Toplevel.Def(id, block) => monomorphize(block, id).map((newId, newBlock) => Toplevel.Def(newId, newBlock)).toList
//   case Toplevel.Val(id, tpe, binding) => ???
// }

// def monomorphize(block: Block, baseId: Id)(using ctx: MonoContext): List[(Id, Block)] = block match {
//   case BlockLit(List(), cparams, vparams, bparams, body) => {
//     val monoBody = monomorphize(body, Map.empty)
//     val monoBlock = BlockLit(List.empty, cparams, vparams, bparams, monoBody)

//     List((baseId, monoBlock))
//   }
//   case BlockLit(tparams, cparams, vparams, bparams, body) => {
//     // TODO: There is some redundancy here, but it works for now
//     val relevantConstraints = tparams.map(tp => Map(tp -> ctx.solvedConstraints.getOrElse(tp, Set.empty)).filter((_, s) => !s.isEmpty))
//     val splitConstraints = relevantConstraints.flatMap(gen)
//     val combinations = generator(splitConstraints)
//     val flattened = combinations.map(c => c.flatten.toMap)
//     val res = flattened.map(f => {
//       ctx.monoDefs.getOrElse(baseId, {
//         // TODO: Not really happy with this
//         val baseTypes = monoVparams(vparams)
        
//         val newId = freshMonoName(baseId, f.values.toList)
//         val newVparams = vparams.map(monomorphize(_, f))
//         val newBlock = BlockLit(List(), cparams, newVparams, bparams, monomorphize(body, f))
//         ctx.monoDefs += (baseId -> Map(baseTypes -> (newId, newBlock)))
//         (newId, newBlock)
//       })
//     })
//     // res
//     ???
//   }
//   case BlockVar(id, annotatedTpe, annotatedCapt) => ???
//   case New(impl) => ???
//   case Unbox(pure) => ???
// }

// def monomorphize(stmt: Stmt, replacementTparam: Map[Id, PolyType.Base])(using ctx: MonoContext): Stmt = stmt match {
//   case Return(expr) => Return(monomorphize(expr, replacementTparam))
//   case Val(id, annotatedTpe, binding, body) => Val(id, annotatedTpe, monomorphize(binding, replacementTparam), monomorphize(body, replacementTparam))
//   case App(callee, List(), vargs, bargs) => App(callee, List(), vargs, bargs)
//   case App(callee, targs, vargs, bargs) => {
//     // TODO: This does not seem correct, but I need the base id of the BlockVar to monomorphize
//     //       Not sure if doing everything in one go is possible to do correctly
//     callee match {
//       case BlockVar(id, annotatedTpe, annotatedCapt) => {
//         // TODO: What if the block has not been generated yet?
//         //       We don't use names of blocks but pass entire blocks
//         val baseTypes = targs map monoVparam
//         val genCallee = ctx.monoDefs.getOrElse((id, baseTypes), ???)
//         val f = App(genCallee._2, List(), vargs, bargs)
//         println(f)
//         f
//       }
//       case _ => ???
//     }
//   }
//   case Let(id, annotatedTpe, binding, body) => Let(id, annotatedTpe, monomorphize(binding, replacementTparam), monomorphize(body, replacementTparam))
//   case If(cond, thn, els) => If(monomorphize(cond, replacementTparam), monomorphize(thn, replacementTparam), monomorphize(els, replacementTparam))
//   case o => println(o); ???
// }

// def monomorphize(expr: Expr, replacementTparam: Map[Id, PolyType.Base]): Expr = expr match {
//   case DirectApp(b, List(), vargs, bargs) => DirectApp(b, List(), vargs, bargs)
//   case o => println(o); ???
// }

// def monomorphize(pure: Pure, replacementTparam: Map[Id, PolyType.Base]): Pure = pure match
//   case ValueVar(id, annotatedType) => ValueVar(id, monomorphize(annotatedType, replacementTparam))
//   case Literal(value, annotatedType) => pure
//   case o => println(o); ???

// def monomorphize(vparam: ValueParam, replacementTparam: Map[Id, PolyType.Base]): ValueParam = vparam match {
//   case ValueParam(id, tpe) => ValueParam(id, monomorphize(tpe, replacementTparam))
// }

// def monomorphize(valueType: ValueType, replacementTparam: Map[Id, PolyType.Base]): ValueType = valueType match
//   case ValueType.Var(name) => replacementTparam.get(name).get.toValueType
//   case ValueType.Data(name, targs) => valueType
//   case ValueType.Boxed(tpe, capt) => ???

// TODO: After solving the constraints it would be helpful to know
//       which functions have which tparams
//       so we can generate the required monomorphic functions

// enum PolyType {
//   case Base(val tpe: Id)
//   case Var(val sym: Id)

//   def toSymbol: Id = this match {
//     case Base(tpe) => tpe 
//     case Var(sym) => sym
//   }

//   def toValueType: ValueType = this match {
//     case Base(tpe) => ValueType.Data(tpe, List.empty)
//     case Var(sym) => ValueType.Var(sym)
//   }
// }

// def solveConstraints(constraints: PolyConstraints): PolyConstraintsSolved =
//   var solved: PolyConstraintsSolved = Map()

//   def solveConstraint(sym: Id, types: Set[PolyType]): Set[PolyType.Base] =
//     var polyTypes: Set[PolyType.Base] = Set()
//     types.foreach {
//       case PolyType.Var(symbol) => polyTypes ++= solved.getOrElse(symbol, solveConstraint(symbol, constraints.getOrElse(symbol, Set())))
//       case PolyType.Base(tpe) => polyTypes += PolyType.Base(tpe)
//     }
//     solved += (sym -> polyTypes)
//     polyTypes

//   constraints.foreach(solveConstraint)

//   solved

// def combineConstraints(a: PolyConstraints, b: PolyConstraints): PolyConstraints = {
//   a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Iterable.empty)) }
// }

// def findConstraints(definitions: List[Toplevel]): PolyConstraints =
//   definitions.map(findConstraints).reduce(combineConstraints)

// def findConstraints(toplevel: Toplevel): PolyConstraints = toplevel match {
//   case Toplevel.Def(id, block) => findConstraints(block, List.empty)
//   case Toplevel.Val(id, tpe, binding) => ???
// }

// def findConstraints(block: Block, targs: List[ValueType]): PolyConstraints = block match {
//   case BlockLit(tparam :: tparams, cparams, vparams, bparams, body) => findConstraints(body, tparam :: tparams)
//   case BlockLit(List(), cparams, vparams, bparams, body) => findConstraints(body, List.empty)
//   case BlockVar(id, annotatedTpe, annotatedCapt) => findConstraints(annotatedTpe, targs)
//   case New(impl) => ???
//   case Unbox(pure) => ???
//   case _ => Map.empty
// }

// def findConstraints(stmt: Stmt, tparams: List[Id]): PolyConstraints = stmt match {
//   case App(callee, targs, vargs, bargs) => findConstraints(callee, targs)
//   case Return(expr) if !tparams.isEmpty => Map(tparams.head -> Set(findPolyType(expr.tpe)))
//   case Return(expr) => Map.empty
//   case Val(id, annotatedTpe, binding, body) => combineConstraints(findConstraints(binding, tparams), findConstraints(body, tparams))
//   // TODO: Let & If case is wrong, but placeholders are required as they are used in print
//   case Let(id, annotatedTpe, binding, body) => Map.empty
//   case If(cond, thn, els) => Map.empty
//   case o => println(o); ???
// }

// def findConstraints(value: Val): PolyConstraints = value match {
//   // TODO: List.empty might be wrong
//   case Val(id, annotatedTpe, binding, body) => combineConstraints(findConstraints(binding, List.empty), findConstraints(body, List.empty))
// }

// def findConstraints(blockType: BlockType, targs: List[ValueType]): PolyConstraints = blockType match {
//   case BlockType.Function(tparams, cparams, vparams, bparams, result) => tparams.zip(targs).map((id, tpe) => (id -> Set(findPolyType(tpe)))).toMap
//   case BlockType.Interface(name, targs) => ???
// }

// def findPolyType(blockType: BlockType, targs: List[ValueType]): List[PolyType] = blockType match {
//   case BlockType.Function(tparams, cparams, vparams, bparams, result) => ???
//   case BlockType.Interface(name, targs) => ???
// }

// def findPolyType(valueType: ValueType): PolyType = valueType match {
//   case ValueType.Boxed(tpe, capt) => ???
//   case ValueType.Data(name, targs) => PolyType.Base(name)
//   case ValueType.Var(name) => PolyType.Var(name)
// }

// def hasCycle(constraints: PolyConstraints): Boolean =
//     var visited: Set[Id] = Set()
//     var recStack: Set[Id] = Set()

//     def hasCycleHelper(vertex: Id): Boolean =
//       if (recStack.contains(vertex)) return true
//       if (visited.contains(vertex)) return false

//       visited += vertex
//       recStack += vertex

//       var cycleFound = false
//       constraints.getOrElse(vertex, Set()).foreach(v => cycleFound |= hasCycleHelper(v.toSymbol))

//       recStack -= vertex

//       cycleFound
      
//     var cycleFound = false
//     constraints.keys.foreach(v => cycleFound |= !visited.contains(v) && hasCycleHelper(v))

//     cycleFound
