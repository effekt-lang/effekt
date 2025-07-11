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

            val solved = solveConstraints(constraints)
            println("Solved")
            solved.foreach(println)
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

type Solution = Map[FunctionId, Set[Vector[TypeArg.Base]]]

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

def solveConstraints(constraints: Constraints): Solution =
  var solved: Solution = Map()

  val groupedConstraints = constraints.groupBy(c => c.upper)
  val vecConstraints = groupedConstraints.map((sym, constraints) => (sym -> constraints.map(c => c.lower)))

  vecConstraints.foreach((sym, tas) => 
    val sol = solveConstraints(sym).map(bs => bs.toVector)
    solved += (sym -> sol)
  )

  def solveConstraints(funId: FunctionId): Set[List[TypeArg.Base]] =
    val filteredConstraints = vecConstraints(funId)
    var nbs: Set[List[TypeArg.Base]] = Set.empty
    filteredConstraints.foreach(b => 
      var l: List[List[TypeArg.Base]] = List(List.empty)
      def listFromIndex(ind: Int) = if (ind >= l.length) List.empty else l(ind)
      b.foreach({
        case TypeArg.Base(tpe) => l = productAppend(l, List(TypeArg.Base(tpe)))
        case TypeArg.Var(funId, pos) => 
          val funSolved = solved.getOrElse(funId, solveConstraints(funId))
          val posArgs = funSolved.map(v => v(pos))
          l = posArgs.zipWithIndex.map((base, ind) => listFromIndex(ind) :+ base).toList
          println(l)
      })
      nbs ++= l
    )
    nbs

  solved

def productAppend[A](ls: List[List[A]], rs: List[A]): List[List[A]] =
  rs.flatMap(r => ls.map(l => l :+ r))

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
