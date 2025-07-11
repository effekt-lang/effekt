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
            val constraints = findConstraints(definitions)(using new MonoFindContext)
            // println("Constraints")
            // constraints.foreach(c => println(c))
            // println()

            // Solve collected constraints
            val solution = solveConstraints(constraints)
            // println("Solved")
            // solution.foreach(println)
            // println()

            // Monomorphize existing definitions
            var monoNames: MonoNames = Map.empty
            solution.foreach((funId, targs) => 
              targs.foreach(vb => 
                monoNames += ((funId, vb) -> freshMonoName(funId, vb))
              )
            )

            val monoDefs = monomorphize(definitions)(using MonoContext(solution, monoNames))
            // monoDefs.foreach(defn => println(util.show(defn)))
            val newModuleDecl = ModuleDecl(path, includes, declarations, externs, monoDefs, exports)
            return Some(CoreTransformed(source, tree, mod, newModuleDecl))
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
type MonoNames = Map[(FunctionId, Vector[TypeArg.Base]), FunctionId]

enum TypeArg {
  case Base(val tpe: Id)
  case Var(funId: FunctionId, pos: Int)
}

// Type Id -> Var
type TypeParams = Map[Id, TypeArg.Var]

class MonoFindContext {
  var typingContext: TypeParams = Map()

  def extendTypingContext(tparam: Id, index: Int, functionId: FunctionId) =
    typingContext += (tparam -> TypeArg.Var(functionId, index))
}

case class MonoContext(solution: Solution, names: MonoNames) {
  var replacementTparams: Map[Id, TypeArg.Base] = Map.empty
}

def findConstraints(definitions: List[Toplevel])(using MonoFindContext): Constraints =
  definitions flatMap findConstraints

def findConstraints(definition: Toplevel)(using ctx: MonoFindContext): Constraints = definition match
  case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    findConstraints(body)
  case Toplevel.Def(id, block) => ???
  case Toplevel.Val(id, tpe, binding) => ???

def findConstraints(block: Block)(using ctx: MonoFindContext): Constraints = block match
  case BlockVar(id, annotatedTpe, annotatedCapt) => ???
  case BlockLit(tparams, cparams, vparams, bparams, body) => ???
  case Unbox(pure) => ???
  case New(impl) => ???

def findConstraints(stmt: Stmt)(using ctx: MonoFindContext): Constraints = stmt match
  case Let(id, annotatedTpe, binding, body) => findConstraints(binding) ++ findConstraints(body)
  case Return(expr) => findConstraints(expr)
  case Val(id, annotatedTpe, binding, body) => findConstraints(binding) ++ findConstraints(body)
  case App(callee: BlockVar, targs, vargs, bargs) => List(Constraint(targs.map(findId).toVector, callee.id))
  case If(cond, thn, els) => findConstraints(cond) ++ findConstraints(thn) ++ findConstraints(els)
  case o => println(o); ???

def findConstraints(expr: Expr)(using ctx: MonoFindContext): Constraints = expr match
  case DirectApp(b, List(), vargs, bargs) => List.empty
  case ValueVar(id, annotatedType) => List.empty
  case Literal(value, annotatedType) => List.empty
  case o => println(o); ???

def findId(vt: ValueType)(using ctx: MonoFindContext): TypeArg = vt match
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
      })
      nbs ++= l
    )
    nbs

  solved

def productAppend[A](ls: List[List[A]], rs: List[A]): List[List[A]] =
  rs.flatMap(r => ls.map(l => l :+ r))

def monomorphize(definitions: List[Toplevel])(using ctx: MonoContext): List[Toplevel] =
  var newDefinitions: List[Toplevel] = List.empty
  definitions.foreach(definition => newDefinitions ++= monomorphize(definition))
  newDefinitions

def monomorphize(toplevel: Toplevel)(using ctx: MonoContext): List[Toplevel] = toplevel match
  case Toplevel.Def(id, BlockLit(List(), cparams, vparams, bparams, body)) => 
    List(Toplevel.Def(id, BlockLit(List.empty, cparams, vparams, bparams, monomorphize(body))))
  case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) => 
    val monoTypes = ctx.solution(id).toList
    monoTypes.map(baseTypes => 
      val replacementTparams = tparams.zip(baseTypes).toMap
      ctx.replacementTparams ++= replacementTparams
      Toplevel.Def(ctx.names(id, baseTypes), BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body)))
    )
  case Toplevel.Def(id, block) => ???
  case Toplevel.Val(id, tpe, binding) => ???

def monomorphize(block: Block)(using ctx: MonoContext): Block = block match
  case b: BlockVar => monomorphize(b)
  case o => println(o); ???

def monomorphize(blockVar: BlockVar, replacementId: FunctionId)(using ctx: MonoContext): BlockVar = blockVar match
  case BlockVar(id, BlockType.Function(List(), cparams, vparams, bparams, result), annotatedCapt) => blockVar
  // TODO: What is in annotated captures. Does it need to be handled?
  case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCapt) => 
    val monoAnnotatedTpe = BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
    BlockVar(replacementId, monoAnnotatedTpe, annotatedCapt)
  case o => ???

def monomorphize(stmt: Stmt)(using ctx: MonoContext): Stmt = stmt match
  case Return(expr) => Return(monomorphize(expr))
  case Val(id, annotatedTpe, binding, body) => Val(id, monomorphize(annotatedTpe), monomorphize(binding), monomorphize(body))
  case App(callee: BlockVar, targs, vargs, bargs) => 
    val replacementId = replacementIdFromTargs(callee.id, targs)
    App(monomorphize(callee, replacementId), List.empty, vargs map monomorphize, bargs map monomorphize)
  case Let(id, annotatedTpe, binding, body) => Let(id, monomorphize(annotatedTpe), monomorphize(binding), monomorphize(body))
  case If(cond, thn, els) => If(monomorphize(cond), monomorphize(thn), monomorphize(els))
  case o => println(o); ???

def monomorphize(expr: Expr)(using ctx: MonoContext): Expr = expr match
  case DirectApp(b, targs, vargs, bargs) => 
    val replacementId = replacementIdFromTargs(b.id, targs)
    DirectApp(monomorphize(b, replacementId), List.empty, vargs map monomorphize, bargs map monomorphize)
  case o => println(o); ???

def monomorphize(pure: Pure)(using ctx: MonoContext): Pure = pure match
  case ValueVar(id, annotatedType) => ValueVar(id, monomorphize(annotatedType))
  case PureApp(b, targs, vargs) => 
    val replacementId = replacementIdFromTargs(b.id, targs)
    PureApp(monomorphize(b, replacementId), List.empty, vargs map monomorphize)
  case Literal(value, annotatedType) => Literal(value, monomorphize(annotatedType))
  case o => println(o); ???

def monomorphize(valueParam: ValueParam)(using ctx: MonoContext): ValueParam = valueParam match 
  case ValueParam(id, tpe) => ValueParam(id, monomorphize(tpe))

def monomorphize(blockParam: BlockParam)(using ctx: MonoContext): BlockParam = blockParam match
  // TODO: Same question as in block
  case BlockParam(id, tpe, capt) => BlockParam(id, monomorphize(tpe), capt) 

def monomorphize(blockType: BlockType)(using ctx: MonoContext): BlockType = blockType match 
  case BlockType.Function(tparams, cparams, vparams, bparams, result) => 
    BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
  case o => println(o); ???

def monomorphize(valueType: ValueType)(using ctx: MonoContext): ValueType = valueType match
  case ValueType.Var(name) => ValueType.Var(ctx.replacementTparams(name).tpe)
  case ValueType.Data(name, targs) => ValueType.Data(name, targs)
  case o => println(o); ???

var monoCounter = 0
def freshMonoName(baseId: Id, tpe: TypeArg.Base): Id =
  monoCounter += 1
  Id(baseId.name.name + tpe.tpe.name.name + monoCounter)

def freshMonoName(baseId: Id, tpes: Vector[TypeArg.Base]): Id =
  if (tpes.length == 0) return baseId

  monoCounter += 1
  val tpesString = tpes.map(tpe => tpe.tpe.name.name).mkString
  Id(baseId.name.name + tpesString + monoCounter)

def replacementIdFromTargs(id: FunctionId, targs: List[ValueType])(using ctx: MonoContext): FunctionId =
  if (targs.isEmpty) return id
  var baseTypes: List[TypeArg.Base] = List.empty
  targs.foreach({
    case ValueType.Data(name, targs) => baseTypes :+= TypeArg.Base(name)
    case ValueType.Var(name) => baseTypes :+= ctx.replacementTparams(name)
    case ValueType.Boxed(tpe, capt) => 
  })
  ctx.names((id, baseTypes.toVector))

// Old stuff

// type PolyConstraints = Map[Id, Set[PolyType]]
// type PolyConstraintsSolved = Map[Id, Set[PolyType.Base]]
// type PolyConstraintSingle = Map[Id, PolyType.Base]

// class MonoContext(val solvedConstraints: PolyConstraintsSolved, var monoDefs: Map[Id, Map[List[PolyType.Base], (Id, Block)]] = Map.empty)



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
