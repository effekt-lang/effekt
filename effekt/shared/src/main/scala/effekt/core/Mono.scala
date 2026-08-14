package effekt
package core

import effekt.context.Context
import effekt.util.messages.ErrorMessageReifier
import effekt.core.Type.functionType
import effekt.core.DeBruijn.toDeBruijn
import effekt.core.DeBruijn.addTparams
import effekt.core.optimizer.Deadcode
import effekt.core.optimizer.BindSubexpressions

object Mono extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "mono"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = {
    input match {
      case CoreTransformed(source, tree, mod, core) =>
        val main = Context.ensureMainExists(mod)
        val reachable = Deadcode.remove(main, core)
        val bound = BindSubexpressions.transform(reachable)
        val preprocessed = preprocess(Deadcode.remove(main, bound))
        Some(CoreTransformed(source, tree, mod, specialize(preprocessed)))
    }
  }
}

def specialize(module: ModuleDecl)(using Context): ModuleDecl = module match
  case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
    val monoFindContext = MonoFindContext()
    val dctx = DeclarationContext(declarations, externs)
    val constraints =
      findConstraints(definitions)(using monoFindContext) ++
      externs.flatMap(findConstraints(_)(using monoFindContext)) ++
      declarations.flatMap(findConstraints(_)(using monoFindContext))

    val solution = solveConstraints(constraints)

    var monoFunNames: MonoFunNames = Map.empty
    val monoTpeNames: MonoTpeNames = collection.mutable.Map.empty
    solution.foreach((id, targs) =>
      if (dctx.findExternDef(id).isDefined) {
        targs.foreach(vb => monoFunNames += ((id, vb) -> id))
      } else if (dctx.findData(id).isDefined) {
        val data = dctx.findData(id).get
        targs.foreach(vb => freshMonoTypeName(data.id, vb, monoTpeNames))
      } else {
        targs.foreach(vb => monoFunNames += ((id, vb) -> freshMonoName(id, vb)))
      }
    )

    declarations.foreach {
      case Data(id, List(), constructors) => monoTpeNames += ((id, Vector.empty) -> ValueType.Data(id, List.empty))
      case Interface(id, List(), properties) => monoFunNames += ((id, Vector.empty) -> id)
      case _ => ()
    }

    val polyExternDefs: List[Id] = externs.collect {
      case Extern.Def(id, _, tparams, _, _, _, _, _, _) if tparams.nonEmpty => id
    }
    val monoContext = MonoContext(solution, monoFunNames, monoTpeNames, polyExternDefs)
    val monoDecls = declarations.flatMap(monomorphize(_)(using monoContext)(using dctx))
    val monoDefs = monomorphize(definitions)(using monoContext)(using Context, dctx)
    ModuleDecl(path, includes, monoDecls, externs, monoDefs, exports)

object DeBruijn {

  // Filled when handling new function
  var typeToIndex: Map[Id, Index] = Map.empty

  // https://blueberrywren.dev/blog/debruijn-explanation/
  // λa. λb. λc. c

  // indexes:
  // λ λ λ 0
  // ^ ^ ^
  // 2 1 0

  // levels:
  // λ λ λ 2
  // ^ ^ ^
  // 0 1 2

  // In lambda bump everything
  // -> we are using de bruijn indices
  case class Index(level: Int, position: Int)
  // Currently only supported toplevel (non-debruijn) captures (e.g. {io})
  // type Captures = Set[Id]

  def bumpTypeLevels() = {
    typeToIndex = typeToIndex.map((id, index) =>
      (id, Index(index.level + 1, index.position))
    )
  }

  def addTparams(tparams: List[Id]) = {
    tparams.zipWithIndex.foreach((id, index) => {
      typeToIndex += (id -> Index(0, index))
    })
  }

  enum ValueType {
    case Var(index: Index)
    case Data(name: Id, targs: List[ValueType])
    case Boxed(tpe: BlockType, capt: Captures)
  }

  enum BlockType {
    case Function(tarity: Int, carity: Int, vparams: List[ValueType], bparams: List[BlockType], result: ValueType)
    case Interface(name: Id, targs: List[ValueType])
  }

  def toDeBruijn(tpe: core.ValueType): ValueType = tpe match {
    case core.ValueType.Boxed(tpe, capt) =>
      // FIXME: Actually only support toplevel captures, if we pass stuff like this we will crash with some programs
      ValueType.Boxed(toDeBruijn(tpe), capt)
    case core.ValueType.Data(name, targs) =>
      ValueType.Data(name, targs map toDeBruijn)
    case core.ValueType.Var(name) =>
      ValueType.Var(typeToIndex(name))
  }

  def toDeBruijn(tpe: core.BlockType): BlockType = tpe match {
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) => {

      // We are one level deeper, bump all existing Indices by one level
      // save current map to reset to later
      val savedTypeIndexMap = typeToIndex
      bumpTypeLevels()

      // Insert new tparams at level 0
      addTparams(tparams)

      // Handle all the types of the lower level, then reset our type -> index map to before we went to this level
      val vparams_ = vparams map toDeBruijn
      val bparams_ = bparams map toDeBruijn
      val result_ = toDeBruijn(result)

      typeToIndex = savedTypeIndexMap

      BlockType.Function(tparams.size, cparams.size, vparams_, bparams_, result_)
    }
    case core.BlockType.Interface(name, targs) =>
      BlockType.Interface(name, targs map toDeBruijn)
  }

}

/**
Rewrites this:

{{{
    def higherorder { f : [A] (A) => A } = f[Int](42)

    def main() = {
        println(higherorder { [B] (x) => x })
    }
}}}

To this:

{{{
    interface Poly {
    def apply[A](a: A): A
    }

    def higherorder { f : Poly } = f.apply[Int](42)
    def main() = {
    def id = new Poly {
        def apply[B](b: B) = b
    }
    println(higherorder { id })
    }
}}}
*/
case class FreshNames(interface: Id, apply: Id)
case class ReplacementInterface(names: FreshNames, interface: BlockType.Interface)
class PreprocessContext {
// List of Interfaces that are emitted during preprocessing
var interfaces: List[Declaration.Interface] = List.empty

// Map from function name + block arg index -> (interface + interface operation)
var replacements: Map[DeBruijn.BlockType, FreshNames] = Map.empty

// apply ids for block arguments
var appReplacements: Map[Id, (Id, Block.BlockVar)] = Map.empty

def freshInterfaceNames(): FreshNames =
  FreshNames(Id("Poly"), Id("apply"))

def freshInterface(blockTpe: BlockType.Function, block: Block.BlockLit): FreshNames =
  val freshNames = freshInterfaceNames()
  // Fresh tparams and subst?
  val extendedBlockTpe = BlockType.Function(blockTpe.tparams, blockTpe.cparams, blockTpe.vparams, blockTpe.bparams, blockTpe.result)
  val property = Property(freshNames.apply, extendedBlockTpe)
  interfaces +:= Declaration.Interface(freshNames.interface, block.tparams, List(property))
  freshNames

def emit(blockId: Id, blockTpe: BlockType.Function, block: Block.BlockLit): BlockType.Interface =
  val debruijnBlockTpe = toDeBruijn(blockTpe)
  val interface = replacements.get(debruijnBlockTpe) match {
    case None => {
      val interface = freshInterface(blockTpe, block)
      replacements += debruijnBlockTpe -> interface
      interface
    }
    case Some(value) => value
  }

  val targs = block.tparams.map(id => ValueType.Var(id))
  val btInterface: BlockType.Interface = BlockType.Interface(interface.interface, targs)
  val callee: Block.BlockVar = BlockVar(blockId, btInterface, Set(blockId))
  appReplacements += blockId -> (interface.apply, callee)

  btInterface
}


def preprocess(module: ModuleDecl): ModuleDecl = module match
  case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
    val preprocessContext = PreprocessContext()
    val defns = preprocess(definitions)(using preprocessContext)
    ModuleDecl(path, includes, declarations ++ preprocessContext.interfaces, externs, defns, exports)


def preprocess(definitions: List[Toplevel])(using PreprocessContext): List[Toplevel] =
  definitions.map({
    case Toplevel.Def(id, block) => Toplevel.Def(id, preprocess(block))
    case Toplevel.Val(id, binding) => Toplevel.Val(id, preprocess(binding))
  })

def preprocess(block: Block)(using PreprocessContext): Block = block match
  case b@BlockVar(id, annotatedTpe, annotatedCapt) => preprocess(b)
  case b@BlockLit(tparams, cparams, vparams, bparams, body) => preprocess(b)
  // TODO: Recurse everywhere
  case Unbox(pure) => block
  case New(impl) => block


def preprocess(block: Block.BlockLit)(using ctx: PreprocessContext): Block.BlockLit =
  // TODO: Replace with "New"
  val processedBparams = block.bparams.map(blockParam => {
    blockParam.tpe match {
      case b: BlockType.Function =>
        if(b.tparams.nonEmpty) {
          val interface = ctx.emit(blockParam.id, b, block)
          BlockParam(blockParam.id, interface, blockParam.capt)
        } else {
          blockParam
        }
      case BlockType.Interface(name, targs) =>
        blockParam
    }
  })
  addTparams(block.tparams)
  Block.BlockLit(block.tparams, block.cparams, block.vparams, processedBparams, preprocess(block.body))

def preprocess(block: Block.BlockVar)(using PreprocessContext): Block.BlockVar = block match {
  case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(id, preprocess(annotatedTpe), annotatedCapt)
}

def preprocessBargs(bargs: List[Block], targs: List[ValueType])(using ctx: PreprocessContext): List[Block] = bargs map {
  // TODO: add example to each case
  case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(id, preprocess(annotatedTpe), annotatedCapt)
  case block@BlockLit(tparams, cparams, vparams, bparams, body) =>
    val debruijnBlockTpe = toDeBruijn(block.tpe)
    ctx.replacements.get(debruijnBlockTpe) match {
      case Some(value) => {
        val defnId = Id(value.interface.name.name ++ "_defn")
        val freshOp = Operation(value.apply, tparams, cparams, vparams, bparams, preprocess(body))
        val interfaceTpe: BlockType.Interface = BlockType.Interface(value.interface, targs)
        Block.New(Implementation(interfaceTpe, List(freshOp)))
      }
      case None => Block.BlockLit(tparams, cparams, vparams, bparams, preprocess(body))
    }
  case Unbox(pure) => Unbox(pure)
  case New(impl) => New(impl)
}

def preprocess(stmt: Stmt)(using ctx: PreprocessContext): Stmt = stmt match {
  case App(callee, targs, vargs, bargs) => {
    val processedBargs = preprocessBargs(bargs, targs)

    callee match {
      case BlockVar(id, annotatedTpe, annotatedCapt) => {
        ctx.appReplacements.get(id) match {
          case Some((replacementId, blockVar)) =>
            Invoke(blockVar, replacementId, annotatedTpe, targs, vargs, processedBargs)
          case None =>
            val calleeTpe = callee.functionType
            val updatedCalleeTpe = BlockType.Function(calleeTpe.tparams, calleeTpe.cparams, calleeTpe.vparams, processedBargs.map(_.tpe), calleeTpe.result)
            App(Block.BlockVar(id, updatedCalleeTpe, annotatedCapt), targs, vargs, processedBargs)
        }
      }
      case New(impl) => App(New(impl), targs, vargs, processedBargs)
      // TODO: I tought this should not happen, but it does (i.e. in examples/llvm/nosuchelement.effekt and others)
      case BlockLit(tparams, cparams, vparams, bparams, body) => App(preprocess(callee), targs, vargs, bargs)
      case Unbox(pure) => sys error "Should not happen, BindSubexpressions ran before"
    }
  }
  case Val(id, binding, body) =>
    Val(id, preprocess(binding), preprocess(body))
  case ImpureApp(id, callee, targs, vargs, bargs, body) =>
    ImpureApp(id, callee, targs, vargs, bargs, preprocess(body))
  case Return(expr) => Return(expr)
  case Alloc(id, init, region, body) => Alloc(id, init, region, preprocess(body))
  case Def(id, block, body) => Def(id, preprocess(block), preprocess(body))
  case Get(id, annotatedTpe, ref, annotatedCapt, body) => Get(id, annotatedTpe, ref, annotatedCapt, preprocess(body))
  case Hole(tpe, span) => stmt
  case If(cond, thn, els) => If(cond, preprocess(thn), preprocess(els))
  case Invoke(callee, method, methodTpe, targs, vargs, bargs) => stmt
  case Let(id, binding, body) => Let(id, binding, preprocess(body))
  case Match(scrutinee, matchTpe, clauses, default) => stmt
  case Put(ref, annotatedCapt, value, body) => Put(ref, annotatedCapt, value, preprocess(body))
  case Region(body) => stmt
  case Reset(body) => stmt
  case Resume(k, body) => Resume(k, preprocess(body))
  case Shift(prompt, k, body) => stmt
  case Var(ref, init, capture, body) => Var(ref, init, capture, preprocess(body))
}

def preprocess(blockType: BlockType)(using ctx: PreprocessContext): BlockType = blockType match {
  case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
    val bruijnBlockTpe = toDeBruijn(blockType)
    ctx.replacements.get(bruijnBlockTpe) match {
      case Some(name) => BlockType.Interface(name.interface, List.empty)
      case None => BlockType.Function(tparams, cparams, vparams map preprocess, bparams map preprocess, preprocess(result))
    }

  case BlockType.Interface(name, targs) => BlockType.Interface(name, targs)
}

// FIXME: Implement
def preprocess(valueTpe: ValueType)(using ctx: PreprocessContext): ValueType = valueTpe

type FunctionId = Id
case class MonoConstraint(lower: Vector[TypeArg], upper: FunctionId)
type MonoConstraints = List[MonoConstraint]

type Ground = TypeArg.Base | TypeArg.Boxed
type Solution = Map[FunctionId, Set[Vector[Ground]]]
type MonoFunNames = Map[(FunctionId, Vector[Ground]), FunctionId]
// Option[Int] => Option_Int[]
// Array[Option[Int]] => Array[Option_Int]
//       -----------           ----------
//          ground                mono
type MonoTpeNames = collection.mutable.Map[(Id, Vector[Ground]), ValueType.Data]


enum TypeArg {
  case Base(tpe: Id, targs: List[TypeArg])
  case Var(funId: FunctionId, pos: Int)
  case Boxed(tpe: BlockType, capt: Captures)
}

// Type Id -> Var
type TypeParams = Map[Id, TypeArg.Var]

class MonoFindContext {
  var typingContext: TypeParams = Map()

  def extendTypingContext(tparam: Id, index: Int, functionId: FunctionId) =
    typingContext += (tparam -> TypeArg.Var(functionId, index))
}

case class MonoContext(solution: Solution, funNames: MonoFunNames, tpeNames: MonoTpeNames, polyExternDefs: List[Id]) {
  var replacementTparams: Map[Id, Ground] = Map.empty

  lazy val invertedTpeNames: Map[ValueType.Data, (Id, Vector[Ground])] = tpeNames.map { case (k, v) => (v, k) }.toMap

  def instantiateTparams(tparams: List[Id], targs: List[Ground]) = {
    assert(targs.size == tparams.size, s"Wrong number of type arguments\n  targs: ${targs}\n  tparams: ${tparams}")
    replacementTparams ++= tparams.zip(targs).toMap
  }

  def isPolyExtern(id: Id) = polyExternDefs.contains(id)
}

def findConstraints(definitions: List[Toplevel])(using MonoFindContext): MonoConstraints =
  definitions flatMap findConstraints

def findConstraints(definition: Toplevel)(using ctx: MonoFindContext): MonoConstraints = definition match
  case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    findConstraints(body)
  case Toplevel.Def(id, block) =>
    findConstraints(block)
  case Toplevel.Val(id, binding) =>
    findConstraints(binding)

def findConstraints(declaration: Declaration)(using ctx: MonoFindContext): MonoConstraints = declaration match
  // Maybe[T] { Just[](x: T) }
  case Data(id, tparams, constructors) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    constructors.map { constr =>
      val arity = tparams.size
      val constructorArgs = (0 until arity).map(index =>
        TypeArg.Var(constr.id, index) // Just.0
      ).toVector // < Just.0 >
      MonoConstraint(constructorArgs, id) // < Just.0 > <: Maybe
    }.filter(_.lower.nonEmpty)
  case Interface(id, tparams, properties) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    properties.map { prop =>
      val arity = tparams.size
      val propArgs = (0 until arity).map(index =>
        TypeArg.Var(prop.id, index) // Just.0
      ).toVector // < Just.0 >
      MonoConstraint(propArgs, id) // < Just.0 > <: Maybe
    }.filter(_.lower.nonEmpty) ++ (properties flatMap findConstraints)

def findConstraints(extern: Extern)(using ctx: MonoFindContext): MonoConstraints = extern match {
  case Extern.Def(id, qualifiedSignature, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    val (_, constraints) = findConstraints(vparams.map(_.tpe))
    constraints
  case Extern.Data(id, tparams, body) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    List()
  case Extern.Interface(id, tparams, body) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    List()
  case Extern.Include(_, _) => List()
}

def findConstraints(property: Property)(using ctx: MonoFindContext): MonoConstraints = property match {
  case Property(id, tpe@BlockType.Function(tparams, cparams, vparams, bparams, result)) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    findConstraints(tpe, id)
  case Property(id, tpe@BlockType.Interface(name, targs)) => findConstraints(tpe)
}

def findConstraints(block: Block)(using ctx: MonoFindContext): MonoConstraints = block match
  case BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt) => findConstraints(annotatedTpe)
  case BlockVar(id, annotatedTpe: BlockType.Function, annotatedCapt) => findConstraints(annotatedTpe, id)
  case BlockLit(tparams, cparams, vparams, bparams, body) => findConstraints(body)
  case Unbox(pure) => findConstraints(pure)
  case New(impl) => findConstraints(impl)

def findConstraints(blockType: BlockType.Interface)(using ctx: MonoFindContext): MonoConstraints = blockType match
  case BlockType.Interface(name, targs) =>
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, name)) ++ constraints

def findConstraints(blockType: BlockType.Function, fnId: Id)(using ctx: MonoFindContext): MonoConstraints = blockType match
  case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, fnId))
    List()

def findConstraints(impl: Implementation)(using ctx: MonoFindContext): MonoConstraints = impl match
  case Implementation(interface, operations) =>
    findConstraints(interface) ++
    (operations flatMap findConstraints)

def findConstraints(operation: Operation)(using ctx: MonoFindContext): MonoConstraints = operation match
  case Operation(name, tparams, cparams, vparams, bparams, body) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, name))
    findConstraints(body)

def findConstraints(clause: (Id, BlockLit))(using ctx: MonoFindContext): MonoConstraints = clause match
  case (id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    findConstraints(body)

def findConstraints(stmt: Stmt)(using ctx: MonoFindContext): MonoConstraints = stmt match
  case Let(id, binding, body) => findConstraints(binding) ++ findConstraints(body)
  case Return(expr) => findConstraints(expr)
  case Val(id, binding, body) => findConstraints(binding) ++ findConstraints(body)
  case Var(ref, init, capture, body) => findConstraints(body)
  case ImpureApp(id, callee, targs, vargs, bargs, body) =>
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, callee.id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ findConstraints(body) ++ constraints
  case App(callee: BlockVar, targs, vargs, bargs) =>
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, callee.id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  // TODO: Very specialized, but otherwise passing an id that matches in monomorphize is hard
  //       although I'm not certain any other case can even happen
  // TODO: part 2, also update the implementation in monomorphize if changing this
  case App(Unbox(ValueVar(id, annotatedType)), targs, vargs, bargs) =>
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  case App(callee, targs, vargs, bargs) =>
    findConstraints(callee) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints)
  case Invoke(callee @ BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt), method, methodTpe, targs, vargs, bargs) =>
    val (newTargs, constraints) = findConstraints(annotatedTpe.targs ++ targs)
    List(MonoConstraint(newTargs.toVector, method)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  case Invoke(Unbox(ValueVar(id, annotatedType)), method, methodTpe, targs, vargs, bargs) =>
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, method)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
    findConstraints(callee) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints)
  case Reset(body) => findConstraints(body)
  case If(cond, thn, els) => findConstraints(cond) ++ findConstraints(thn) ++ findConstraints(els)
  case Def(id, BlockLit(tparams, cparams, vparams, bparams, bbody), body) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    findConstraints(bbody) ++ findConstraints(body)
  case Def(id, block, body) =>
    findConstraints(block) ++ findConstraints(body)
    // FIXME: Handle k as well
  case Shift(prompt, k, body) => findConstraints(prompt) ++ findConstraints(body)
  case Match(scrutinee, tpe, clauses, default) => clauses.flatMap(findConstraints) ++ findConstraints(default)
  case Resume(k, body) => findConstraints(k) ++ findConstraints(body)
  case Get(id, annotatedTpe, ref, annotatedCapt, body) => findConstraints(body)
  case Put(ref, annotatedCapt, value, body) => findConstraints(value) ++ findConstraints(body)
  case Alloc(id, init, region, body) => findConstraints(init) ++ findConstraints(body)
  case Region(body) => findConstraints(body)
  case Hole(tpe, span) => List.empty

def findConstraints(opt: Option[Stmt])(using ctx: MonoFindContext): MonoConstraints = opt match
  case None => List.empty
  case Some(stmt) => findConstraints(stmt)

def findConstraints(expr: Expr)(using ctx: MonoFindContext): MonoConstraints = expr match
  case PureApp(b, targs, vargs) =>
    val (newTargs, constraints) = findConstraints(targs)
    MonoConstraint(newTargs.toVector, b.id) :: constraints
  case ValueVar(id, annotatedType) => List.empty
  case Literal(value, annotatedType) => List.empty
  case Make(data, tag, targs, vargs) =>
    val (dataTargs, dataConstraints) = findConstraints(data.targs)
    val (newTargs, constraints) = findConstraints(data.targs ++ targs)
    List(MonoConstraint(dataTargs.toVector, data.name), MonoConstraint(newTargs.toVector, tag)) ++ // <Int> <: Just
    dataConstraints ++ constraints
  case Box(b, annotatedCapture) =>
    findConstraints(b)

def findConstraints(vts: List[ValueType])(using ctx: MonoFindContext): (List[TypeArg], MonoConstraints) = {
  val vtFindConstraints = vts map findConstraints
  val targs = vtFindConstraints.map(_._1)
  val constraints = vtFindConstraints.flatMap(_._2)
  (targs, constraints)
}

def findConstraints(vt: ValueType)(using ctx: MonoFindContext): (TypeArg, MonoConstraints) = vt match {
  case ValueType.Boxed(tpe@BlockType.Function(tparams, cparams, vparams, bparams, result), capt) => {
    // TODO: Perhaps recurse into tpe
    // TODO: What do I do with a function type here? It does not have a name which does not work for my current findConstraints
    (TypeArg.Boxed(tpe, capt), List.empty)
  }
  case ValueType.Boxed(tpe@BlockType.Interface(name, targs), capt) => {
    val constraints = findConstraints(tpe)
    (TypeArg.Boxed(tpe, capt), constraints)
  }
  case ValueType.Data(name, targs) => {
    val (newTargs, constraints) = findConstraints(targs)
    val additionalConstraints = if (newTargs.nonEmpty) {
      List(MonoConstraint(newTargs.toVector, name))
    } else {
      List.empty
    }
    (TypeArg.Base(name, newTargs), constraints ++ additionalConstraints)
  }
  case ValueType.Var(name) => (ctx.typingContext(name), List.empty)
}

def filterBounds(bounds: Map[Id, Set[Vector[TypeArg]]]): Map[Id, Set[Vector[Ground]]] = bounds.view.mapValues(filterNonGround).toMap

def filterNonGround(bounds: Set[Vector[TypeArg]]): Set[Vector[Ground]] = bounds.flatMap(filterNonGround)

def filterNonGround(bound: Vector[TypeArg]): Option[Vector[Ground]] = {
  var res: Vector[Ground] = Vector.empty
  bound.foreach({
    case TypeArg.Base(id, targs) => {
      val groundTargs = filterNonGround(targs.toVector)
      groundTargs match {
        case None => ()
        case Some(_) => res :+= TypeArg.Base(id, targs)
      }
    }
    case TypeArg.Boxed(tpe, capt) => res :+= TypeArg.Boxed(tpe, capt)
    case TypeArg.Var(funId, pos) => ()
  })
  if (res.size == bound.size) {
    Some(res)
  } else {
    None
  }
}

// One specific variant of a type variable
type Variant = (Id, Vector[TypeArg])
type Variants = List[Variant]

// Substitution of all combinations of variants of type variables
type Substitution = Map[Id, Vector[TypeArg]]
type Substitutions = List[Substitution]

def solveConstraints(constraints: MonoConstraints)(using Context): Solution = {
  val groupedConstraints = constraints.groupBy(c => c.upper)
  var bounds = groupedConstraints.map((sym, constraints) => (sym -> constraints.map(c => c.lower).toSet))

  while (true) {
    val previousBounds = bounds
    bounds.foreach((sym, tas) =>
      val bound = propagateBounds(sym, tas)
      bounds += (sym -> bound)
    )

    if (previousBounds == bounds) return filterBounds(bounds)
  }

  def propagateBounds(funId: FunctionId, filteredConstraints: Set[Vector[TypeArg]]): Set[Vector[TypeArg]] =
    var nbs: Set[List[TypeArg]] = Set.empty
    filteredConstraints.foreach(b =>

      def solveTypeArg(typeArg: TypeArg, substitution: Map[Id, Vector[TypeArg]], taPos: Int, insideTypeConstructor: Boolean): TypeArg = typeArg match {
        case TypeArg.Base(tpe, targs) =>
          val solvedTargs = targs.zipWithIndex.map((ta, ind) => solveTypeArg(ta, substitution, ind, true))
          TypeArg.Base(tpe, solvedTargs)
        case TypeArg.Boxed(tpe, capt) => TypeArg.Boxed(tpe, capt)
        case TypeArg.Var(fnId, pos) =>
          if (funId == fnId && taPos == pos && insideTypeConstructor) Context.abort(pretty"Detected polymorphic recursion for '${funId}' at position '${taPos}'")
          substitution(fnId)(pos)
      }

      // a => <Int, Char>, <Double, Bool>
      // b => <a.0, a.1>
      def collectBounds(typeArg: TypeArg): List[FunctionId] = typeArg match {
        case TypeArg.Var(fnId, _) => List(fnId)
        case TypeArg.Base(_, targs) => targs.flatMap(collectBounds)
        case _ => List()
      }
      val substitutions = b.flatMap(collectBounds).distinct.foldLeft(List(Map.empty): Substitutions) {
        case (substitutions, funId) =>
          val variants = bounds.getOrElse(funId, Set.empty).map((funId, _)).toList
          mapProductAppend(substitutions, variants)
      }

      substitutions.foreach(substitution => {
        val l = b.zipWithIndex.map((typeArg, ind) => {
          solveTypeArg(typeArg, substitution, ind, false)
        }).toList
        nbs += l
      })
    )
    nbs.map(l => l.toVector)

  // we will never get here
  filterBounds(bounds)
}

def productAppend[A](ls: List[List[A]], rs: List[A]): List[List[A]] =
  for { l <- ls; r <- rs } yield l :+ r

// Cross product of existing substitutions and all variants for one type variable
def mapProductAppend(ls: Substitutions, rs: Variants): List[Map[Id, Vector[TypeArg]]] =
  for { l <- ls; r <- rs } yield l + r

def monomorphize(definitions: List[Toplevel])(using ctx: MonoContext)(using Context, DeclarationContext): List[Toplevel] =
  var newDefinitions: List[Toplevel] = List.empty
  definitions.foreach(definition => newDefinitions ++= monomorphize(definition))
  newDefinitions

def monomorphize(toplevel: Toplevel)(using ctx: MonoContext)(using Context, DeclarationContext): List[Toplevel] = toplevel match
  case Toplevel.Def(id, BlockLit(List(), cparams, vparams, bparams, body)) =>
    List(Toplevel.Def(id, Renamer.rename(BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body)))))
  case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
    val monoTypes = ctx.solution(id).toList
    monoTypes.map(baseTypes =>
      ctx.instantiateTparams(tparams, baseTypes.toList)
      Toplevel.Def(ctx.funNames(id, baseTypes), Renamer.rename(BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))))
    )
  case Toplevel.Def(id, block) =>
    List(Toplevel.Def(id, monomorphize(block)))
  case Toplevel.Val(id, binding) =>
    List(Toplevel.Val(id, monomorphize(binding)))

def monomorphize(decl: Declaration)(using ctx: MonoContext)(using DeclarationContext): List[Declaration] = decl match
  case Data(id, tparams, constructors) =>
    val monoTypes = ctx.solution.getOrElse(id, Set.empty).toList
    if (monoTypes.isEmpty) {
      List(Data(id, tparams, constructors.flatMap(monomorphize(_, Vector.empty))))
    } else {
      monoTypes.map(baseTypes =>
        ctx.instantiateTparams(tparams, baseTypes.toList)
        Declaration.Data(ctx.tpeNames(id, baseTypes).name, List.empty, constructors.flatMap(constr => monomorphize(constr, baseTypes)))
      )
    }
  case Interface(id, tparams, properties) =>
    val monoTypes = ctx.solution.getOrElse(id, Set.empty).toList
    if (monoTypes.isEmpty) {
      List(Declaration.Interface(id, tparams, properties.flatMap(monomorphize(_, Vector.empty))))
    } else {
      monoTypes.map(baseTypes =>
        ctx.instantiateTparams(tparams, baseTypes.toList)
        val monoProp = properties.flatMap(prop => monomorphize(prop, baseTypes))
        val interfaceName = ctx.funNames(id, baseTypes)
        if (interfaceName == id) {
          Declaration.Interface(interfaceName, tparams, monoProp)
        } else {
          Declaration.Interface(interfaceName, List.empty, monoProp)
        }
      )
    }

def monomorphize(property: Property, variant: Vector[Ground])(using ctx: MonoContext)(using DeclarationContext): List[Property] = property match {
  case Property(id, tpe@BlockType.Function(tparams, cparams, vparams, bparams, result)) => {
    val baseTypes = ctx.solution.getOrElse(id, Set.empty).toList
    val relevantTypes = baseTypes.filter(tpes => tpes.startsWith(variant))
    relevantTypes.map(baseType => {
      val existentialBaseTypes = baseType.drop(variant.size)
      ctx.instantiateTparams(tparams, existentialBaseTypes.toList)
      Property(ctx.funNames((id, baseType)), monomorphize(tpe))
    })
  }
  case Property(id, tpe) => ???
}

def monomorphize(constructor: Constructor, variant: Vector[Ground])(using ctx: MonoContext)(using DeclarationContext): List[Constructor] = constructor match
  case Constructor(id, tparams, fields) =>
    // All solutions for this constructor
    val baseTypes = ctx.solution.getOrElse(id, Set.empty).toList
    // Filter solutions that do not belong to the variant currently being handled
    val relevantTypes = baseTypes.filter(tpes => tpes.startsWith(variant))
    // The solutions for constructors may have more types than the variant because of existentials
    // in which case we need to generate multiple constructors
    relevantTypes.map(baseType => {
      // Remove types not relevant for existentials (mono13)
      val existentialBaseTypes = baseType.drop(variant.size)
      ctx.instantiateTparams(tparams, existentialBaseTypes.toList)
      Constructor(ctx.funNames(id, baseType), List.empty, fields map monomorphize)
    })

def monomorphize(block: Block)(using ctx: MonoContext)(using Context, DeclarationContext): Block = block match
  case b: BlockLit => monomorphize(b)
  case b: BlockVar => monomorphize(b)
  case New(impl) => New(monomorphize(impl))
  case Unbox(pure) => Unbox(monomorphize(pure))

def monomorphize(impl: Implementation)(using ctx: MonoContext)(using Context, DeclarationContext): Implementation = impl match
  case Implementation(BlockType.Interface(name, targs), operations) =>
    val variant = (targs map toTypeArg).toVector
    Implementation(BlockType.Interface(replacementFun(name, targs), List.empty), operations.flatMap(op => monomorphize(op, variant)))

def monomorphize(interface: BlockType.Interface)(using ctx: MonoContext): BlockType.Interface = interface match
  case BlockType.Interface(name, targs) =>
    val funName = replacementFun(name, targs)
    BlockType.Interface(funName, List.empty)

def monomorphize(operation: Operation, variant: Vector[Ground])(using ctx: MonoContext)(using Context, DeclarationContext): List[Operation] = operation match
  case Operation(name, tparams, cparams, vparams, bparams, body) =>
    val baseTypes = ctx.solution.getOrElse(name, Set.empty).toList
    val relevantTypes = baseTypes.filter(tpes => tpes.startsWith(variant))
    relevantTypes.map(baseTypes =>
      val existentialBaseTypes = baseTypes.drop(variant.size)
      ctx.instantiateTparams(tparams, existentialBaseTypes.toList)
      Operation(ctx.funNames(name, baseTypes), List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))
    )


def monomorphize(block: BlockLit)(using ctx: MonoContext)(using Context, DeclarationContext): BlockLit = block match
  case BlockLit(tparams, cparams, vparams, bparams, body) =>
    BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))

def monomorphize(block: BlockVar)(using ctx: MonoContext)(using DeclarationContext): BlockVar = block match
  case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(id, monomorphize(annotatedTpe), annotatedCapt)

def monomorphize(field: Field)(using ctx: MonoContext)(using DeclarationContext): Field = field match
  case Field(id, tpe) => Field(id, monomorphize(tpe))

// FIXME: Not a big fan of this function needing so many extra parameters
def monomorphize(blockVar: BlockVar, replacementId: FunctionId, targs: List[ValueType])(using ctx: MonoContext)(using DeclarationContext): BlockVar = blockVar match
  case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCapt) if ctx.isPolyExtern(id) =>
    ctx.instantiateTparams(tparams, targs map toTypeArg)
    val annotatedTpe = BlockType.Function(tparams, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
    BlockVar(id, annotatedTpe, annotatedCapt)
  case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCapt) =>
    ctx.instantiateTparams(tparams, targs map toTypeArg)
    val monoAnnotatedTpe = BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
    BlockVar(replacementId, monoAnnotatedTpe, annotatedCapt)
  case BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt) =>
    BlockVar(id, monomorphize(annotatedTpe), annotatedCapt)

def monomorphize(stmt: Stmt)(using ctx: MonoContext)(using Context, DeclarationContext): Stmt = stmt match
  case Return(expr) =>
    Return(monomorphize(expr))
  case Val(id, binding, body) =>
    Val(id, monomorphize(binding), monomorphize(body))
  case Var(ref, init, capture, body) =>
    Var(ref, monomorphize(init), capture, monomorphize(body))
  case ImpureApp(id, callee, targs, vargs, bargs, body) =>
    ImpureApp(id, callee, targs map monomorphize, vargs map monomorphize, bargs map monomorphize, monomorphize(body))
  case App(callee: BlockVar, targs, vargs, bargs) if ctx.isPolyExtern(callee.id) =>
    ctx.instantiateTparams(callee.functionType.tparams, targs map toTypeArg)
    App(callee, targs map monomorphize, vargs map monomorphize, bargs map monomorphize)
  case App(callee: BlockVar, targs, vargs, bargs) =>
    val monoFnId = replacementFun(callee.id, targs)
    App(monomorphize(callee, monoFnId, targs), List.empty, vargs map monomorphize, bargs map monomorphize)
  // TODO: Highly specialized, see todo in findConstraints for info
  //       change at the same time as findConstraints
  case App(Unbox(ValueVar(id, annotatedTpe)), targs, vargs, bargs) =>
    App(Unbox(ValueVar(id, monomorphize(annotatedTpe))), List.empty, vargs map monomorphize, bargs map monomorphize)
  case App(callee, targs, vargs, bargs) =>
    App(monomorphize(callee), List.empty, vargs map monomorphize, bargs map monomorphize)
  case Let(id, binding, body) =>
    Let(id, monomorphize(binding), monomorphize(body))
  case If(cond, thn, els) =>
    If(monomorphize(cond), monomorphize(thn), monomorphize(els))
  case Invoke(Unbox(pure), method, methodTpe, targs, vargs, bargs) =>
    Invoke(Unbox(monomorphize(pure)), method, methodTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
  case Invoke(BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt), method, BlockType.Function(tparams, cparams, vparams, bparams, result), targs, vargs, bargs) =>
    val combinedTargs = annotatedTpe.targs ++ targs
    val replacementMethod = replacementFun(method, combinedTargs)
    val monoAnnotatedTpe = BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
    Invoke(BlockVar(id, monomorphize(annotatedTpe), annotatedCapt), replacementMethod, monoAnnotatedTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
  case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
    Invoke(monomorphize(callee), method, methodTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
  case Resume(k, body) =>
    Resume(monomorphize(k), monomorphize(body))
  case Reset(body) =>
    Reset(monomorphize(body))
  case Def(id, BlockLit(List(), cparams, vparams, bparams, bbody), body) =>
    Stmt.Def(id, BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(bbody)), monomorphize(body))
  case Def(id, BlockLit(tparams, cparams, vparams, bparams, bbody), body) =>
    val monoTypes = ctx.solution(id).toList
    // Monomorphizing inner functions may yield multiple definitions
    // which then need to be nested
    def nestDefs(defnTypes: List[Vector[Ground]]): Stmt = defnTypes match {
      case head :: next =>
        ctx.instantiateTparams(tparams, head.toList)
        Stmt.Def(ctx.funNames(id, head), BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(bbody)), nestDefs(next))
      case Nil => monomorphize(body)
    }
    nestDefs(monoTypes)
  case Def(id, block, body) =>
    val monoBlock = monomorphize(block)
    val monoBody = monomorphize(body)
    Def(id, monoBlock, monoBody)
  case Shift(prompt, k, body) =>
    Shift(monomorphize(prompt), monomorphize(k), monomorphize(body))
  case Match(scrutinee, matchTpe, clauses, default) =>
    // We need the type of the scrutinee, to be able to only monomorphize the cases to this variant
    val monoScrut = monomorphize(scrutinee)
    val variant = monoScrut.tpe match {
      // Get the type of this variant by inverting the monomorphized name of the scrutinee
      case t: ValueType.Data => ctx.invertedTpeNames.getOrElse(t, (t.name, Vector.empty))(1)
      // Ignore variant if we are matching on anything else (examples/pos/bidirectional/typeparametric.effekt)
      case _ => Vector.empty
    }

    Match(monoScrut, monomorphize(matchTpe), clauses.flatMap(clause => monomorphize(clause, variant)), monomorphize(default))
  case Get(id, annotatedTpe, ref, annotatedCapt, body) =>
    Get(id, monomorphize(annotatedTpe), ref, annotatedCapt, monomorphize(body))
  case Put(ref, annotatedCapt, value, body) =>
    Put(ref, annotatedCapt, monomorphize(value), monomorphize(body))
  case Alloc(id, init, region, body) =>
    Alloc(id, monomorphize(init), region, monomorphize(body))
  case Region(body) =>
    Region(monomorphize(body))
  case Hole(tpe, span) =>
    Hole(monomorphize(tpe), span)

def monomorphize(clause: (Id, BlockLit), variant: Vector[Ground])(using ctx: MonoContext)(using Context, DeclarationContext): List[(Id, BlockLit)] = clause match
  case (id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
    val baseTypes = ctx.solution.getOrElse(id, Set.empty).toList
    val relevantTypes = baseTypes.filter(tpes => tpes.startsWith(variant))
    relevantTypes.map(baseType =>
      val existentialBaseTypes = baseType.drop(variant.size)
      ctx.instantiateTparams(tparams, existentialBaseTypes.toList)
      val monoBlockLit: Block.BlockLit = BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))
      (ctx.funNames(id, baseType), monoBlockLit)
    ).toList

def monomorphize(opt: Option[Stmt])(using ctx: MonoContext)(using Context, DeclarationContext): Option[Stmt] = opt match
  case None => None
  case Some(stmt) => Some(monomorphize(stmt))

def monomorphize(expr: Expr)(using ctx: MonoContext)(using Context, DeclarationContext): Expr = expr match
  case Literal(value, annotatedType) =>
    Literal(value, monomorphize(annotatedType))
  case PureApp(b, targs, vargs) =>
    PureApp(b, targs map monomorphize, vargs map monomorphize)
  case Make(data, tag, targs, vargs) =>
    val combinedTargs = data.targs ++ targs
    val replacementTag = replacementFun(tag, combinedTargs)
    Make(replacementData(data.name, data.targs), replacementTag, List.empty, vargs map monomorphize)
  case Box(b, annotatedCapture) =>
    Box(monomorphize(b), annotatedCapture)
  case ValueVar(id, annotatedType) =>
    ValueVar(id, monomorphize(annotatedType))

def monomorphize(valueParam: ValueParam)(using MonoContext, DeclarationContext): ValueParam = valueParam match
  case ValueParam(id, tpe) => ValueParam(id, monomorphize(tpe))

def monomorphize(blockParam: BlockParam)(using MonoContext, DeclarationContext): BlockParam = blockParam match
  case BlockParam(id, tpe, capt) =>
    BlockParam(id, monomorphize(tpe), capt)

def monomorphize(blockType: BlockType)(using ctx: MonoContext)(using DeclarationContext): BlockType = blockType match {
  case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
    BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
  case BlockType.Interface(name, targs) =>
    val funName = ctx.funNames.getOrElse((name, (targs map toTypeArg).toVector), name)
    // Special case here if we have 'Resume' or 'Prompt' we didn't change the name which we can detect here
    // then we don't change the targs for typechecking to work
    if (funName == name) {
      BlockType.Interface(funName, targs map monomorphize)
    } else {
      BlockType.Interface(funName, List.empty)
    }
}

def monomorphize(valueType: ValueType)(using ctx: MonoContext)(using dctx: DeclarationContext): ValueType = valueType match {
  case ValueType.Var(name) => ctx.replacementTparams(name) match {
    case TypeArg.Base(tpe, targs) => replacementData(tpe, targs.toVector)
    case TypeArg.Boxed(tpe, capt) => ValueType.Boxed(monomorphize(tpe), capt)
  }
  // We do not monomorphize targs here, because our name lookup for types is looking for
  // Option[Option[Int]] -> Option_Option_Int
  // and not
  // Option[Option_Int] -> Option_Option_Int
  case ValueType.Data(name, targs) => replacementData(name, targs)
  case ValueType.Boxed(tpe, capt) => ValueType.Boxed(monomorphize(tpe), capt)
}

def monomorphize(typeArg: TypeArg)(using MonoContext)(using dctx: DeclarationContext): ValueType = typeArg match {
  case TypeArg.Base(tpe, targs) =>
    dctx.findExternData(tpe) match {
      case Some(_) => {
        ValueType.Data(tpe, targs map monomorphize)
      }
      case None => {
        replacementData(tpe, targs.toVector)
      }
    }
  case TypeArg.Boxed(tpe, capt) => ValueType.Boxed(monomorphize(tpe), capt)
  case TypeArg.Var(funId, pos) =>
    // FIXME: Do we want to reflect this unreachability in the Data structure used for monomorphizing?
    //        we would need another version of TypeArg that disallows targs in Base to be anything other than Ground
    throw new RuntimeException(s"All the vars should have been removed in the solving stage, still got '${typeArg}'")
}

def freshMonoTypeName(dataName: Id, tpes: Vector[Ground], monoTypeNames: MonoTpeNames): ValueType.Data = {
  monoTypeNames.getOrElse((dataName, tpes), {
    val nameBuilder = StringBuilder(dataName.name.name)
    val valueTypes = tpes map {
      case TypeArg.Base(tpe, targs) => {
        // Safe `get`, because we are handling Vector[Ground] and just re-establishing this invariant,
        // because our types do not guarantee this
        val filteredTargs = filterNonGround(targs.toVector).get
        val innerData = freshMonoTypeName(tpe, filteredTargs, monoTypeNames)
        nameBuilder.append("_" + innerData.name.name.name)
        innerData
      }
      case TypeArg.Boxed(tpe, capt) => {
        ValueType.Boxed(tpe, capt)
      }
    }
    val freshId = Id(nameBuilder.toString())
    val monoData: ValueType.Data = ValueType.Data(freshId, List.empty)
    monoTypeNames += ((dataName, tpes) -> monoData)
    monoData
  })
}

def freshMonoName(baseId: Id, tpes: Vector[Ground]): Id = {
  if (tpes.isEmpty) return baseId

  // Keep the ids of 'Resume' and 'Prompt', so we can detect this case and make typechecking work later
  // also see monomorphize(blockType: BlockType)
  if (baseId == core.Type.ResumeSymbol || baseId == core.Type.PromptSymbol) return baseId

  val tpesString = tpes.map({
    case TypeArg.Base(tpe, targs) => tpe.name.name
    // TODO: Fix naming
    case TypeArg.Boxed(tpe, capt) => "BOXED"
  }).mkString
  Id(baseId.name.name + tpesString)
}

def replacementFun(id: FunctionId, targs: List[ValueType])(using ctx: MonoContext): FunctionId = {
  if (targs.isEmpty) return id
  val baseTypes: Vector[Ground] = (targs map toTypeArg).toVector
  ctx.funNames(id, baseTypes)
}

def replacementData(id: Id, targs: Vector[TypeArg])(using ctx: MonoContext, dctx: DeclarationContext): ValueType.Data = {
  if (targs.isEmpty) return ValueType.Data(id, List.empty)

  val groundTpes = filterNonGround(targs).get
  dctx.findExternData(id) match {
    case Some(_) => ValueType.Data(id, targs.toList map monomorphize)
    case None => ctx.tpeNames((id, groundTpes))
  }
}

def replacementData(id: Id, targs: List[ValueType])(using ctx: MonoContext, dctx: DeclarationContext): ValueType.Data = {
  dctx.findExternData(id) match {
    case Some(_) => {
      ValueType.Data(id, targs map monomorphize)
    }
    case None => {
      val baseTypes: Vector[Ground] = (targs map toTypeArg).toVector
      replacementData(id, baseTypes)
    }
  }
}

def toTypeArg(vt: ValueType)(using ctx: MonoContext): Ground = vt match {
  case ValueType.Data(name, targs) => TypeArg.Base(name, targs map toTypeArg)
  case ValueType.Var(name) => ctx.replacementTparams(name)
  case ValueType.Boxed(tpe, capt) => TypeArg.Boxed(tpe, capt)
}
