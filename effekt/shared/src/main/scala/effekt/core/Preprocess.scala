package effekt
package core

import effekt.context.Context
import effekt.core.DeBruijn.toDeBruijn
import effekt.core.DeBruijn.addTparams
import effekt.core.optimizer.Deadcode
import effekt.core.optimizer.Normalizer
import effekt.core.optimizer.BindSubexpressions
import effekt.core.Type.functionType
import effekt.core.Free.value


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
object Preprocess extends Phase[CoreTransformed, CoreTransformed] {

    override val phaseName: String = "preprocess"

    override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = {
      input match {
        case CoreTransformed(source, tree, mod, core) => core match {
          case ModuleDecl(path, includes, declarations, externs, definitions, exports) => {
            val mainSymbol = Context.ensureMainExists(mod)
            var transformed = core
            // DeadCodeElimination
            transformed = Deadcode.remove(mainSymbol, transformed)
            // Normalize (no blocklits as callees)
            // transformed = Normalizer.normalize(Set(mainSymbol), transformed, 50)
            // BindSubexpressions (no unbox as callees)
            transformed = BindSubexpressions.transform(transformed)
            // DeadCodeElimination
            transformed = Deadcode.remove(mainSymbol, transformed)

            transformed = preprocess(transformed)
            // println(util.show(transformed))
            Some(CoreTransformed(source, tree, mod, transformed))
          }
        }
      }
    }

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
        var preprocessContext = PreprocessContext()
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
            if(b.tparams.length > 0) {
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

}