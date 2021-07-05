package effekt
package llvm

import scala.collection.mutable
import effekt.machine.Analysis._
import effekt.context.Context
import effekt.context.assertions.SymbolAssertions
import effekt.machine.FreshValueSymbol
import effekt.symbols.{ /, BlockSymbol, Module, Name, Symbol, UserEffect, ValueSymbol, builtins }
import effekt.util.{ Task, control }
import effekt.util.control._

object LLVMTransformer {

  def transform(mod: machine.ModuleDecl)(implicit C: LLVMTransformerContext): List[Top] =
    mod match {
      case machine.ModuleDecl(decls, prog) => decls.map(transform) ++ transformToplevel(prog)
    }

  def transform(decl: machine.Decl)(implicit C: LLVMTransformerContext): Top =
    decl match {
      case machine.DefPrim(typ, functionName, params, body) =>
        DefFun(typ, functionName, params, body)
      case machine.Include(content) =>
        Include(content)
    }

  def transformToplevel(stmt: machine.Stmt)(implicit C: LLVMTransformerContext): List[Top] =
    stmt match {
      case machine.Def(functionName, machine.BlockLit(params, body), rest) => {

        val normalBody = anormalForm(body);
        val liftedBody = parameterLift(normalBody);
        val linearBody = linearize(liftedBody);
        val (localDefs, entry) = blockFloat(linearBody);

        val frameDefs = {
          findFrameDefs(normalBody).map {
            case (blockName, arity) =>

              val basicBlocks = reachableBasicBlocks(blockName, localDefs);
              val phiInstructionsMap = findPhiInstructions(basicBlocks);

              val params = localDefs(blockName).params;

              DefFrm(blockName, params.take(arity), params.drop(arity), blockName, basicBlocks.toList.map {
                case (blockName, machine.BlockLit(_, blockBody)) =>
                  val (instructions, terminator) = transform(blockBody, localDefs.keySet);
                  BasicBlock(
                    blockName,
                    phiInstructionsMap(blockName) ++ instructions,
                    terminator
                  )
              })
          }
        };

        val closureDefs = {
          findClosureDefs(normalBody).map {
            case (blockName, arity) =>

              val basicBlocks = reachableBasicBlocks(blockName, localDefs);
              val phiInstructionsMap = findPhiInstructions(basicBlocks);

              val params = localDefs(blockName).params;

              DefClo(blockName, params.take(arity), params.drop(arity), blockName, basicBlocks.toList.map {
                case (blockName, machine.BlockLit(_, blockBody)) =>
                  val (instructions, terminator) = transform(blockBody, localDefs.keySet);
                  BasicBlock(
                    blockName,
                    phiInstructionsMap(blockName) ++ instructions,
                    terminator
                  )
              })
          }
        };

        val thisDef = {

          // TODO is this creating of entry block necessary?
          val entryBlockName: BlockSymbol = machine.FreshBlockSymbol("entry", C.module);
          val entryBlock = machine.BlockLit(params, entry);
          val thisLocalDefs = localDefs + (entryBlockName -> entryBlock);

          val basicBlocks = reachableBasicBlocks(entryBlockName, thisLocalDefs);
          val phiInstructionsMap = findPhiInstructions(basicBlocks);

          DefCnt(functionName, params, entryBlockName, basicBlocks.toList.map {
            case (blockName, machine.BlockLit(_, blockBody)) =>
              val (instructions, terminator) = transform(blockBody, thisLocalDefs.keySet);
              BasicBlock(
                blockName,
                phiInstructionsMap(blockName) ++ instructions,
                terminator
              )
          })
        }

        thisDef :: frameDefs.toList ++ closureDefs.toList ++ transformToplevel(rest)

      }
      case machine.Ret(List()) =>
        // TODO define main function here
        List()
      case _ =>
        C.abort("unsupported " + stmt)
    }

  def transform(body: machine.Stmt, localBlocks: Set[BlockSymbol])(implicit C: LLVMTransformerContext): (List[Instruction], Terminator) = body match {
    case machine.Let(id: ValueSymbol, machine.Construct(typ, args), rest) => {
      val (instructions, terminator) = transform(rest, localBlocks)
      // ToDo: refactor this
      if (args.isEmpty) {
        (InsertValues(id, machine.Record(List()), args.map(transform)) :: instructions, terminator)
      } else {
        (InsertValues(id, typ.asInstanceOf[machine.Record], args.map(transform)) :: instructions, terminator)
      }
    }
    case machine.Let(id: ValueSymbol, machine.Select(_, target, field), rest) => {
      val (instructions, terminator) = transform(rest, localBlocks)
      (ExtractValue(id, transform(target), field) :: instructions, terminator)
    }
    case machine.Let(id: ValueSymbol, machine.Inject(typ, arg, variant), rest) => {
      val (instructions, terminator) = transform(rest, localBlocks)
      (Inject(id, typ.asInstanceOf[machine.Variant], transform(arg), variant) :: instructions, terminator)
    }
    case machine.Let(id: ValueSymbol, machine.Reject(typ, arg, variant), rest) => {
      val (instructions, terminator) = transform(rest, localBlocks)
      (ExtractValue(id, transform(arg), variant + 1) :: instructions, terminator)
    }
    case machine.Let(id: ValueSymbol, machine.AppPrim(typ, func, args), rest) => {
      val (instructions, terminator) = transform(rest, localBlocks);
      (Call(id, typ, func, args.map(transform)) :: instructions, terminator)
    }
    case machine.Let(id: BlockSymbol, machine.NewStack(machine.Stack(cntType), block, args), rest) => {
      val (instructions, terminator) = transform(rest, localBlocks);
      (NewStack(cntType, id, transform(block), args) :: instructions, terminator)
    }
    case machine.Let(id: ValueSymbol, machine.EviPlus(l, r), rest) => {
      val (instructions, terminator) = transform(rest, localBlocks);
      (EviPlus(id, transform(l), transform(r)) :: instructions, terminator)
    }
    case machine.Let(id: ValueSymbol, machine.EviDecr(l), rest) => {
      val (instructions, terminator) = transform(rest, localBlocks);
      (EviDecr(id, transform(l)) :: instructions, terminator)
    }
    case machine.Let(id: ValueSymbol, machine.EviIsZero(l), rest) => {
      val (instructions, terminator) = transform(rest, localBlocks);
      (EviIsZero(id, transform(l)) :: instructions, terminator)
    }
    case machine.Let(_, _, _) => {
      println(body);
      C.abort("Internal error: binding value to block symbol or block to value symbol")
    }
    case machine.Def(_, _, _) => {
      C.abort("Internal error: local definition in instructions")
    }
    case machine.PushFrame(cntType, block, args, rest) => {
      val (instructions, terminator) = transform(rest, localBlocks);
      (PushFrame(cntType, transform(block), args) :: instructions, terminator)
    }
    case machine.PushStack(stack, rest) => {
      val (instructions, terminator) = transform(rest, localBlocks);
      (PushStack(transform(stack)) :: instructions, terminator)
    }
    case machine.PopStack(id, rest) => {
      val (instructions, terminator) = transform(rest, localBlocks);
      (PopStack(id) :: instructions, terminator)
    }
    case machine.CopyStack(id, stack, rest) => {
      val (instructions, terminator) = transform(rest, localBlocks);
      (CopyStack(id, transform(stack)) :: instructions, terminator)
    }
    case machine.EraseStack(stack, rest) => {
      val (instructions, terminator) = transform(rest, localBlocks);
      (EraseStack(transform(stack)) :: instructions, terminator)
    }
    case machine.Ret(exprs) => {
      (List(), Ret(exprs.map(transform)))
    }
    case machine.Jump(block, args) => {
      val id = transform(block);
      if (localBlocks.contains(id)) {
        (List(), JumpLocal(id, args.map(transform)))
      } else {
        (List(), Jump(id, args.map(transform)))
      }
    }
    case machine.If(cond, thenBlock, thenArgs, elseBlock, elseArgs) => {
      (List(), If(transform(cond), transform(thenBlock), thenArgs, transform(elseBlock), elseArgs))
    }
    case machine.Match(scrutinee, variant, thenBlock, _, elseBlock, _) => { // ToDo: extract first field from scrutinee
      val tagName = FreshValueSymbol("tag", C.module)
      val instructions = List(ExtractValue(tagName, transform(scrutinee), 0))
      (instructions, Switch(machine.Var(machine.PrimInt(), tagName), transform(elseBlock), List((variant, transform(thenBlock)))))
    }
    case machine.Panic() => {
      (List(), Panic())
    }
  }

  def transform(arg: machine.Arg)(implicit C: LLVMTransformerContext): machine.Value = arg match {
    case expr: machine.Expr =>
      C.abort("Internal error: expression in value position")
    case value: machine.Value =>
      value
  }

  def transform(block: machine.Block)(implicit C: LLVMTransformerContext): BlockSymbol = block match {
    case machine.BlockVar(id) =>
      id
    case _ =>
      C.abort("Internal error: label literal in label variable position")
  }

  def findPhiInstructions(basicBlocks: Map[BlockSymbol, machine.BlockLit]): Map[BlockSymbol, List[Phi]] = {
    import scala.collection.mutable
    type PhiMap = mutable.Map[machine.Param, List[(BlockSymbol, machine.Value)]]
    type PhiDB = mutable.Map[BlockSymbol, PhiMap]
    val phiDB: PhiDB = mutable.Map.empty
    basicBlocks.keys.foreach(callee => phiDB.update(callee, mutable.Map.empty))
    for {
      (caller, machine.BlockLit(_, blockBody)) <- basicBlocks
      (callee, args) <- jumpTargets(blockBody)
      if phiDB.isDefinedAt(callee)
      phiMap = phiDB(callee)
      calleeParams = basicBlocks(callee).params
      (param, arg) <- calleeParams zip args
      callers = phiMap.getOrElse(param, Nil)
    } phiMap.put(param, (caller, arg) :: callers)
    phiDB.map {
      case (callee, phis) => (callee, phis.map {
        case ((param, args)) => Phi(param, args)
      }.toList)
    }.toMap
  }

  /**
   * Extra info in context
   */

  case class LLVMTransformerContext(module: Module, context: Context) {
    context.module = module;
  }

  private implicit def asContext(C: LLVMTransformerContext): Context = C.context
  private implicit def getContext(implicit C: LLVMTransformerContext): Context = C.context
}
