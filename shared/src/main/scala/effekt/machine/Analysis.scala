package effekt
package machine

import effekt.context.Context
import effekt.symbols.{ Symbol, UserEffect, ValueSymbol, BlockSymbol, Name, Module, builtins, / }
import effekt.util.{ Task, control }
import effekt.util.control._

object Analysis {

  /**
   * Analysis on terms
   */

  def freeVars(stmt: Stmt): Set[Var] = stmt match {
    case Let(name, expr, rest) =>
      freeVars(expr) ++ freeVars(rest).filterNot(_.id == name)
    case Def(name, block, rest) =>
      freeVars(block) ++ freeVars(rest)
    case PushFrame(_, _, args, rest) =>
      args.flatMap(freeVars).toSet ++ freeVars(rest)
    case PushStack(stack, rest) =>
      freeVars(stack) ++ freeVars(rest)
    case PopStack(stackName, rest) =>
      freeVars(rest).filterNot(_.id == stackName)
    case CopyStack(stackName, stack, rest) =>
      freeVars(stack) ++ freeVars(rest).filterNot(_.id == stackName)
    case EraseStack(stack, rest) =>
      freeVars(stack) ++ freeVars(rest)
    case Ret(values) =>
      values.flatMap(freeVars(_)).toSet
    case Jump(_, args) =>
      args.flatMap(freeVars).toSet
    case If(cond, thenBlock, thenArgs, elseBlock, elseArgs) =>
      freeVars(cond) ++ thenArgs.flatMap(freeVars) ++ elseArgs.flatMap(freeVars)
  }

  def freeVars(arg: Arg): Set[Var] = arg match {
    case expr: Expr   => freeVars(expr)
    case value: Value => freeVars(value)
  }

  def freeVars(expr: Expr): Set[Var] = expr match {
    case AppPrim(_, _, args)      => args.flatMap(freeVars).toSet
    case NewStack(_, block, args) => args.flatMap(freeVars).toSet ++ freeVars(block)
    case EviPlus(l, r)            => freeVars(l) ++ freeVars(r)
    case EviDecr(l)               => freeVars(l)
    case EviIsZero(l)             => freeVars(l)
  }

  def freeVars(value: Value): Set[Var] = value match {
    case v: Var        => Set(v)
    case i: IntLit     => Set()
    case b: BooleanLit => Set()
    case u: UnitLit    => Set()
    case l: EviLit     => Set()
  }

  def freeVars(block: Block): Set[Var] = block match {
    case BlockLit(params, body) =>
      freeVars(body).filterNot(v => params.exists(param => v.id == param.id))
    case BlockVar(_) =>
      Set()
  }

  def substitute(mapping: Map[Symbol, Value], stmt: Stmt): Stmt = stmt match {
    case Let(x, expr, rest) =>
      Let(x, substitute(mapping, expr), substitute(mapping, rest))
    case Def(blockName, BlockLit(params, body), rest) =>
      Def(blockName, BlockLit(params, substitute(mapping, body)), substitute(mapping, rest))
    case PushFrame(cntType, block, blockArgs, rest) =>
      PushFrame(cntType, substitute(mapping, block), blockArgs.map(substitute(mapping, _)), substitute(mapping, rest))
    case PushStack(stack, rest) =>
      PushStack(substitute(mapping, stack), substitute(mapping, rest))
    case PopStack(stackName, rest) =>
      PopStack(stackName, substitute(mapping, rest))
    case CopyStack(stackName, stack, rest) =>
      CopyStack(stackName, substitute(mapping, stack), substitute(mapping, rest))
    case EraseStack(stack, rest) =>
      EraseStack(substitute(mapping, stack), substitute(mapping, rest))
    case Ret(values) =>
      Ret(values.map(substitute(mapping, _)))
    case Jump(block, blockArgs) =>
      Jump(substitute(mapping, block), blockArgs.map(substitute(mapping, _)))
    case If(cond, thenBlock, thenArgs, elseBlock, elseArgs) =>
      If(
        cond,
        substitute(mapping, thenBlock), thenArgs.map(substitute(mapping, _)),
        substitute(mapping, elseBlock), elseArgs.map(substitute(mapping, _))
      )
  }

  def substitute(mapping: Map[Symbol, Value], block: Block): Block = block match {
    case BlockLit(params, body) => BlockLit(params, substitute(mapping, body))
    case BlockVar(id)           => BlockVar(id)
  }

  def substitute(mapping: Map[Symbol, Value], arg: Arg): Arg = arg match {
    case expr: Expr =>
      substitute(mapping, expr)
    case value: Value =>
      substitute(mapping, value)
  }

  def substitute(mapping: Map[Symbol, Value], expr: Expr): Expr = expr match {
    case AppPrim(typ, blockName, args) =>
      AppPrim(typ, blockName, args.map(substitute(mapping, _)))
    case NewStack(typ, block, args) =>
      NewStack(typ, substitute(mapping, block), args.map(substitute(mapping, _)))
    case EviPlus(l, r) =>
      EviPlus(substitute(mapping, l), substitute(mapping, r))
    case EviDecr(l) =>
      EviDecr(substitute(mapping, l))
    case EviIsZero(l) =>
      EviIsZero(substitute(mapping, l))
  }

  def substitute(mapping: Map[Symbol, Value], value: Value): Value = value match {
    case IntLit(n)      => IntLit(n)
    case BooleanLit(b)  => BooleanLit(b)
    case UnitLit()      => UnitLit()
    case Var(typ, name) => mapping.getOrElse(name, Var(typ, name))
    case EviLit(n)      => EviLit(n)
  }

  // TODO parameterlift expressions
  // TODO this only works on ANF
  def parameterLift(stmt: Stmt)(implicit C: Context): Stmt = stmt match {
    case Let(x, e, rest) =>
      // TODO parameterLift blocks in NewStack
      Let(x, e, parameterLift(rest))
    case Def(name, BlockLit(params, body), rest) =>
      val vars = freeVars(BlockLit(params, body)).toList
      val freshVars = vars.map(v =>
        Var(v.typ, FreshValueSymbol(v.id.name.localName, C.module)));
      val freshParams = freshVars.map { v => Param(v.typ, v.id) }
      val mapping = vars.map(_.id).zip(freshVars).toMap;
      Def(name, BlockLit(
        params ++ freshParams,
        parameterLift(substitute(mapping, addArguments(name, vars, body)))
      ),
        parameterLift(addArguments(name, vars, rest)))
    case PushFrame(cntType, block, blockArgs, rest) =>
      // TODO parameterlift blocks
      PushFrame(cntType, block, blockArgs, parameterLift(rest))
    case PushStack(stack, rest) =>
      PushStack(stack, parameterLift(rest))
    case PopStack(stackName, rest) =>
      PopStack(stackName, parameterLift(rest))
    case CopyStack(stackName, stack, rest) =>
      CopyStack(stackName, stack, parameterLift(rest))
    case EraseStack(stack, rest) =>
      EraseStack(stack, parameterLift(rest))
    case Ret(expr) =>
      Ret(expr)
    case Jump(block, args) =>
      // TODO parameterlift blocks
      Jump(block, args)
    case If(cond, thenBlock, thenArgs, elseBlock, elseArgs) =>
      // TODO parameterlift blocks
      If(cond, thenBlock, thenArgs, elseBlock, elseArgs)
  }

  // TODO this only works correctly on ANF as we don't go into expressions
  def addArguments(name: BlockSymbol, args: List[Var], stmt: Stmt): Stmt =
    stmt match {
      case Let(x, AppPrim(typ, func, funcArgs), rest) =>
        Let(x, AppPrim(typ, func, funcArgs), addArguments(name, args, rest))
      case Let(stackName, NewStack(typ, BlockVar(blockName), blockArgs), rest) =>
        val newArgs = if (blockName == name) { args } else { List() };
        Let(stackName, NewStack(typ, BlockVar(blockName), blockArgs ++ newArgs), addArguments(name, args, rest))
      case Let(stackName, NewStack(typ, BlockLit(params, body), blockArgs), rest) =>
        Let(stackName, NewStack(typ, BlockLit(params, addArguments(name, args, body)), blockArgs), addArguments(name, args, rest))
      case Let(x, expr, rest) =>
        Let(x, expr, addArguments(name, args, rest))
      case Def(blockName, BlockLit(params, body), rest) =>
        Def(blockName, BlockLit(
          params,
          addArguments(name, args, body)
        ),
          addArguments(name, args, rest))
      case PushFrame(cntType, BlockVar(blockName), blockArgs, rest) =>
        val newArgs = if (blockName == name) { args } else { List() };
        PushFrame(cntType, BlockVar(blockName), blockArgs ++ newArgs, addArguments(name, args, rest))
      case PushFrame(cntType, BlockLit(params, body), blockArgs, rest) =>
        PushFrame(cntType, BlockLit(params, addArguments(name, args, body)), blockArgs, addArguments(name, args, rest))
      case PushStack(stack, rest) =>
        PushStack(stack, addArguments(name, args, rest))
      case PopStack(stackName, rest) =>
        PopStack(stackName, addArguments(name, args, rest))
      case CopyStack(stackName, stack, rest) =>
        CopyStack(stackName, stack, addArguments(name, args, rest))
      case EraseStack(stack, rest) =>
        EraseStack(stack, addArguments(name, args, rest))
      case Ret(expr) =>
        Ret(expr)
      case Jump(BlockVar(blockName), blockArgs) =>
        val newArgs = if (blockName == name) { args } else { List() };
        Jump(BlockVar(blockName), blockArgs ++ newArgs)
      case Jump(BlockLit(params, body), blockArgs) =>
        Jump(BlockLit(params, addArguments(name, args, body)), blockArgs)
      case If(cond, thenBlock, thenArgs, elseBlock, elseArgs) =>
        val newThenArgs = thenBlock match {
          case BlockVar(thenBlockName) if (thenBlockName == name) => args
          case BlockVar(_) => List()
          case BlockLit(_, _) => List()
        };
        val newElseArgs = elseBlock match {
          case BlockVar(elseBlockName) if (elseBlockName == name) => args
          case BlockVar(_) => List()
          case BlockLit(_, _) => List()
        };
        val newThenBlock = thenBlock match {
          case BlockVar(id)           => BlockVar(id)
          case BlockLit(params, body) => BlockLit(params, addArguments(name, args, body))
        };
        val newElseBlock = elseBlock match {
          case BlockVar(id)           => BlockVar(id)
          case BlockLit(params, body) => BlockLit(params, addArguments(name, args, body))

        };
        If(
          cond,
          newThenBlock, thenArgs ++ newThenArgs,
          newElseBlock, elseArgs ++ newElseArgs
        )
    }

  def blockFloat(stmt: Stmt): (Map[BlockSymbol, BlockLit], Stmt) = stmt match {
    case Let(x, e, rest) =>
      val (defs, nakedRest) = blockFloat(rest);
      (defs, Let(x, e, nakedRest))
    case Def(name, BlockLit(params, body), rest) =>
      val (blockDefs, nakedBody) = blockFloat(body);
      val (restDefs, nakedRest) = blockFloat(rest);
      val defs = Map(name -> BlockLit(params, nakedBody)) ++ blockDefs ++ restDefs;
      (defs, nakedRest)
    case PushFrame(cntType, blockName, blockArgs, rest) =>
      val (defs, nakedRest) = blockFloat(rest);
      (defs, (PushFrame(cntType, blockName, blockArgs, nakedRest)))
    case PushStack(stack, rest) =>
      val (defs, nakedRest) = blockFloat(rest);
      (defs, (PushStack(stack, nakedRest)))
    case PopStack(stackName, rest) =>
      val (defs, nakedRest) = blockFloat(rest);
      (defs, (PopStack(stackName, nakedRest)))
    case CopyStack(stackName, stack, rest) =>
      val (defs, nakedRest) = blockFloat(rest);
      (defs, (CopyStack(stackName, stack, nakedRest)))
    case EraseStack(stack, rest) =>
      val (defs, nakedRest) = blockFloat(rest);
      (defs, (EraseStack(stack, nakedRest)))
    case Ret(expr) =>
      (Map(), Ret(expr))
    case Jump(blockName, args) =>
      (Map(), Jump(blockName, args))
    case If(cond, thenBlockName, thenArgs, elseBlockName, elseArgs) =>
      (Map(), If(cond, thenBlockName, thenArgs, elseBlockName, elseArgs))
  }

  // TODO this only works correctly on ANF and KNF
  def jumpTargets(stmt: Stmt): Map[BlockSymbol, List[Value]] =
    stmt match {
      case Let(_, _, rest) =>
        jumpTargets(rest)
      case Def(_, _, rest) =>
        jumpTargets(rest)
      case PushFrame(_, _, _, rest) =>
        jumpTargets(rest)
      case PushStack(_, rest) =>
        jumpTargets(rest)
      case PopStack(_, rest) =>
        jumpTargets(rest)
      case CopyStack(_, _, rest) =>
        jumpTargets(rest)
      case EraseStack(_, rest) =>
        jumpTargets(rest)
      case Ret(_) =>
        Map()
      case Jump(BlockVar(targetName), args) =>
        Map(targetName -> args.asInstanceOf[List[Value]])
      case Jump(BlockLit(_, _), _) =>
        // TODO this only works correctly on KNF
        Map()
      case If(_, BlockVar(thenName), thenArgs, BlockVar(elseName), elseArgs) =>
        // TODO what if both branches goto the same block?
        Map(thenName -> thenArgs) ++ Map(elseName -> elseArgs)
      case If(_, _, _, _, _) =>
        // TODO this only works correctly on KNF
        Map()
    }

  def reachableBasicBlocks(entryBlockName: BlockSymbol, basicBlocks: Map[BlockSymbol, BlockLit]): Map[BlockSymbol, BlockLit] = {

    var reachableBlocksSet: Set[BlockSymbol] = Set();
    def go(blockName: BlockSymbol): Unit = {
      if (!reachableBlocksSet.contains(blockName)) {
        reachableBlocksSet += blockName;
        if (basicBlocks.isDefinedAt(blockName)) {
          jumpTargets(basicBlocks(blockName).body).keys.foreach(go);
        }
      }
    };
    go(entryBlockName);

    val reachableBasicBlocks = basicBlocks.view.filterKeys { blockName =>
      reachableBlocksSet.contains(blockName)
    }.toMap

    reachableBasicBlocks
  }

  type Arity = Int

  def findFrameDefs(stmt: Stmt): Map[BlockSymbol, Arity] =
    stmt match {
      case Let(_, _, rest) =>
        findFrameDefs(rest)
      case PushFrame(cntType, BlockVar(name), env, rest) =>
        Map(name -> (cntType.length - env.length)) ++ findFrameDefs(rest)
      case PushFrame(_, BlockLit(_, _), _, _) =>
        // TODO this only works correctly on KNF
        Map()
      case PushStack(_, rest) =>
        findFrameDefs(rest)
      case PopStack(_, rest) =>
        findFrameDefs(rest)
      case CopyStack(_, _, rest) =>
        findFrameDefs(rest)
      case EraseStack(_, rest) =>
        findFrameDefs(rest)
      case Def(_, BlockLit(_, body), rest) =>
        findFrameDefs(body) ++ findFrameDefs(rest)
      case Ret(_) =>
        Map()
      case Jump(_, _) =>
        Map()
      case If(_, _, _, _, _) =>
        Map()
    }

  // TODO this only works on KNF
  def findClosureDefs(stmt: Stmt): Map[BlockSymbol, Arity] =
    stmt match {
      case Let(_, NewStack(Stack(cntType), BlockVar(name), env), rest) =>
        Map(name -> (cntType.length - env.length)) ++ findClosureDefs(rest)
      case Let(_, _, rest) =>
        findClosureDefs(rest)
      case PushFrame(cntType, name, env, rest) =>
        findClosureDefs(rest)
      case PushStack(_, rest) =>
        findClosureDefs(rest)
      case PopStack(_, rest) =>
        findClosureDefs(rest)
      case CopyStack(_, _, rest) =>
        findClosureDefs(rest)
      case EraseStack(_, rest) =>
        findClosureDefs(rest)
      case Def(_, BlockLit(_, body), rest) =>
        findClosureDefs(body) ++ findClosureDefs(rest)
      case Ret(_) =>
        Map()
      case Jump(_, _) =>
        Map()
      case If(_, _, _, _, _) =>
        Map()
    }

  /**
   * Let insertion
   */

  def anormalForm(stmt: Stmt)(implicit C: Context): Stmt = stmt match {
    case Let(name, expr, rest) => Run {
      anormalForm(expr).map(e => Let(name, e, anormalForm(rest)))
    }
    case Def(name, BlockLit(params, body), rest) =>
      Def(name, BlockLit(params, anormalForm(body)), anormalForm(rest))
    case PushFrame(typ, block, args, rest) => Run {
      anormalForm(block).map(b =>
        PushFrame(typ, b, args, anormalForm(rest)))
    }
    case PushStack(stack, rest) => Run {
      anormalForm(stack).map(v => PushStack(v, anormalForm(rest)))
    }
    case PopStack(stackName, rest) =>
      PopStack(stackName, anormalForm(rest))
    case CopyStack(stackName, stack, rest) => Run {
      anormalForm(stack).map(v => CopyStack(stackName, v, anormalForm(rest)))
    }
    case EraseStack(stack, rest) => Run {
      anormalForm(stack).map(v => EraseStack(v, anormalForm(rest)))
    }
    case Ret(args) => Run {
      sequence(args.map(anormalForm)).map(vs => Ret(vs))
    }
    case Jump(block, args) => Run {
      anormalForm(block).flatMap(b => sequence(args.map(anormalForm)).map(vs => Jump(b, vs)))
    }
    case If(cond, thenBlock, thenArgs, elseBlock, elseArgs) => Run {
      anormalForm(thenBlock).flatMap(tb =>
        anormalForm(elseBlock).flatMap(eb =>
          anormalForm(cond).map(v =>
            If(v, tb, thenArgs, eb, elseArgs))))
    }
  }

  def anormalForm(expr: Expr)(implicit C: Context): Control[Expr] = expr match {
    case AppPrim(typ, func, args) =>
      sequence(args.map(anormalForm)).map(vs => AppPrim(typ, func, vs))
    case NewStack(typ, block, args) =>
      // TODO add to blockParamsSet?
      anormalForm(block).map(b => NewStack(typ, b, args))
    case EviPlus(l, r) =>
      anormalForm(l).flatMap(x => anormalForm(r).map(y => EviPlus(x, y)))
    case EviDecr(l) =>
      anormalForm(l).map(x => EviDecr(x))
    case EviIsZero(l) =>
      anormalForm(l).map(x => EviIsZero(x))
  }

  def anormalForm(arg: Arg)(implicit C: Context): Control[Value] = arg match {
    case expr: Expr =>
      for {
        e <- anormalForm(expr)
        v <- bindingValue(e)
      } yield v
    case value: Value =>
      pure(value)
  }

  def anormalForm(block: Block)(implicit C: Context): Control[Block] = block match {
    case BlockLit(params, body) =>
      bindingBlock(BlockLit(params, anormalForm(body))).map(f => BlockVar(f))
    case BlockVar(id) =>
      pure(BlockVar(id))
  }

  def bindingValue(expr: Expr)(implicit C: Context): Control[Value] = expr match {
    case _: AppPrim =>
      control.use(delimiter) { resume =>
        val x = FreshValueSymbol("x", C.module)
        resume.apply(Var(expr.typ, x)).map(rest => Let(x, expr, rest))
      }
    case _: NewStack =>
      control.use(delimiter) { resume =>
        val k = FreshBlockSymbol("k", C.module)
        resume.apply(Var(expr.typ, k)).map(rest => Let(k, expr, rest))
      }
    // TODO deduplicate the following three
    case _: EviPlus =>
      control.use(delimiter) { resume =>
        val l = FreshValueSymbol("l", C.module)
        resume.apply(Var(Evidence(), l)).map(rest => Let(l, expr, rest))
      }
    case _: EviDecr =>
      control.use(delimiter) { resume =>
        val l = FreshValueSymbol("l", C.module)
        resume.apply(Var(Evidence(), l)).map(rest => Let(l, expr, rest))
      }
    case _: EviIsZero =>
      control.use(delimiter) { resume =>
        val x = FreshValueSymbol("x", C.module)
        resume.apply(Var(PrimBoolean(), x)).map(rest => Let(x, expr, rest))
      }

  }

  def bindingBlock(block: BlockLit)(implicit C: Context): Control[BlockSymbol] =
    control.use(delimiter) { resume =>
      block match {
        case BlockLit(params, body) =>
          val f = FreshBlockSymbol("f", C.module);
          resume.apply(f).map(rest => Def(f, BlockLit(params, body), rest))
      }
    }

  // TODO this only works on ANF and lambda lifted
  def linearize(block: BlockLit)(implicit C: Context): BlockLit = block match {
    case BlockLit(params, body) =>
      val vars = freeVars(body);
      BlockLit(params, Run {
        sequence(params.map(param => perhapsErase(vars, param))).map(_ =>
          linearize(body))
      })
  }

  // TODO this only works on ANF, KNF and lambda lifted form
  def linearize(stmt: Stmt)(implicit C: Context): Stmt = stmt match {
    case Let(stackName, NewStack(typ, block, args), rest) =>
      val vars = freeVars(rest);
      Run {
        sequence(args.map(arg => perhapsCopy(vars, arg))).map(newArgs =>
          Let(stackName, NewStack(typ, block, newArgs), Run {
            perhapsErase(vars, Param(typ, stackName)).map(_ =>
              linearize(rest))
          }))
      }
    case Let(name, AppPrim(typ, blockName, args), rest) =>
      // TODO copy block args?
      Let(name, AppPrim(typ, blockName, args), linearize(rest))
    case Let(name, expr, rest) =>
      Let(name, expr, linearize(rest))
    case Def(name, block, rest) =>
      Def(name, linearize(block), linearize(rest))
    case PushFrame(cntType, block, args, rest) =>
      val vars = freeVars(rest);
      Run {
        sequence(args.map(arg => perhapsCopy(vars, arg))).map(newArgs =>
          PushFrame(cntType, block, newArgs, linearize(rest)))
      }
    case PushStack(stack, rest) =>
      val vars = freeVars(rest);
      Run {
        perhapsCopy(vars, stack).map(newStack =>
          PushStack(newStack, linearize(rest)))
      }
    case PopStack(stackName, rest) =>
      val vars = freeVars(rest);
      // TODO find actual stack type of popped stack
      PopStack(stackName, Run {
        perhapsErase(vars, Param(Stack(List()), stackName)).map(_ => linearize(rest))
      })
    case CopyStack(stackName, stack, rest) =>
      // TODO deal with this case
      C.abort("Internal error: linearizing statement with copyStack in it")
    case EraseStack(stack, rest) =>
      // TODO deal with this case
      C.abort("Internal error: linearizing statement with eraseStack in it")
    case Ret(values) =>
      Ret(values)
    case Jump(block, args) =>
      Jump(block, args)
    case If(cond, thenBlock, thenArgs, elseBlock, elseArgs) =>
      // TODO this only works because the two blocks don't have any actual parameters
      val thenBlockName = FreshBlockSymbol("f", C.module);
      val elseBlockName = FreshBlockSymbol("f", C.module);
      val args = (thenArgs.toSet ++ elseArgs.toSet).toList;
      val freshThenVars = args.map {
        case v: Var =>
          Var(v.typ, FreshValueSymbol(v.id.name.localName, C.module))
        case _ =>
          C.abort("Internal error: linearize If non-var argument")
      };
      val freshElseVars = args.map {
        case v: Var =>
          Var(v.typ, FreshValueSymbol(v.id.name.localName, C.module))
        case _ =>
          C.abort("Internal error: linearize If non-var argument")
      };
      val thenArgsIndices = thenArgs.map(args.indexOf(_));
      val elseArgsIndices = elseArgs.map(args.indexOf(_));
      val freshThenArgs = thenArgsIndices.map(freshThenVars.apply(_));
      val freshElseArgs = elseArgsIndices.map(freshElseVars.apply(_));
      val freshThenParams = freshThenVars.map { v => Param(v.typ, v.id) };
      val freshElseParams = freshElseVars.map { v => Param(v.typ, v.id) };
      Def(thenBlockName, linearize(BlockLit(freshThenParams, Jump(thenBlock, freshThenArgs))),
        Def(elseBlockName, linearize(BlockLit(freshElseParams, Jump(elseBlock, freshElseArgs))),
          If(cond, BlockVar(thenBlockName), args, BlockVar(elseBlockName), args)))
  }

  def perhapsCopy(vars: Set[Var], arg: Arg)(implicit C: Context): Control[Value] = arg match {
    case Var(Stack(typ), name) if (vars.exists { case Var(_, varName) => name == varName }) =>
      control.use(delimiter) { resume =>
        val newName = machine.FreshBlockSymbol(name.name.localName, C.module);
        resume.apply(Var(Stack(typ), newName)).map(rest =>
          CopyStack(newName, Var(Stack(typ), name), rest))
      }
    case value: Value => pure(value)
    case _            => C.abort("Internal error: perhapsCopy non-value argument")
  }

  def perhapsErase(vars: Set[Var], param: Param)(implicit C: Context): Control[Unit] = param match {
    case Param(Stack(typ), name) if (!vars.exists { case Var(_, varName) => name == varName }) =>
      control.use(delimiter) { resume =>
        resume.apply(()).map(rest =>
          EraseStack(Var(Stack(typ), name), rest))
      }
    case _ => pure(())
  }

  private val delimiter: Cap[Stmt] = new Capability { type Res = Stmt }

  def Run(e: Control[Stmt]): Stmt = control.handle(delimiter)(e).run()

}
