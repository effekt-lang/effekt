package effekt
package machine

import scala.collection.mutable
import effekt.context.Context
import effekt.context.assertions.SymbolAssertions
import effekt.core.{ AnyPattern, IgnorePattern, LiteralPattern, TagPattern, ValueParam }
import effekt.symbols.{ /, BlockParam, BlockSymbol, BlockType, BuiltinFunction, CapabilityParam, Module, Name, ResumeParam, Symbol, UserEffect, ValueSymbol, builtins }

case class FreshValueSymbol(baseName: String, module: Module) extends ValueSymbol {
  val name = Name.qualified(baseName, module)
}
case class FreshBlockSymbol(baseName: String, module: Module) extends BlockSymbol {
  val name = Name.qualified(baseName, module)
}

class Transformer {

  def transform(mod: core.ModuleDecl)(implicit C: TransformerContext): ModuleDecl = {
    val core.ModuleDecl(_, _, defs) = mod

    ModuleDecl(transformDecls(defs), transformToplevel(defs))
  }

  def transformDecls(stmt: core.Stmt)(implicit C: TransformerContext): List[Decl] = {
    stmt match {
      case core.Def(blockName, blockType: BlockType, core.Extern(params, body), rest) =>
        DefPrim(transform(blockType.ret.tpe), blockName, params.map(transform), body) :: transformDecls(rest)
      case core.Include(content, rest) =>
        Include(content) :: transformDecls(rest)
      case core.Record(_, _, rest) =>
        // TODO these are for records and capabilities
        // TODO We only support singleton capabilities
        transformDecls(rest)
      case core.Data(_, _, rest) =>
        transformDecls(rest)
      case core.Def(_, _, _, rest) =>
        // TODO expand this catch-all case
        transformDecls(rest)
      case core.Exports(path, symbols) =>
        List()
      case _ =>
        println(stmt)
        C.abort("unsupported declaration " + stmt)
    }
  }

  def transformToplevel(stmt: core.Stmt)(implicit C: TransformerContext): Stmt = {
    stmt match {
      case core.Def(_, _, core.Extern(_, _), rest) =>
        transformToplevel(rest)
      case core.Def(blockName, _, block: core.Block, rest) => {
        // TODO top-level definitions don't need evidence
        C.blockParamsSet = Set();
        Def(blockName, transform(block), transformToplevel(rest))
      }
      case core.Def(_, _, _, rest) =>
        // TODO expand this catch-all case
        transformToplevel(rest)
      case core.Include(_, rest) =>
        transformToplevel(rest)
      case core.Record(_, _, rest) =>
        // TODO these are for records and capabilities
        // TODO We only support singleton capabilities
        transformToplevel(rest)
      case core.Exports(path, symbols) =>
        // TODO jump to main symbol
        Ret(List())
      case core.Data(_, _, rest) =>
        transformToplevel(rest)
      case _ =>
        println(stmt)
        C.abort("unsupported top-level statement " + stmt)
    }
  }

  def transform(stmt: core.Stmt)(implicit C: TransformerContext): Stmt = {
    stmt match {
      case core.Val(name, tpe, bind, rest) =>
        PushFrame(
          List(transform(tpe)),
          BlockLit(List(transform(core.ValueParam(name, tpe))), transform(rest)),
          List(),
          transform(bind)
        )
      case core.Ret(expr) =>
        Ret(List(transform(expr)))
      case core.Def(blockName, _, block: core.Block, rest) =>
        Def(blockName, transform(block), transform(rest))
      case core.App(core.ScopeApp(core.BlockVar(name), scope), List(), args) =>
        // TODO deal with BlockLit
        if (C.blockParamsSet.contains(name)) {
          // TODO get block type from elsewhere
          PushStack(Var(transform(C.blockTypeOf(name)), name), Ret(transform(scope) :: args.map(transform)))
        } else {
          Jump(BlockVar(name), transform(scope) :: args.map(transform))
        }
      case core.App(core.Member(core.ScopeApp(core.BlockVar(name: CapabilityParam), scope), _), null, args) => {
        // TODO fix this null upstream
        // TODO merge this with other application case
        PushStack(Var(transform(name.tpe), name), Ret(transform(scope) :: args.map(transform)))
      }
      // TODO add case for resume
      case core.If(cond, thenStmt, elseStmt) => {
        If(
          transform(cond),
          BlockLit(List(), transform(thenStmt)), List(),
          BlockLit(List(), transform(elseStmt)), List()
        )
      }
      case core.Match(scrutinee, clauses) => {
        clauses match {
          case (core.TagPattern(tag, _), core.BlockLit(params, body)) :: cs =>
            val idx = C.blockTypeOf(tag).ret.tpe match {
              case dataType: symbols.DataType => dataType.variants.indexOf(tag)
              case _ => C.abort("unsupported type " + C.blockTypeOf(tag).ret.tpe)
            }
            val fieldsName = FreshValueSymbol("fields", C.module)
            val fieldTypes = Record(params.map(p => transform(p.asInstanceOf[ValueParam].tpe)))
            val thenBody = params.zipWithIndex.foldRight(transform(body)) {
              case ((ValueParam(id, tpe), index), rest) => Let(id, Select(transform(tpe), Var(fieldTypes, fieldsName), index), rest)
            }
            // ToDo: do not transform scrutinee twice!
            val thenBlock = BlockLit(List(), Let(fieldsName, Reject(fieldTypes, transform(scrutinee), idx), thenBody))
            val elseBlock = BlockLit(List(), transform(core.Match(scrutinee, cs)))
            Match(transform(scrutinee), idx, thenBlock, List(), elseBlock, List())
          case Nil => Panic()
          case _   => C.abort("unsupported Match statement" + clauses)
        }
      }
      case core.Handle(body, handlers) => {

        val answerType = handlers match {
          case handler :: _ => transform(answerTypeOf(handler))
          case _ =>
            println(handlers)
            C.abort("unsupported handlers " + handlers)
        }
        val paramName = FreshValueSymbol("a", C.module);
        val delimiter = NewStack(
          Stack(List(answerType)),
          BlockLit(
            List(Param(answerType, paramName)),
            Ret(List(Var(answerType, paramName)))
          ), List()
        );
        val evidence = EviLit(1);

        PushStack(
          delimiter,
          Jump(transform(body), evidence :: handlers.map(transform))
        )
      }
      case _ =>
        println(stmt)
        C.abort("unsupported statement " + stmt)
    }
  }

  def transform(expr: core.Expr)(implicit C: TransformerContext): Arg = {
    expr match {
      case core.IntLit(value) =>
        IntLit(value)
      case core.BooleanLit(value) =>
        BooleanLit(value)
      case core.UnitLit() =>
        UnitLit()
      case core.ValueVar(name: ValueSymbol) =>
        // TODO get value type from elsewhere
        Var(transform(C.valueTypeOf(name)), name)
      case core.PureApp(core.BlockVar(blockName: BuiltinFunction), List(), args) =>
        AppPrim(transform(blockName.ret.get.tpe), blockName, args.map(transform))
      case core.PureApp(core.BlockVar(constructorName: symbols.Record), List(), args) =>
        constructorName.tpe match {
          case dataType: symbols.DataType =>
            Inject(transform(dataType), Construct(transform(constructorName), args.map(transform)), dataType.variants.indexOf(constructorName))
          case _: symbols.Record =>
            if (args.isEmpty) {
              UnitLit()
            } else {
              Construct(transform(constructorName), args.map(transform))
            }
          case _ => C.abort("unsupported type " + constructorName.tpe)
        }
      case core.Select(target, field) =>
        val fld = field.asInstanceOf[symbols.Field]
        val idx = fld.rec.fields.indexOf(fld)
        Select(transform(fld.tpe), transform(target), idx)
      case _ =>
        println(expr)
        C.abort("unsupported expression " + expr)
    }
  }

  def transform(block: core.Block)(implicit C: TransformerContext): BlockLit = {
    block match {
      case core.ScopeAbs(scopeName, core.BlockLit(params, body)) =>
        params.foreach {
          case core.BlockParam(name, _) => C.blockParamsSet += name
          case _ => ()
        };
        BlockLit(Param(evidenceType(), scopeName) :: params.map(transform), transform(body))
      case _ =>
        println(block)
        C.abort("unsupported block " + block)
    }
  }

  def transform(arg: core.Argument)(implicit C: TransformerContext): Arg = {
    arg match {
      case expr: core.Expr =>
        transform(expr)
      case core.BlockVar(name: CapabilityParam) =>
        Var(transform(name.tpe), name)
      case core.Lifted(scope, block) =>
        val blockArg = transform(block: core.Argument);
        val paramTypes = blockArg match {
          case Var(Stack(typs), _) => typs
          case NewStack(Stack(typs), _, _) => typs
          case _ => C.abort("Internal error: Lifted non-block argument.")
        };
        val params = paramTypes.map { paramType =>
          paramType match {
            case Stack(_) =>
              Param(paramType, FreshBlockSymbol("c", C.module))
            case Evidence() =>
              Param(evidenceType(), core.ScopeId())
            case _ =>
              Param(paramType, FreshValueSymbol("a", C.module))
          }
        };
        val paramArgs = params.map { param => Var(param.typ, param.id) }
        val args = EviPlus(transform(scope), paramArgs.head) :: paramArgs.tail
        NewStack(Stack(paramTypes), BlockLit(
          params,
          PushStack(blockArg, Ret(args))
        ), List())
      case block: core.Block =>
        // TODO This seems to overlap, move this elsewhere?
        val transformedBlock = transform(block);
        NewStack(Stack(transformedBlock.params.map(_.typ)), transformedBlock, List())
      case _ =>
        println(arg);
        C.abort("unsupported argument " + arg)
    }
  }

  def transform(handler: core.Handler)(implicit C: TransformerContext): Expr = {
    handler match {
      case core.Handler(_, List((operationName, core.BlockLit(params :+ resume, body)))) =>
        // TODO we assume here that resume is the last param
        // TODO we assume that there are no block params in handlers

        val resumeName = resume.id.asInstanceOf[ResumeParam]

        val resultType = transform(operationName.annotatedReturn.tpe);
        // TODO find a better way to get the answer type
        val answerType = transform(answerTypeOf(handler));

        val continuationName = FreshBlockSymbol("k", C.module);
        val resumptionParamName = FreshValueSymbol("r", C.module);

        val resumption = withEvidence(answerType, BlockLit(
          List(Param(resultType, resumptionParamName)),
          PushStack(Var(Stack(List(resultType)), continuationName), Ret(List(Var(resultType, resumptionParamName))))
        ));

        val capability = withEvidence(resultType, BlockLit(
          params.map(transform),
          PopStack(
            continuationName,
            Def(resumeName, resumption,
              transform(body))
          )
        ));

        NewStack(Stack(capability.params.map(_.typ)), capability, List())
      case _ =>
        println(handler)
        C.abort("unsupported handler " + handler)
    }
  }

  def withEvidence(typ: Type, block: BlockLit)(implicit C: TransformerContext): BlockLit = block match {
    case BlockLit(params, body) =>
      val scopeName = core.ScopeId();
      val evidenceAndParams = Param(evidenceType(), scopeName) :: params;
      BlockLit(
        evidenceAndParams,
        liftStack(typ, Var(evidenceType(), scopeName), body)
      )

  }

  def liftStack(typ: Type, evi: Value, stmt: Stmt)(implicit C: TransformerContext): Stmt = {
    val currentScopeName = core.ScopeId();
    val currentStackName = FreshBlockSymbol("stk", C.module);
    val paramName = FreshValueSymbol("a", C.module);
    val liftLoopName = FreshBlockSymbol("lift", C.module);
    val liftLoop = BlockLit(
      List(Param(evidenceType(), currentScopeName)),
      If(
        EviIsZero(Var(evidenceType(), currentScopeName)),
        BlockLit(List(), stmt), List(),
        BlockLit(
          List(),
          PopStack(
            currentStackName,
            PushFrame(
              List(typ),
              BlockLit(
                List(Param(typ, paramName)),
                PushStack(
                  Var(Stack(List(typ)), currentStackName),
                  Ret(List(Var(typ, paramName)))
                )
              ), List(),
              Jump(BlockVar(liftLoopName), List(EviDecr(Var(evidenceType(), currentScopeName))))
            )
          )
        ), List()
      )
    );
    Def(liftLoopName, liftLoop,
      Jump(BlockVar(liftLoopName), List(evi)))
  }

  def transform(param: core.Param)(implicit C: TransformerContext): Param = {
    param match {
      case core.ValueParam(name, tpe) =>
        Param(transform(tpe), name)
      case core.BlockParam(name, tpe) =>
        Param(transform(tpe), name)
      case _ =>
        println(param)
        C.abort("unsupported parameter " + param)
    }
  }

  def transform(scope: core.Scope)(implicit C: TransformerContext): Arg = {
    scope match {
      case core.Here() =>
        EviLit(0)
      case core.Nested(scopes) =>
        // TODO optimize non-empty case
        val empty: Arg = EviLit(0);
        scopes.foldRight(empty) { (scope, evi) => EviPlus(transform(scope), evi) }
      case core.ScopeVar(scopeName) =>
        Var(evidenceType(), scopeName)
    }
  }

  def transform(typ: symbols.Type)(implicit C: TransformerContext): Type = {
    typ match {
      case symbols.BuiltinType(builtins.TUnit.name, List()) =>
        PrimUnit()
      case symbols.BuiltinType(builtins.TInt.name, List()) =>
        PrimInt()
      case symbols.BuiltinType(builtins.TBoolean.name, List()) =>
        PrimBoolean()
      case symbols.Record(_, _, _, fields) =>
        if (fields.isEmpty) {
          PrimUnit()
        } else {
          val fieldTypes = fields.map(_.tpe)
          Record(fieldTypes.map(transform(_)))
        }
      case symbols.DataType(_, _, variants) =>
        Variant(variants.map(transform))
      case symbols.BlockType(_, sections, ret / _) =>
        // TODO do we only use this function on parameter types?
        Stack(evidenceType() :: sections.flatten.map(transform(_)))
      case symbols.CapabilityType(UserEffect(_, _, List(op))) =>
        // TODO capability types with multiple operations?
        Stack(evidenceType() :: symbols.paramsToTypes(op.params).flatten.map(transform(_)))
      case _: symbols.TypeVar =>
        // TODO this is very wrong, but polymorphism isn't supported!
        PrimUnit()
      case _ =>
        println(typ)
        C.abort("unsupported type " + typ)
    }
  }

  def evidenceType(): Type = Evidence()

  def answerTypeOf(handler: core.Handler)(implicit C: TransformerContext): symbols.Type =
    handler match {
      case core.Handler(_, List((_, core.BlockLit(params, _)))) =>
        // TODO we assume here that resume is the last param
        C.blockTypeOf(params.last.id) match {
          case symbols.BlockType(_, _, symbols.Effectful(returnType, _)) => returnType
        }
      case _ =>
        println(handler)
        C.abort("can't find answer type of " + handler)
    }

  /**
   * Extra info in context
   */

  case class TransformerContext(context: Context) {
    var blockParamsSet: Set[BlockSymbol] = Set()
  }

  private implicit def asContext(C: TransformerContext): Context = C.context
  private implicit def getContext(implicit C: TransformerContext): Context = C.context
}
