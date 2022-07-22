package effekt
package machine

import scala.collection.mutable
import effekt.context.Context
import effekt.lifted.{ AnyPattern, IgnorePattern, LiteralPattern, TagPattern, ValueParam }
import effekt.symbols.{ TermSymbol, BlockSymbol, BlockType, FunctionType, BuiltinFunction, UserFunction, Module, Name, ResumeParam, Symbol, ValueSymbol, builtins }

// TODO delete imports

case class FreshValueSymbol(baseName: String, module: Module) extends ValueSymbol {
  val name = Name.qualified(baseName, module)
}
case class FreshBlockSymbol(baseName: String, module: Module) extends BlockSymbol {
  val name = Name.qualified(baseName, module)
}

object Transformer {

  def transform(mainSymbol: TermSymbol, mod: lifted.ModuleDecl, deps: List[lifted.ModuleDecl])(using C: Context): Program = {
    val lifted.ModuleDecl(_, _, stmt, _) = mod;
    implicit val TLC = ToplevelContext(transform(mainSymbol));

    val statement = transformToplevel(stmt, deps);

    val declarations = TLC.declarations; TLC.declarations = null;

    Program(declarations, statement)
  }

  def transformToplevel(stmt: lifted.Stmt, mods: List[lifted.ModuleDecl])(using ToplevelContext, Context): Statement =
    stmt match {
      case lifted.Def(name, functionType: FunctionType, lifted.Extern(params, body), rest) =>
        emitDeclaration(DefineForeign(transform(functionType.result), transform(name),
          params.map{case ValueParam(id, tpe) => Variable(id.name.name, transform(tpe)) }, body));
        transformToplevel(rest, mods)
      case lifted.Def(blockName, _, lifted.ScopeAbs(scopeName, lifted.BlockLit(params, body)), rest) =>
        // TODO top-level definitions don't need evidence, or do they?
        // TODO add evidence param
        Def(Label(transform(blockName), params.map(transform)), transform(body), transformToplevel(rest, mods))
      case lifted.Def(_, _, _, rest) =>
        // TODO expand this catch-all case
        transformToplevel(rest, mods)
      case lifted.Include(content, rest) =>
        emitDeclaration(        Include(content));
        transformToplevel(rest, mods)
      case lifted.Record(_, _, rest) =>
        // TODO these are for records and capabilities
        // TODO We only support singleton capabilities
        transformToplevel(rest, mods)
      case lifted.Data(_, _, rest) =>
        transformToplevel(rest, mods)
      case lifted.Ret(lifted.UnitLit()) =>
        // TODO this marks the end of the list
        mods match {
          case Nil =>
            Jump(Label(getMainName, List()))
          case  lifted.ModuleDecl(_, _, stmt, _) :: mods =>
            transformToplevel(stmt, mods)
        }
      case _ =>
        println(stmt)
        abort("unsupported declaration " + stmt)
    }

  def transform(stmt: lifted.Stmt)(implicit C: Context): Statement =
    stmt match {
      case lifted.Ret(lifted.Run(stmt)) =>
        transform(stmt)
      case lifted.Ret(expr) =>
        transform(expr).run(value => Return(List(value)))
      case lifted.App(lifted.ScopeApp(lifted.BlockVar(id), scope), List(), args) =>
        // TODO deal with BlockLit
        // TODO deal with local block definitions (their free variables must be kept in a map)
        // TODO deal with evidence
        id match {
          case UserFunction(_, _, vparams, bparams, _, _, _) =>
            val environment = (vparams ++ bparams).map(transformParamSymbol);
            transform(args).run(values =>
              Substitute(environment.zip(values), Jump(Label(transform(id), environment))))
          case symbols.BlockParam(_, tpe) =>
            transform(args).run(values =>
              Invoke(Variable(transform(id), transform(tpe)) , 0, values))
          case _ =>
            println(id);
            C.abort("unsupported blocksymbol " + id)
        }
      case lifted.Val(id, tpe, bind, rest) =>
        PushFrame(
          Clause(List(transform(lifted.ValueParam(id, tpe))), transform(rest)),
          transform(bind)
        )
      case lifted.If(cond, thenStmt, elseStmt) =>
        transform(cond).run(value =>
          Switch(value, List(Clause(List(), transform(thenStmt)), Clause(List(), transform(elseStmt)))))
      case lifted.Let(id, tpe, binding, rest) =>
        transform(binding).run(value =>
          Substitute(List(Variable(transform(id), transform(tpe)) -> value), transform(rest)))
    }
  //       case lifted.Def(blockName, _, block: lifted.Block, rest) =>
  //         Def(blockName, transform(block), transform(rest))
  //       case lifted.App(lifted.Member(lifted.ScopeApp(lifted.BlockVar(name: CapabilityParam), scope), _), null, args) => {
  //         // TODO fix this null upstream
  //         // TODO merge this with other application case
  //         PushStack(Var(name, transform(name.tpe)), Ret(transform(scope) :: args.map(transform)))
  //       }
  //       // TODO add case for resume
  //       case lifted.If(cond, thenStmt, elseStmt) => {
  //         If(
  //           transform(cond),
  //           BlockLit(List(), transform(thenStmt)), List(),
  //           BlockLit(List(), transform(elseStmt)), List()
  //         )
  //       }
  //       case lifted.Match(scrutinee, clauses) => {
  //         clauses match {
  //           case (lifted.TagPattern(tag, _), lifted.BlockLit(params, body)) :: cs =>
  //             val idx = C.blockTypeOf(tag).ret.tpe match {
  //               case dataType: symbols.DataType => dataType.variants.indexOf(tag)
  //               case _ => C.abort("unsupported type " + C.blockTypeOf(tag).ret.tpe)
  //             }
  //             val fieldsName = FreshValueSymbol("fields", C.module)
  //             val fieldTypes = Record(params.map(p => transform(p.asInstanceOf[ValueParam].tpe)))
  //             val thenBody = params.zipWithIndex.foldRight(transform(body)) {
  //               case ((ValueParam(id, tpe), index), rest) => Let(id, Select(transform(tpe), Var(fieldsName, fieldTypes), index), rest)
  //             }
  //             // ToDo: do not transform scrutinee twice!
  //             val thenBlock = BlockLit(List(), Let(fieldsName, Reject(fieldTypes, transform(scrutinee), idx), thenBody))
  //             val elseBlock = BlockLit(List(), transform(lifted.Match(scrutinee, cs)))
  //             Match(transform(scrutinee), idx, thenBlock, List(), elseBlock, List())
  //           case Nil => Panic()
  //           case _   => C.abort("unsupported Match statement" + clauses)
  //         }
  //       }
  //       case lifted.Handle(body, handlers) => {

  //         val answerType = handlers match {
  //           case handler :: _ => transform(answerTypeOf(handler))
  //           case _ =>
  //             println(handlers)
  //             C.abort("unsupported handlers " + handlers)
  //         }
  //         val paramName = FreshValueSymbol("a", C.module);
  //         val delimiter = NewStack(
  //           Stack(List(answerType)),
  //           BlockLit(
  //             List(Param(answerType, paramName)),
  //             Ret(List(Var(paramName, answerType)))
  //           ), List()
  //         );
  //         val evidence = EviLit(1);

  //         PushStack(
  //           delimiter,
  //           Jump(transform(body), evidence :: handlers.map(transform))
  //         )
  //       }
  //       case _ =>
  //         println(stmt)
  //         C.abort("unsupported statement " + stmt)

  def transform(arg: lifted.Argument)(implicit C: Context): Binding[Variable] =
    arg match {
      case lifted.ValueVar(id) =>
        val tpe = C.valueTypeOf(id);
        pure(Variable(transform(id), transform(tpe)))
      case lifted.IntLit(value) =>
        // TODO generate fresh name differently...
        val id = FreshValueSymbol("x", C.module);
        val variable = Variable(transform(id), Primitive("Int"));
        Binding(k =>
        Run(LiteralInt(value), List(), List(Clause(List(variable), k(variable)))))
      case lifted.PureApp(lifted.BlockVar(blockName: BuiltinFunction), List(), args) =>
        val id = FreshValueSymbol("x", C.module);
        val tpe = blockName.result;
        val variable = Variable(transform(id), transform(tpe));
        transform(args).flatMap(values =>
          Binding(k =>
            Run(CallForeign(transform(blockName)), values, List(
              Clause(List(variable),
              k(variable))))))
      case lifted.Run(stmt) =>
        val id = FreshValueSymbol("x", C.module);
        // TODO find actual type
        val variable = Variable(transform(id), Primitive("Int"));
        Binding(k => PushFrame(Clause(List(variable), k(variable)), transform(stmt)))
      case lifted.ScopeAbs(_, lifted.BlockLit(params, body)) =>
        // TODO deal with evidence
        val parameters = params.map(transform);
        val id = FreshValueSymbol("g", C.module);
        val variable = Variable(transform(id), Negative(List(parameters.map(_.tpe))));
        Binding(k => New(variable, List(Clause(params.map(transform), transform(body))), k(variable)))
      case _ =>
        println(arg)
        C.abort("unsupported argument " + arg)
    }

  def transform(args: List[lifted.Argument])(implicit C: Context): Binding[List[Variable]] =
    args match {
      case Nil => pure(Nil)
      case arg :: args => transform(arg).flatMap(value => transform(args).flatMap(values => pure(value :: values)))
    }
  //       case lifted.BooleanLit(value) =>
  //         BooleanLit(value)
  //       case lifted.UnitLit() =>
  //         UnitLit()
  //       case lifted.ValueVar(name: ValueSymbol) =>
  //         // TODO get value type from elsewhere
  //         Var(name, transform(C.valueTypeOf(name)))
  //       case lifted.PureApp(lifted.BlockVar(constructorName: symbols.Record), List(), args) =>
  //         constructorName.tpe match {
  //           case dataType: symbols.DataType =>
  //             Inject(transform(dataType), Construct(transform(constructorName), args.map(transform)), dataType.variants.indexOf(constructorName))
  //           case _: symbols.Record =>
  //             if (args.isEmpty) {
  //               UnitLit()
  //             } else {
  //               Construct(transform(constructorName), args.map(transform))
  //             }
  //           case _ => C.abort("unsupported type " + constructorName.tpe)
  //         }
  //       case lifted.Select(target, field) =>
  //         val fld = field.asInstanceOf[symbols.Field]
  //         val idx = fld.rec.fields.indexOf(fld)
  //         Select(transform(fld.tpe), transform(target), idx)
  //       case _ =>
  //         println(expr)
  //         C.abort("unsupported expression " + expr)

  //   def transform(block: lifted.Block)(implicit C: Context): BlockLit = {
  //     block match {
  //       case lifted.ScopeAbs(scopeName, lifted.BlockLit(params, body)) =>
  //         params.foreach {
  //           case lifted.BlockParam(name, _) => C.blockParamsSet += name
  //           case _ => ()
  //         };
  //         BlockLit(Param(evidenceType(), scopeName) :: params.map(transform), transform(body))
  //       case _ =>
  //         println(block)
  //         C.abort("unsupported block " + block)
  //     }
  //   }

  //   def transform(arg: lifted.Argument)(implicit C: Context): Arg = {
  //     arg match {
  //       case expr: lifted.Expr =>
  //         transform(expr)
  //       case lifted.BlockVar(name: CapabilityParam) =>
  //         Var(name, transform(name.tpe))
  //       case lifted.Lifted(scope, block) =>
  //         val blockArg = transform(block: lifted.Argument);
  //         val paramTypes = blockArg match {
  //           case Var(_, Stack(typs)) => typs
  //           case NewStack(Stack(typs), _, _) => typs
  //           case _ => C.abort("Internal error: Lifted non-block argument.")
  //         };
  //         val params = paramTypes.map { paramType =>
  //           paramType match {
  //             case Stack(_) =>
  //               Param(paramType, FreshBlockSymbol("c", C.module))
  //             case Evidence() =>
  //               Param(evidenceType(), lifted.ScopeId())
  //             case _ =>
  //               Param(paramType, FreshValueSymbol("a", C.module))
  //           }
  //         };
  //         val paramArgs = params.map { param => Var(param.id, param.typ) }
  //         val args = EviPlus(transform(scope), paramArgs.head) :: paramArgs.tail
  //         NewStack(Stack(paramTypes), BlockLit(
  //           params,
  //           PushStack(blockArg, Ret(args))
  //         ), List())
  //       case block: lifted.Block =>
  //         // TODO This seems to overlap, move this elsewhere?
  //         val transformedBlock = transform(block);
  //         NewStack(Stack(transformedBlock.params.map(_.typ)), transformedBlock, List())
  //       case _ =>
  //         println(arg);
  //         C.abort("unsupported argument " + arg)
  //     }
  //   }

  //   def transform(handler: lifted.Handler)(implicit C: Context): Expr = {
  //     handler match {
  //       case lifted.Handler(_, List((operationName, lifted.BlockLit(params :+ resume, body)))) =>
  //         // TODO we assume here that resume is the last param
  //         // TODO we assume that there are no block params in handlers

  //         val resumeName = resume.id.asInstanceOf[ResumeParam]

  //         val resultType = transform(operationName.annotatedReturn.tpe);
  //         // TODO find a better way to get the answer type
  //         val answerType = transform(answerTypeOf(handler));

  //         val continuationName = FreshBlockSymbol("k", C.module);
  //         val resumptionParamName = FreshValueSymbol("r", C.module);

  //         val resumption = withEvidence(answerType, BlockLit(
  //           List(Param(resultType, resumptionParamName)),
  //           PushStack(Var(continuationName, Stack(List(resultType))), Ret(List(Var(resumptionParamName, resultType))))
  //         ));

  //         val capability = withEvidence(resultType, BlockLit(
  //           params.map(transform),
  //           PopStack(
  //             continuationName,
  //             Def(resumeName, resumption,
  //               transform(body))
  //           )
  //         ));

  //         NewStack(Stack(capability.params.map(_.typ)), capability, List())
  //       case _ =>
  //         println(handler)
  //         C.abort("unsupported handler " + handler)
  //     }
  //   }

  //   def withEvidence(typ: Type, block: BlockLit)(implicit C: Context): BlockLit = block match {
  //     case BlockLit(params, body) =>
  //       val scopeName = lifted.ScopeId();
  //       val evidenceAndParams = Param(evidenceType(), scopeName) :: params;
  //       BlockLit(
  //         evidenceAndParams,
  //         liftStack(typ, Var(scopeName, evidenceType()), body)
  //       )

  //   }

  //   def liftStack(typ: Type, evi: Value, stmt: Stmt)(implicit C: Context): Stmt = {
  //     val currentScopeName = lifted.ScopeId();
  //     val currentStackName = FreshBlockSymbol("stk", C.module);
  //     val paramName = FreshValueSymbol("a", C.module);
  //     val liftLoopName = FreshBlockSymbol("lift", C.module);
  //     val liftLoop = BlockLit(
  //       List(Param(evidenceType(), currentScopeName)),
  //       If(
  //         EviIsZero(Var(currentScopeName, evidenceType())),
  //         BlockLit(List(), stmt), List(),
  //         BlockLit(
  //           List(),
  //           PopStack(
  //             currentStackName,
  //             PushFrame(
  //               List(typ),
  //               BlockLit(
  //                 List(Param(typ, paramName)),
  //                 PushStack(
  //                   Var(currentStackName, Stack(List(typ))),
  //                   Ret(List(Var(paramName, typ)))
  //                 )
  //               ), List(),
  //               Jump(BlockVar(liftLoopName), List(EviDecr(Var(currentScopeName, evidenceType()))))
  //             )
  //           )
  //         ), List()
  //       )
  //     );
  //     Def(liftLoopName, liftLoop,
  //       Jump(BlockVar(liftLoopName), List(evi)))
  //   }

  def transform(param: lifted.Param)(implicit C: Context): Variable =
    param match {
      case lifted.ValueParam(name, tpe) =>
        Variable(transform(name), transform(tpe))
      case lifted.BlockParam(name, tpe) =>
        Variable(transform(name), transform(tpe))
      case _ =>
        println(param)
        C.abort("unsupported parameter " + param)
    }

  // def transform(scope: lifted.Scope)(implicit C: Context): Variable =
  //   scope match {
  //     case lifted.ScopeVar(transform(scopeName)) =>
  //       Variable(scopeName, Primitive("Evi"))
  //   }
  //       case lifted.Here() =>
  //         EviLit(0)
  //       case lifted.Nested(scopes) =>
  //         // TODO optimize non-empty case
  //         val empty: Arg = EviLit(0);
  //         scopes.foldRight(empty) { (scope, evi) => EviPlus(transform(scope), evi) }

  def transform(tpe: symbols.Type)(implicit C: Context): Type =
    tpe match {
      case symbols.BuiltinType(builtins.TUnit.name, List()) =>
        Positive(List(List()))
      case symbols.BuiltinType(builtins.TInt.name, List()) =>
        Primitive("Int")
      case symbols.BuiltinType(builtins.TBoolean.name, List()) =>
        Positive(List(List(), List()))
      case symbols.FunctionType(List(), List(), vparams, List(), _, _) =>
        // TODO block params too
        Negative(List(vparams.map(transform)))
      case _ =>
        println(tpe)
        C.abort("unsupported type " + tpe)
    }

  // case symbols.Record(_, _, _, fields) =>
  //   if (fields.isEmpty) {
  //     PrimUnit()
  //   } else {
  //     val fieldTypes = fields.map(_.tpe)
  //     Record(fieldTypes.map(transform(_)))
  //   }
  // case symbols.DataType(_, _, variants) =>
  //   Variant(variants.map(transform))
  // case symbols.BlockType(_, sections, ret / _) =>
  //   // TODO do we only use this function on parameter types?
  //   Stack(evidenceType() :: sections.flatten.map(transform(_)))
  // case symbols.CapabilityType(UserEffect(_, _, List(op))) =>
  //   // TODO capability types with multiple operations?
  //   Stack(evidenceType() :: symbols.paramsToTypes(op.params).flatten.map(transform(_)))
  // case _: symbols.TypeVar =>
  //   // TODO this is very wrong, but polymorphism isn't supported!
  //   PrimUnit()

  def transformParamSymbol(param: symbols.Param)(implicit C: Context): Variable =
    param match {
      case symbols.ValueParam(name, Some(tpe)) =>
        Variable(s"${param.name}_${param.id}", transform(tpe))
      case symbols.BlockParam(name, tpe) =>
        Variable(s"${param.name}_${param.id}", transform(tpe))
      case _ =>
        println(param);
        C.abort("unsupported symbol param " + param)
    }

  def transform(id: Symbol): String =
    s"${id.name}_${id.id}"

  //   def evidenceType(): Type = Evidence()

  //   def answerTypeOf(handler: lifted.Handler)(implicit C: Context): symbols.Type =
  //     handler match {
  //       case lifted.Handler(_, List((_, lifted.BlockLit(params, _)))) =>
  //         // TODO we assume here that resume is the last param
  //         C.blockTypeOf(params.last.id) match {
  //           case symbols.BlockType(_, _, symbols.Effectful(returnType, _)) => returnType
  //         }
  //       case _ =>
  //         println(handler)
  //         C.abort("can't find answer type of " + handler)
  //     }

  /**
   * Extra info in context
   */

  def abort(message: String)(using C: Context) =
    C.abort(message)

  case class ToplevelContext(val mainName: String) {
    var declarations: List[Declaration] = List()
  }

  def emitDeclaration(declaration: Declaration)(implicit TLC: ToplevelContext) = {
    TLC.declarations = TLC.declarations :+ declaration
  }

  def getMainName(using TLC: ToplevelContext): String = {
    TLC.mainName
  }

  case class Binding[A](run: (A => Statement) => Statement) {
    def flatMap[B](rest: A => Binding[B]): Binding[B] = {
      Binding(k => run(a => rest(a).run(k)))
    }
  }

  def pure[A](a: A): Binding[A] = Binding(k => k(a))

}
