package effekt
package machine

import scala.collection.mutable
import effekt.context.Context
import effekt.lifted
import effekt.symbols
import effekt.symbols.{ BlockSymbol, BlockType, BuiltinFunction, FunctionType, Module, Name, Symbol, TermSymbol, UserFunction, ValueSymbol }

object Transformer {

  def transform(mainSymbol: TermSymbol, mod: lifted.ModuleDecl, deps: List[lifted.ModuleDecl])(using C: Context): Program = {
    val stmt = mod.defs;
    given TLC: ToplevelContext = ToplevelContext(transform(mainSymbol));
    given BC: BlocksParamsContext = BlocksParamsContext();

    findToplevelBlocksParams(stmt)
    val transformedMain = transformToplevel(stmt, Jump(Label(getMainName, List())))

    val statement = deps.foldLeft(transformedMain) {
      case (compiled, dependency) =>
        findToplevelBlocksParams(dependency.defs);
        transformToplevel(dependency.defs, compiled)
    }

    val declarations = TLC.declarations

    Program(declarations, statement)
  }


  // TODO this marks the end of the list
  //  mods match {
  //    case Nil =>
  //      Jump(Label(getMainName, List()))
  //    case lifted.ModuleDecl(_, _, stmt, _) :: mods =>
  //      findToplevelBlocksParams(stmt);
  //      transformToplevel(stmt, mods)
  //  }

  def transformToplevel(stmt: lifted.Stmt, entryPoint: Statement)(using ToplevelContext, BlocksParamsContext, Context): Statement =
    stmt match {
      case lifted.Def(name, functionType: FunctionType, lifted.Extern(params, body), rest) =>
        val transformedParams = params.map {
          case lifted.ValueParam(id, tpe) => Variable(id.name.name, transform(tpe))
          case lifted.BlockParam(id, tpe) => Context.abort("Foreign functions currently cannot take block arguments.")
          case lifted.EvidenceParam(_) => ???
        }
        emitDeclaration(Extern(transform(name), transformedParams, transform(functionType.result), body))
        transformToplevel(rest, entryPoint)

      case lifted.Def(id, _, lifted.BlockLit(params, body), rest) =>
        // TODO top-level definitions don't need evidence, or do they?
        // some of the params are now evidence params... TODO handle evidence.
        Def(Label(transform(id), params.map(transform)), transform(body), transformToplevel(rest, entryPoint))

      case lifted.Include(content, rest) =>
        emitDeclaration(Include(content));
        transformToplevel(rest, entryPoint)

      case lifted.Record(_, _, rest) =>
        // Currently machine is structurally typed, we do not generate definitions
        transformToplevel(rest, entryPoint)

      case lifted.Data(_, _, rest) =>
        // Currently machine is structurally typed, we do not generate definitions
        transformToplevel(rest, entryPoint)

      case lifted.Return(lifted.UnitLit()) =>
        entryPoint

      case _ =>
        Context.abort("Unsupported declaration: " + stmt)
    }

  def transform(stmt: lifted.Stmt)(using BlocksParamsContext, Context): Statement =
    stmt match {
      case lifted.Return(lifted.Run(stmt, tpe)) =>
        transform(stmt)

      case lifted.Return(expr) =>
        transform(expr).run { value => Return(List(value)) }

      case lifted.Val(id, tpe, bind, rest) =>
        PushFrame(
          Clause(List(transform(lifted.ValueParam(id, tpe))), transform(rest)),
            transform(bind)
        )

      case lifted.Let(id, tpe, binding, rest) =>
        transform(binding).run { value =>
          // TODO consider passing the environment to [[transform]] instead of explicit substitutions here.
          Substitute(List(Variable(transform(id), transform(tpe)) -> value), transform(rest))
        }

      case lifted.Def(id, tpe, block @ lifted.BlockLit(params, body), rest) =>
        // TODO deal with evidence
        // TODO does not work for mutually recursive local definitions
        val freeParams = lifted.freeVariables(block).toList.collect {
          case id: symbols.ValueSymbol => Variable(transform(id), transform(Context.valueTypeOf(id)))
          case id: symbols.BlockParam  => Variable(transform(id), transform(Context.blockTypeOf(id)))
          case id: symbols.ResumeParam => Variable(transform(id), transform(Context.blockTypeOf(id)))
          // we ignore functions since we do not "close" over them.

          // TODO
          //          case id: lifted.ScopeId => ???
        }
        val allParams = params.map(transform) ++ freeParams;
        noteBlockParams(id, allParams)
        Def(Label(transform(id), allParams), transform(body), transform(rest))

      case lifted.App(lifted.BlockVar(id), List(), args) =>
        // TODO deal with BlockLit
        // TODO deal with evidence
        id match {
          case symbols.UserFunction(_, _, _, _, _, _, _) =>
            // TODO this is a hack, values is in general shorter than environment
            val environment = getBlocksParams(id)
            transform(args).run { values =>
              // Here we actually need a substitution to prepare the environment for the jump
              Substitute(environment.zip(values), Jump(Label(transform(id), environment)))
            }
          case symbols.BlockParam(_, tpe) =>
            transform(args).run { values =>
              Invoke(Variable(transform(id), transform(tpe)), builtins.Apply, values)
            }
          case symbols.ResumeParam(_) =>
            // TODO currently only scoped resumptions are supported
            transform(args).run { values =>
              PushStack(Variable(transform(id), Type.Stack()),
                Return(values))
            }
          case _ =>
            Context.abort("Unsupported blocksymbol: " + id)
        }

      case lifted.App(lifted.Member(lifted.BlockVar(id), op), List(), args) =>
        id match {
          case symbols.BlockParam(_, tpe) =>
            transform(args).run { values =>
              // TODO find correct operation tag for [[op]]
              //   we currently only support singleton effect operations (or calling the first one, for that matter)
              Invoke(Variable(transform(id), transform(tpe)), 0, values)
            }
          case _ =>
            Context.abort("Unsupported blocksymbol: " + id)
        }

      case lifted.If(cond, thenStmt, elseStmt) =>
        transform(cond).run { value =>
          Switch(value, List(Clause(List(), transform(elseStmt)), Clause(List(), transform(thenStmt))))
        }

      case lifted.Match(scrutinee, clauses) =>
        // TODO unordered matches
        // TODO overlapping matches
        // TODO incomplete matches
        // TODO nested matches
        transform(scrutinee).run { value =>
          Switch(value, clauses.map {
            case (lifted.IgnorePattern(), _) => ??? // Clause(List(), NOOP) // TODO verify
            case (lifted.AnyPattern(), _) => ???
            case (lifted.TagPattern(constructor, patterns), lifted.BlockLit(params, body)) =>
              Clause(params.map(transform), transform(body))
            case (lifted.LiteralPattern(l), block) => ???
          })
        }

      case lifted.Handle(lifted.BlockLit(List(ev, id), body), List(handler)) =>
        // TODO deal with evidence
        // TODO more than one handler
        val variable = Variable(freshName("a"), transform(answerTypeOf(handler)));
        val returnClause = Clause(List(variable), Return(List(variable)));
        val delimiter = Variable(freshName("returnClause"), Type.Stack());

        NewStack(delimiter, returnClause,
          PushStack(delimiter,
            New(transform(id), transform(handler),
              transform(body))))

      case _ =>
        Context.abort("Unsupported statement: " + stmt)
    }

  def transform(arg: lifted.Argument)(using BlocksParamsContext, Context): Binding[Variable] = arg match {
    case expr: lifted.Expr => transform(expr)
    case block: lifted.Block => transform(block)
    case lifted.Evidence(_) => transform(lifted.IntLit(0)) // TODO implement
  }

  def transform(block: lifted.Block)(using BlocksParamsContext, Context): Binding[Variable] = block match {
    case lifted.BlockVar(id) =>
      val tpe = Context.blockTypeOf(id)
      pure(Variable(transform(id), transform(tpe)))

    case lifted.BlockLit(params, body) =>
      // TODO deal with evidence
      val parameters = params.map(transform);
      val variable = Variable(freshName("g"), Negative(List(parameters.map(_.tpe))))
      Binding { k =>
        New(variable, List(Clause(parameters, transform(body))), k(variable))
      }

    case lifted.Member(b, field) => ???
    case lifted.Extern(params, body) => ???
    case lifted.Unbox(e) => ???
    case lifted.New(impl) => ???
  }

  def transform(expr: lifted.Expr)(using BlocksParamsContext, Context): Binding[Variable] = expr match {
    case lifted.ValueVar(id) =>
      val tpe = Context.valueTypeOf(id);
      pure(Variable(transform(id), transform(tpe)))

    case lifted.UnitLit() =>
      val variable = Variable(freshName("x"), Positive(List(List())));
      Binding { k =>
        Construct(variable, builtins.Unit, List(), k(variable))
      }

    case lifted.IntLit(value) =>
      val variable = Variable(freshName("x"), Type.Int());
      Binding { k =>
        LiteralInt(variable, value, k(variable))
      }

    case lifted.BooleanLit(value: Boolean) =>
      val variable = Variable(freshName("x"), Positive(List(List(), List())))
      Binding { k =>
        Construct(variable, if (value) builtins.True else builtins.False, List(), k(variable))
      }

    case lifted.PureApp(lifted.BlockVar(blockName: symbols.BuiltinFunction), List(), args) =>
      val variable = Variable(freshName("x"), transform(blockName.result))
      transform(args).flatMap { values =>
        Binding { k =>
          ForeignCall(variable, transform(blockName), values, k(variable))
        }
      }

    case lifted.PureApp(lifted.BlockVar(blockName: symbols.Record), List(), args) =>
      val variable = Variable(freshName("x"), transform(blockName.tpe));
      val tag = blockName.tpe match {
        case symbols.DataType(name, Nil, variants) => variants.indexOf(blockName)
        // TODO
        case symbols.DataType(name, tparams, variants) => Context.abort("not yet supported: (data) type polymorphism")

        case symbols.Record(name, Nil, tpe, fields) => builtins.SingletonRecord
        // TODO
        case symbols.Record(name, tparams, tpe, fields) => Context.abort("not yet supported: record polymorphism")

        case symbol => Context.abort(s"application to an unknown symbol: $symbol")
      }
      
      transform(args).flatMap { values =>
        Binding { k =>
          Construct(variable, tag, values, k(variable))
        }
      }

    case lifted.Select(target: lifted.Expr, field: symbols.Field) =>
      val fieldIndex = field.record.fields.indexOf(field)
      val variables = field.record.fields.map { f => Variable(freshName("n"), transform(f.tpe)) }
      transform(target).flatMap { value => 
        Binding { k =>
          Switch(value, List(Clause(variables, k(variables(fieldIndex)))))
        }
      }

    case lifted.Run(stmt, tpe) =>
      // NOTE: `stmt` is guaranteed to be of type `tpe`.
      val variable = Variable(freshName("x"), transform(tpe))
      Binding { k =>
        PushFrame(Clause(List(variable), k(variable)), transform(stmt))
      }

    case _ =>
      Context.abort(s"Unsupported expression: ${expr}")
  }

  def transform(args: List[lifted.Argument])(using BlocksParamsContext, Context): Binding[List[Variable]] =
    args match {
      case Nil => pure(Nil)
      case arg :: args => transform(arg).flatMap { value => transform(args).flatMap { values => pure(value :: values) } }
    }

  def transform(handler: lifted.Handler)(using BlocksParamsContext, Context): List[Clause] = {
    handler match {
      case lifted.Handler(_, List((operationName, lifted.BlockLit(params :+ resume, body)))) =>
        // TODO we assume here that resume is the last param
        // TODO we assume that there are no block params in handlers
        List(Clause(params.map(transform),
          PopStack(Variable(transform(resume).name, Type.Stack()),
            transform(body))))
      case _ =>
        Context.abort("Unsupported handler " + handler)
    }
  }

  def transform(param: lifted.Param)(using Context): Variable =
    param match {
      case lifted.ValueParam(name, tpe) =>
        Variable(transform(name), transform(tpe))
      case lifted.BlockParam(name, tpe) =>
        Variable(transform(name), transform(tpe))
      case lifted.EvidenceParam(name) =>
        Variable(transform(name), builtins.Evidence)
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

  def transform(tpe: symbols.Type)(using Context): Type = tpe match {
  
    case symbols.builtins.TUnit => builtins.UnitType

    case symbols.builtins.TInt => Type.Int()

    case symbols.builtins.TBoolean => builtins.BooleanType

    case symbols.FunctionType(Nil, Nil, vparams, Nil, _, _) =>
      // TODO block params too
      Negative(List(vparams.map(transform)))

    case symbols.Interface(_, List(), ops) =>
      val opsSignatures = ops.map {
        case symbols.Operation(_, List(), vparams, _, _, _) =>
          // TODO why is field type in param optional?
          vparams.map { param => transform(param.tpe.get) }
        // TODO
        case op => throw new Exception("not yet supported: polymorphic operations")
      };
      Negative(opsSignatures)

    case symbols.DataType(_, List(), records) =>
      val recSignatures = records.map {
        case symbols.Record(_, List(), _, fields) =>
          fields.map {
            case symbols.Field(_, symbols.ValueParam(_, Some(tpe)), _) =>
              transform(tpe)
            // TODO
            case _ => ???
          }
        // TODO
        case _ => throw new Exception("not yet supported: polymorphic records")
      }
      Positive(recSignatures)

    case symbols.Record(_, List(), tpe, fields) =>
      Positive(List(fields.map { field => transform(field.tpe) }))

    case _ =>
      System.err.println(s"UNSUPPORTED TYPE: getClass($tpe) = ${tpe.getClass}")
      Context.abort(s"unsupported type $tpe")
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

  def transform(id: Symbol): String =
    s"${id.name}_${id.id}"

  def freshName(baseName: String): String = baseName + "_" + symbols.Symbol.fresh.next()


  //   def evidenceType(): Type = Evidence()

  def findToplevelBlocksParams(stmt: lifted.Stmt)(using BlocksParamsContext, Context): Unit = {
    stmt match {
      case lifted.Def(name, functionType: FunctionType, lifted.Extern(params, body), rest) =>
        findToplevelBlocksParams(rest)

      case lifted.Def(blockName, _, lifted.BlockLit(params, body), rest) =>
        // TODO add evidence param
        noteBlockParams(blockName, params.map(transform));
        findToplevelBlocksParams(rest)

      case lifted.Def(_, _, _, rest) =>
        // TODO expand this catch-all case
        findToplevelBlocksParams(rest)
      case lifted.Include(content, rest) =>
        findToplevelBlocksParams(rest)
      case lifted.Record(_, _, rest) =>
        findToplevelBlocksParams(rest)
      case lifted.Data(_, _, rest) =>
        findToplevelBlocksParams(rest)
      case lifted.Return(lifted.UnitLit()) =>
        ()
      case _ =>
        println("unsupported in finding toplevel blocks " + stmt)
        ()
    }
  }

  def answerTypeOf(handler: lifted.Handler)(implicit C: Context): symbols.Type =
    handler match {
      case lifted.Handler(_, List((_, lifted.BlockLit(params, _)))) =>
        // TODO we assume here that resume is the last param
        C.blockTypeOf(params.last.id) match {
          case symbols.FunctionType(_, _, _, _, returnType, _) => returnType
          case _ => ???
        }
      case _ =>
        println(handler)
        C.abort("can't find answer type of " + handler)
    }

  /**
   * Extra info in context
   */

  def abort(message: String)(using C: Context) =
    C.abort(message)

  class ToplevelContext(val mainName: String) {
    var declarations: List[Declaration] = List()
  }

  def emitDeclaration(declaration: Declaration)(using TLC: ToplevelContext) = {
    TLC.declarations = TLC.declarations :+ declaration
  }

  def getMainName(using TLC: ToplevelContext): String = {
    TLC.mainName
  }

  class BlocksParamsContext() {
    var blocksParams: Map[BlockSymbol, Environment] = Map()
  }

  def noteBlockParams(id: BlockSymbol, params: Environment)(using BC: BlocksParamsContext): Unit = {
    BC.blocksParams = BC.blocksParams + (id -> params)
  }

  def getBlocksParams(id: BlockSymbol)(using BC: BlocksParamsContext): Environment = {
    // TODO what if this is not found?
    BC.blocksParams(id)
  }

  case class Binding[A](run: (A => Statement) => Statement) {
    def flatMap[B](rest: A => Binding[B]): Binding[B] = {
      Binding(k => run(a => rest(a).run(k)))
    }
  }

  def pure[A](a: A): Binding[A] = Binding(k => k(a))


  def Context(using C: Context) = C
}
