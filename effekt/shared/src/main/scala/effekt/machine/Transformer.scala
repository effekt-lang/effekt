package effekt
package machine

import scala.collection.mutable
import effekt.context.Context
import effekt.lifted.DeclarationContext
import effekt.lifted.given
import effekt.lifted
import effekt.lifted.{Definition, LiftInference}
import effekt.symbols
import effekt.symbols.{BlockSymbol, ExternFunction, ExternType, FunctionType, Module, Name, Symbol, TermSymbol, UserFunction, ValueSymbol}
import effekt.symbols.builtins.TState
import effekt.util.messages.ErrorReporter

object Transformer {

  private def ErrorReporter(using E: ErrorReporter): ErrorReporter = E

  def transform(main: CoreTransformed, mainSymbol: TermSymbol)(using C: Context): Program = {
    val Some(CoreLifted(_, _, _, liftedMain)) = LiftInference(main) : @unchecked

    C.using(module = main.mod) {
      transform(mainSymbol, liftedMain);
    }
  }

  def transform(mainSymbol: TermSymbol, mod: lifted.ModuleDecl)(using E: ErrorReporter): Program = {

    val mainName = transform(mainSymbol)
    given BC: BlocksParamsContext = BlocksParamsContext();
    given DC: DeclarationContext = lifted.DeclarationContext(mod.decls)

    // collect all information
    val declarations = mod.externs.map(transform)
    val definitions = mod.definitions
    val mainEntry = Jump(Label(mainName, List()))

    findToplevelBlocksParams(definitions)

    val transformedDefinitions = definitions.foldLeft(mainEntry) {
      case (rest, lifted.Definition.Def(id, lifted.BlockLit(tparams, params, body))) =>
        Def(Label(transform(id), params.map(transform)), transform(body), rest)
      case (rest, d) =>
        ErrorReporter.abort(s"Toplevel def and let bindings not yet supported: ${d}")
    }

    Program(declarations, transformedDefinitions)
  }

  def transform(extern: lifted.Extern)(using BlocksParamsContext, ErrorReporter): Declaration = extern match {
    case lifted.Extern.Def(name, tps, ps, ret, body) =>
      Extern(transform(name), ps.map(transform), transform(ret), body)

    case lifted.Extern.Include(contents) =>
      Include(contents)
  }

  def transform(stmt: lifted.Stmt)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Statement =
    stmt match {
      case lifted.Scope(definitions, rest) =>

        definitions.foreach {
          case Definition.Def(id,  block @ lifted.BlockLit(tparams, params, body)) =>
            // TODO does not work for mutually recursive local definitions
            val freeParams = lifted.freeVariables(block).toList.collect {
              case lifted.ValueParam(id, tpe) => Variable(transform(id), transform(tpe))
              case lifted.BlockParam(id: (symbols.BlockParam | symbols.ResumeParam), tpe) =>
                // TODO find out if this is a block parameter or a function without inspecting the symbol
                Variable(transform(id), transform(tpe))
              case lifted.EvidenceParam(id) => Variable(transform(id), builtins.Evidence)
              // we ignore functions since we do not "close" over them.

              // TODO
              //          case id: lifted.ScopeId => ???
            }
            val allParams = params.map(transform) ++ freeParams;
            noteBlockParams(id, allParams)
          case _ => ()
        }

        definitions.foldRight(transform(rest)) {
          case (lifted.Definition.Let(id, binding), rest) =>
            transform(binding).run { value =>
              // TODO consider passing the environment to [[transform]] instead of explicit substitutions here.
              Substitute(List(Variable(transform(id), transform(binding.tpe)) -> value), rest)
            }

          case (lifted.Definition.Def(id, block @ lifted.BlockLit(tparams, params, body)), rest) =>
            Def(Label(transform(id), getBlocksParams(id)), transform(body), rest)

          case (lifted.Definition.Def(id, block @ lifted.New(impl)), rest) =>
            val interfaceId = impl.interface.name
            // TODO freeParams?
            // TODO deal with evidence?
            val properties = DeclarationContext.getInterface(interfaceId).properties
            val implTransformed = properties.map({ prop =>
              impl.operations.find(_._1 == prop.id).get
            }).map({
              case lifted.Operation(_, lifted.BlockLit(tparams, params, body)) =>
                // TODO we assume that there are no block params in methods
                Clause(params.map(transform), transform(body))
            })
            New(Variable(transform(id), transform(impl.interface)), implTransformed, rest)

          case (d @ lifted.Definition.Def(_, _: lifted.BlockVar | _: lifted.Member | _: lifted.Unbox), rest) =>
            ErrorReporter.abort(s"block definition: $d")
        }

      case lifted.Return(lifted.Run(stmt)) =>
        transform(stmt)

      case lifted.Return(expr) =>
        transform(expr).run { value => Return(List(value)) }

      case lifted.Val(id, binding, rest) =>
        PushFrame(
          Clause(List(transform(lifted.ValueParam(id, binding.tpe))), transform(rest)),
            transform(binding)
        )
      case lifted.App(lifted.BlockVar(id, tpe), targs, args) =>
        if(targs.exists(requiresBoxing)){ ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }
        // TODO deal with BlockLit
        id match {
          case symbols.UserFunction(_, _, _, _, _, _, _)  | symbols.TmpBlock() =>
            // TODO this is a hack, values is in general shorter than environment
            val environment = getBlocksParams(id)
            transform(args).run { values =>
              // Here we actually need a substitution to prepare the environment for the jump
              Substitute(environment.zip(values), Jump(Label(transform(id), environment)))
            }
          case symbols.BlockParam(_, _) =>
            transform(args).run { values =>
              Invoke(Variable(transform(id), transform(tpe)), builtins.Apply, values)
            }
          case symbols.ResumeParam(_) =>
            // TODO currently only scoped resumptions are supported
            // TODO assuming first parameter is evidence TODO actually use evidence?
            transform(args).run { values =>
              val (evidence :: returnedValues) = values: @unchecked;
              PushStack(Variable(transform(id), Type.Stack()),
                Return(returnedValues))
            }
          case _ =>
            ErrorReporter.abort(s"Unsupported blocksymbol: $id")
        }

      case lifted.App(lifted.Member(lifted.BlockVar(id, tpe), op, annotatedTpe), targs, args) =>
        if(targs.exists(requiresBoxing)){ ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }
        val opTag = {
          tpe match
            case lifted.BlockType.Interface(ifceId, _) =>
              DeclarationContext.getPropertyTag(op)
            case _ => ErrorReporter.abort(s"Unsupported receiver type $tpe")
        }
        transform(args).run { values =>
          Invoke(Variable(transform(id), transform(tpe)), opTag, values)
        }

      case lifted.If(cond, thenStmt, elseStmt) =>
        transform(cond).run { value =>
          Switch(value, List(0 -> Clause(List(), transform(elseStmt)), 1 -> Clause(List(), transform(thenStmt))), None)
        }

      case lifted.Match(scrutinee, clauses, default) =>
        val transformedClauses = clauses.map { case (constr, lifted.BlockLit(tparams, params, body)) =>
          DeclarationContext.getConstructorTag(constr) -> Clause(params.map(transform), transform(body))
        }
        val transformedDefault = default.map { clause =>
          Clause(List(), transform(clause))
        }

        transform(scrutinee).run { value =>
          Switch(value, transformedClauses, transformedDefault)
        }

      case lifted.Try(lifted.BlockLit(tparams, List(ev, id), body), List(handler)) =>
        // TODO more than one handler
        val variable = Variable(freshName("a"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val delimiter = Variable(freshName("returnClause"), Type.Stack())
        val regionVar = Variable(freshName("_"), Type.Region())

        LiteralEvidence(transform(ev), builtins.There,
          NewStack(delimiter, regionVar, returnClause,
            PushStack(delimiter,
              New(transform(id), transform(handler),
                transform(body)))))

      case lifted.Region(lifted.BlockLit(tparams, List(ev, id), body)) =>
        val variable = Variable(freshName("a"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val delimiter = Variable(freshName("returnClause"), Type.Stack())
        val regionVar = Variable(transform(id.id), Type.Region())

        LiteralEvidence(transform(ev), builtins.There,
          NewStack(delimiter, regionVar, returnClause,
            PushStack(delimiter, transform(body))))

      case lifted.State(id, init, region, body) =>
        transform(init).run { value =>
          val tpe = value.tpe;
          val name = transform(id)
          val variable = Variable(name, tpe)
          val stateVariable = Variable(name + "$State", Type.Reference(tpe))
          val loadVariable = Variable(freshName(name), tpe)
          val getter = Clause(List(),
                        Load(loadVariable, stateVariable,
                          Return(List(loadVariable))))

          val setterVariable = Variable(freshName(name), tpe)
          val setter = Clause(List(setterVariable),
                                Store(stateVariable, setterVariable,
                                  Return(List())))
          val regionVar = Variable(transform(region), Type.Region())

          // TODO use interface when it's implemented
          Allocate(stateVariable, value, regionVar,
            //New(variable, List(getter, setter),
              transform(body))
        }

      case _ =>
        ErrorReporter.abort(s"Unsupported statement: $stmt")
    }

  def transform(arg: lifted.Argument)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Binding[Variable] = arg match {
    case expr: lifted.Expr => transform(expr)
    case block: lifted.Block => transform(block)
    case lifted.Evidence(scopes) => {
      scopes.map({ scope =>
        Variable(transform(scope), builtins.Evidence)
      }).foldRight({
        val res = Variable(freshName("ev_zero"), builtins.Evidence)
        Binding { k =>
          LiteralEvidence(res, builtins.Here, k(res))
        }: Binding[Variable]
      })({(evi, acc) =>
        val res = Variable(freshName("ev_acc"), builtins.Evidence)
        acc.flatMap({accV => Binding { k =>
          ComposeEvidence(res, evi, accV, k(res))
        }})
      })
    }
  }

  def transform(block: lifted.Block)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Binding[Variable] = block match {
    case lifted.BlockVar(id, tpe) =>
      pure(Variable(transform(id), transform(tpe)))

    case lifted.BlockLit(tparams, params, body) =>
      val parameters = params.map(transform);
      val variable = Variable(freshName("g"), Negative("<function>"))
      Binding { k =>
        New(variable, List(Clause(parameters, transform(body))), k(variable))
      }

    case lifted.Member(b, field, annotatedTpe) => ???
    case lifted.Unbox(e) => ???
    case lifted.New(impl) => ???
  }

  def transform(expr: lifted.Expr)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Binding[Variable] = expr match {
    case lifted.ValueVar(id, tpe) =>
      pure(Variable(transform(id), transform(tpe)))

    case lifted.Literal((), _) =>
      val variable = Variable(freshName("x"), Positive("Unit"));
      Binding { k =>
        Construct(variable, builtins.Unit, List(), k(variable))
      }

    case lifted.Literal(value: Int, _) =>
      val variable = Variable(freshName("x"), Type.Int());
      Binding { k =>
        LiteralInt(variable, value, k(variable))
      }

    case lifted.Literal(value: Boolean, _) =>
      val variable = Variable(freshName("x"), Positive("Boolean"))
      Binding { k =>
        Construct(variable, if (value) builtins.True else builtins.False, List(), k(variable))
      }

    case lifted.Literal(v: Double, _) =>
      val literal_binding = Variable(freshName("x"), Type.Double());
      Binding { k =>
        LiteralDouble(literal_binding, v, k(literal_binding))
      }

    case lifted.Literal(javastring: String, _) =>
      val literal_binding = Variable(freshName("utf8_string_literal"), Type.String());
      Binding { k =>
        LiteralUTF8String(literal_binding, javastring.getBytes("utf-8"), k(literal_binding))
      }

    // hardcoded translation for get and put.
    // TODO remove this when interfaces are correctly translated
    case lifted.PureApp(lifted.Member(lifted.BlockVar(x, lifted.BlockType.Interface(_, List(stateType))), TState.get, annotatedTpe), targs, List()) =>
      if(targs.exists(requiresBoxing)){ ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

      val tpe = transform(stateType)
      val variable = Variable(freshName("x"), tpe)
      val stateVariable = Variable(transform(x) + "$State", Type.Reference(tpe))
      Binding { k =>
        Load(variable, stateVariable, k(variable))
      }

    case lifted.PureApp(lifted.Member(lifted.BlockVar(x, lifted.BlockType.Interface(_, List(stateType))), TState.put, annotatedTpe), targs, List(arg)) =>
      if(targs.exists(requiresBoxing)){ ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

      val tpe = transform(stateType)
      val variable = Variable(freshName("x"), Positive("Unit"));
      val stateVariable = Variable(transform(x) + "$State", Type.Reference(tpe))
      transform(arg).flatMap { value =>
        Binding { k =>
          Store(stateVariable, value,
            Construct(variable, builtins.Unit, List(), k(variable)))
        }
      }

    case lifted.PureApp(lifted.BlockVar(blockName: symbols.ExternFunction, tpe: lifted.BlockType.Function), targs, args) =>
      if(targs.exists(requiresBoxing)){ ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

      val variable = Variable(freshName("x"), transform(tpe.result))
      transform(args).flatMap { values =>
        Binding { k =>
          ForeignCall(variable, transform(blockName), values, k(variable))
        }
      }

    case lifted.PureApp(lifted.BlockVar(blockName, tpe: lifted.BlockType.Function), targs, args)
    if DeclarationContext.findConstructor(blockName).isDefined =>
      if(targs.exists(requiresBoxing)){ ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

      val variable = Variable(freshName("x"), transform(tpe.result));
      val tag = DeclarationContext.getConstructorTag(blockName)

      transform(args).flatMap { values =>
        Binding { k =>
          Construct(variable, tag, values, k(variable))
        }
      }

    case lifted.Select(target, field, tpe)
    if DeclarationContext.findField(field).isDefined =>
      // TODO all of this can go away, if we desugar records in the translation to core!
      val fields = DeclarationContext.getField(field).constructor.fields
      val fieldIndex = fields.indexWhere(_.id == field)
      val variables = fields.map { f => Variable(freshName("n"), transform(tpe)) }
      transform(target).flatMap { value =>
        Binding { k =>
          Switch(value, List(0 -> Clause(variables, k(variables(fieldIndex)))), None)
        }
      }

    case lifted.Run(stmt) =>
      // NOTE: `stmt` is guaranteed to be of type `tpe`.
      val variable = Variable(freshName("x"), transform(stmt.tpe))
      Binding { k =>
        PushFrame(Clause(List(variable), k(variable)), transform(stmt))
      }

    case _ =>
      ErrorReporter.abort(s"Unsupported expression: $expr")
  }

  def transform(args: List[lifted.Argument])(using BlocksParamsContext, DeclarationContext, ErrorReporter): Binding[List[Variable]] =
    args match {
      case Nil => pure(Nil)
      case arg :: args => transform(arg).flatMap { value => transform(args).flatMap { values => pure(value :: values) } }
    }

  def transform(handler: lifted.Implementation)(using BlocksParamsContext, DeclarationContext, ErrorReporter): List[Clause] = {
    handler.operations.sortBy[Int]({
      case lifted.Operation(operationName, _) =>
        DeclarationContext.getInterface(handler.interface.name).properties.indexWhere(_.id == operationName)
    }).map({
      case lifted.Operation(operationName, lifted.BlockLit(tparams, params :+ resume, body))=>
        // TODO we assume here that resume is the last param
        // TODO we assume that there are no block params in handlers
        // TODO we assume that evidence has to be passed as first param
        val ev = Variable(freshName("evidence"), builtins.Evidence)
        Clause(ev +: params.map(transform),
          PopStacks(Variable(transform(resume).name, Type.Stack()), ev,
            transform(body)))
      case _ =>
        ErrorReporter.abort(s"Unsupported handler $handler")
    })
  }

  def transform(param: lifted.Param)(using ErrorReporter): Variable =
    param match {
      case lifted.ValueParam(name, tpe) =>
        Variable(transform(name), transform(tpe))
      case lifted.BlockParam(name, tpe) =>
        Variable(transform(name), transform(tpe))
      case lifted.EvidenceParam(name) =>
        Variable(transform(name), builtins.Evidence)
    }

  def transform(tpe: lifted.ValueType)(using ErrorReporter): Type = tpe match {
    case lifted.ValueType.Var(name) => Positive(name.name.name) // assume all value parameters are data
    case lifted.ValueType.Boxed(tpe) => ???
    case lifted.ValueType.Data(symbols.builtins.UnitSymbol, Nil) => builtins.UnitType
    case lifted.ValueType.Data(symbols.builtins.IntSymbol, Nil) => Type.Int()
    case lifted.ValueType.Data(symbols.builtins.BooleanSymbol, Nil) => builtins.BooleanType
    case lifted.ValueType.Data(symbols.builtins.DoubleSymbol, Nil) => Type.Double()
    case lifted.ValueType.Data(symbols.builtins.StringSymbol, Nil) => Type.String()
    case lifted.ValueType.Data(symbol, targs) => Positive(symbol.name.name)
  }

  def transform(tpe: lifted.BlockType)(using ErrorReporter): Type = tpe match {
    case lifted.BlockType.Function(Nil, cparams, vparams, bparams, result) => Negative("<function>")
    case lifted.BlockType.Function(tparams, cparams, vparams, bparams, result) => ???
    case lifted.BlockType.Interface(symbol, targs) => Negative(symbol.name.name)
  }

  def transform(id: Symbol): String =
    s"${id.name}_${id.id}"

  def requiresBoxing(tpe: lifted.ValueType): Boolean = {
    tpe match
      case lifted.ValueType.Var(_) => false // assume by induction all type variables must be data
      case lifted.ValueType.Data(_, args) => {
        args.exists(requiresBoxing)
      }
      case _ => true
  }

  def freshName(baseName: String): String = baseName + "_" + symbols.Symbol.fresh.next()

  def findToplevelBlocksParams(definitions: List[lifted.Definition])(using BlocksParamsContext, ErrorReporter): Unit =
    definitions.foreach {
      case Definition.Def(blockName, lifted.BlockLit(tparams, params, body)) =>
        noteBlockParams(blockName, params.map(transform))
      case _ => ()
    }


  /**
   * Extra info in context
   */

  class BlocksParamsContext() {
    var blocksParams: Map[Symbol, Environment] = Map()
  }

  def DeclarationContext(using DC: DeclarationContext): DeclarationContext = DC

  def noteBlockParams(id: Symbol, params: Environment)(using BC: BlocksParamsContext): Unit = {
    BC.blocksParams = BC.blocksParams + (id -> params)
  }

  def getBlocksParams(id: Symbol)(using BC: BlocksParamsContext): Environment = {
    // TODO what if this is not found?
    BC.blocksParams(id)
  }

  case class Binding[A](run: (A => Statement) => Statement) {
    def flatMap[B](rest: A => Binding[B]): Binding[B] = {
      Binding(k => run(a => rest(a).run(k)))
    }
  }

  def pure[A](a: A): Binding[A] = Binding(k => k(a))
}
