package effekt
package machine

import effekt.PhaseResult.CoreLifted

import scala.collection.mutable
import effekt.context.Context
import effekt.lifted.{ DeclarationContext, Definition, Lift, LiftInference, given }
import effekt.lifted
import effekt.symbols
import effekt.symbols.{ Symbol, TermSymbol }
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
    val evidence = Variable(freshName("ev"), builtins.Evidence)
    val mainEntry = LiteralEvidence(evidence, builtins.Here, Jump(Label(mainName, List(evidence))))

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
    case lifted.Extern.Def(name, tps, params, ret, body) =>
      val transformedParams = params.flatMap {
        case lifted.ValueParam(id, tpe) => Some(Variable(id.name.name, transform(tpe)))
        case lifted.BlockParam(id, tpe) => ErrorReporter.abort("Foreign functions currently cannot take block arguments.")
        // for now, in machine we do not pass evidence to externs
        case lifted.EvidenceParam(id) => None // Variable(id.name.name, builtins.Evidence)
      }
      noteDefinition(name, params map transform, Nil)
      val tBody = body match {
        case lifted.ExternBody.StringExternBody(ff, Template(strings, args)) =>
          ExternBody.StringExternBody(ff, Template(strings, args map {
            case lifted.ValueVar(id, tpe) => Variable(id.name.name, transform(tpe))
            case _ => ErrorReporter.abort("In the LLVM backend, only variables are allowed in templates")
          }))
        case lifted.ExternBody.Unsupported(err) =>
          ExternBody.Unsupported(err)
      }
      Extern(transform(name), transformedParams, transform(ret), tBody)

    case lifted.Extern.Include(ff, contents) =>
      Include(ff, contents)
  }

  def transform(stmt: lifted.Stmt)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Statement =
    stmt match {
      case lifted.Scope(definitions, rest) =>

        definitions.foreach {
          case Definition.Def(id,  block @ lifted.BlockLit(tparams, params, body)) =>

            noteParameters(params)

            // TODO does not work for mutually recursive local definitions
            val freeParams = lifted.freeVariables(block).toList.toSet.flatMap {
              case lifted.ValueParam(id, tpe) => Set(Variable(transform(id), transform(tpe)))
              case lifted.EvidenceParam(id) => Set(Variable(transform(id), builtins.Evidence))

              // TODO is this necessary???
              case lifted.BlockParam(pid, lifted.BlockType.Interface(tpe, List(stTpe))) if tpe == symbols.builtins.TState.interface =>
                Set(Variable(transform(pid), Type.Reference(transform(stTpe))))

              case lifted.BlockParam(pid, tpe) if pid != id => BPC.info(pid) match {
                  case BlockInfo.Definition(freeParams, blockParams) =>
                    BPC.freeParams(pid).toSet
                  case BlockInfo.Parameter(tpe) =>
                    Set(Variable(transform(pid), transform(tpe)))
                  case BlockInfo.Resumption =>
                    Set(Variable(transform(pid), Type.Stack()))
                }

              case _ => Set.empty
            }

            noteDefinition(id, params.map(transform), freeParams.toList)
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

      case lifted.Val(id, binding, lifted.Return(lifted.ValueVar(id2, tpe))) if id == id2 =>
        transform(binding)

      case lifted.Val(id, binding, rest) =>
        PushFrame(
          Clause(List(transform(lifted.ValueParam(id, binding.tpe))), transform(rest)),
            transform(binding)
        )

      // TODO deal with BlockLit
      case lifted.App(lifted.BlockVar(id, tpe), targs, args) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        BPC.info(id) match {
          // Unknown Jump
          case BlockInfo.Parameter(t : lifted.BlockType.Function) =>
            transform(args).run { values =>
              Invoke(Variable(transform(id), transform(tpe)), builtins.Apply, values)
            }

          case BlockInfo.Parameter(t : lifted.BlockType.Interface) =>
            E.panic("Cannot call an object (need to select a member first)")

          // Continuation Call
          case BlockInfo.Resumption =>
            // TODO currently only scoped resumptions are supported
            // TODO assuming first parameter is evidence TODO actually use evidence?
            transform(args).run { values =>
              val (evidence :: returnedValues) = values: @unchecked;
              PushStack(Variable(transform(id), Type.Stack()),
                Return(returnedValues))
            }

          // Known Jump
          case BlockInfo.Definition(freeParams, blockParams) =>
            val environment = blockParams ++ freeParams
            transform(args).run { values =>
              // Here we actually need a substitution to prepare the environment for the jump
              Substitute(environment.zip(values), Jump(Label(transform(id), environment)))
            }
        }

      case lifted.App(lifted.Unbox(e), targs, args) =>
        transform(e).run { x =>
          transform(args).run { values =>
            Invoke(x, builtins.Apply, values)
          }
        }

      // hardcoded translation for get and put.
      // TODO remove this when interfaces are correctly translated
      case lifted.App(lifted.Member(lifted.BlockVar(x, lifted.BlockType.Interface(_, List(stateType))), TState.get, annotatedTpe), targs, List(ev)) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        val tpe = transform(stateType)
        val variable = Variable(freshName("app"), tpe)
        val reference = Variable(transform(x), Type.Reference(tpe))
        transform(ev).run { evValue =>
          Load(variable, reference, evValue, Return(List(variable)))
        }

      case lifted.App(lifted.Member(lifted.BlockVar(x, lifted.BlockType.Interface(_, List(stateType))), TState.put, annotatedTpe), targs, List(ev, arg)) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        val tpe = transform(stateType)
        val variable = Variable(freshName("app"), Positive());
        val reference = Variable(transform(x), Type.Reference(tpe))
        transform(arg).run { value =>
          transform(ev).run { evValue =>
            Store(reference, value, evValue,
              Construct(variable, builtins.Unit, List(), Return(List(variable))))
          }
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

      case lifted.App(lifted.Member(lifted.Unbox(lifted.ValueVar(id, lifted.ValueType.Boxed(tpe))), op, annotatedTpe), targs, args) =>
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

      case lifted.Try(lifted.BlockLit(tparams, ev :: ids, body), handlers) =>

        noteParameters(ids)

        val variable = Variable(freshName("try"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val delimiter = Variable(freshName("returnClause"), Type.Stack())
        val prompt = Variable(freshName("prompt"), builtins.Prompt)

        LiteralEvidence(transform(ev), builtins.There,
          FreshPrompt(prompt,
            NewStack(delimiter, prompt, returnClause,
              PushStack(delimiter,
                (ids zip handlers).foldRight(transform(body)){
                  case ((id, handler), body) =>
                    New(transform(id), transform(handler, Some(prompt)), body)
                }))))

      // TODO what about the evidence passed to resume?
      case lifted.Shift(ev, lifted.Block.BlockLit(tparams, List(kparam), body)) =>
        noteResumption(kparam.id)
        transform(ev).run { evValue =>
          PopStacks(Variable(transform(kparam).name, Type.Stack()), evValue,
            transform(body))
        }

      case lifted.Region(lifted.BlockLit(tparams, List(ev, id), body)) =>
        val variable = Variable(freshName("region"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val delimiter = Variable(freshName("returnClause"), Type.Stack())
        val prompt = Variable(freshName("prompt"), builtins.Prompt)

        LiteralEvidence(transform(ev), builtins.There,
          FreshPrompt(prompt,
            NewStack(delimiter, prompt, returnClause,
              PushStack(delimiter, transform(body)))))

      case lifted.Alloc(id, init, region, ev, body) =>
        transform(init).run { value =>
          transform(ev).run { evValue =>
            val tpe = value.tpe;
            val name = transform(id)
            val variable = Variable(name, tpe)
            val reference = Variable(transform(id), Type.Reference(tpe))
            val loadVariable = Variable(freshName(name), tpe)
            val getter = Clause(List(),
                          Load(loadVariable, reference, evValue,
                            Return(List(loadVariable))))

            val setterVariable = Variable(freshName(name), tpe)
            val setter = Clause(List(setterVariable),
                                  Store(reference, setterVariable, evValue,
                                    Return(List())))

            // TODO use interface when it's implemented
            Allocate(reference, value, evValue,
              //New(variable, List(getter, setter),
                transform(body))
          }
        }

      case lifted.Var(init, lifted.BlockLit(List(), List(ev, id), body)) =>
        val stateType = transform(init.tpe)
        val reference = Variable(transform(id).name, Type.Reference(stateType))
        val evidence = transform(ev)

        transform(init).run { value =>
          LiteralEvidence(evidence, 0,
            Allocate(reference, value, evidence,
                transform(body)))
        }

      case lifted.Get(id, ev, tpe) =>
        val stateType = transform(tpe)
        val reference = Variable(transform(id), Type.Reference(stateType))
        val variable = Variable(freshName("get"), stateType)

        transform(ev).run { evidence =>
          Load(variable, reference, evidence,
            Return(List(variable)))
        }

      case lifted.Put(id, ev, arg) =>
        val stateType = transform(arg.tpe)
        val reference = Variable(transform(id), Type.Reference(stateType))
        val variable = Variable(freshName("put"), Positive())

        transform(arg).run { value =>
          transform(ev).run { evidence =>
            Store(reference, value, evidence,
              Construct(variable, builtins.Unit, List(),
                Return(List(variable))))
          }
        }

      case lifted.Hole() => machine.Statement.Hole

      case _ =>
        ErrorReporter.abort(s"Unsupported statement: $stmt")
    }

  def transform(arg: lifted.Argument)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Binding[Variable] = arg match {
    case expr: lifted.Expr => transform(expr)
    case block: lifted.Block => transform(block)
    case lifted.Evidence(scopes) => transform(scopes)
  }

  def transform(scopes: List[lifted.Lift])(using ErrorReporter): Binding[Variable] = scopes match {
    case Nil =>
      val name = Variable(freshName("evidenceZero"), builtins.Evidence)
      Binding { k => LiteralEvidence(name, builtins.Here, k(name)) }
    case lift :: Nil =>
      pure(transform(lift))
    case lift :: rest =>
      val name = Variable(freshName("evidenceComposed"), builtins.Evidence)
      Binding { k =>
        transform(rest).run { value =>
          ComposeEvidence(name, transform(lift), value, k(name))
        }
      }
  }

  def transform(lift: lifted.Lift)(using ErrorReporter): Variable = lift match {
    case Lift.Var(name) => Variable(transform(name), builtins.Evidence)
    case Lift.Try() => ErrorReporter.abort(s"Unsupported lift: $lift")
    case Lift.Reg() => ErrorReporter.abort(s"Unsupported lift: $lift")
  }

  def transform(block: lifted.Block)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] = block match {
    case lifted.BlockVar(id, tpe) if isDefinition(id) =>
      // passing a function directly, so we need to eta-expand
      // TODO cache the closure somehow to prevent it from being created on every call
      val parameters = BPC.blockParams(id)
      val variable = Variable(freshName(id.name.name ++ "$closure"), Negative())
      val environment = getBlocksParams(id)
      Binding { k =>
        New(variable, List(Clause(parameters,
          // conceptually: Substitute(parameters zip parameters, Jump(...)) but the Substitute is a no-op here
          Jump(Label(transform(id), environment))
        )), k(variable))
      }

    case lifted.BlockVar(id, tpe) =>
      pure(Variable(transform(id), transform(tpe)))

    case lifted.BlockLit(tparams, params, body) =>
      noteParameters(params)
      val parameters = params.map(transform);
      val variable = Variable(freshName("blockLit"), Negative())
      Binding { k =>
        New(variable, List(Clause(parameters, transform(body))), k(variable))
      }

    case lifted.New(impl) =>
      val variable = Variable(freshName("new"), Negative())
      Binding { k =>
        New(variable, transform(impl, None), k(variable))
      }

    case lifted.Member(b, field, annotatedTpe) => ???
    case lifted.Unbox(e) => ???
  }

  def transform(expr: lifted.Expr)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Binding[Variable] = expr match {
    case lifted.ValueVar(id, tpe) =>
      pure(Variable(transform(id), transform(tpe)))

    case lifted.Literal((), _) =>
      val variable = Variable(freshName("literal"), Positive());
      Binding { k =>
        Construct(variable, builtins.Unit, List(), k(variable))
      }

    case lifted.Literal(value: Long, _) =>
      val variable = Variable(freshName("longLiteral"), Type.Int());
      Binding { k =>
        LiteralInt(variable, value, k(variable))
      }

    // for characters
    case lifted.Literal(value: Int, _) =>
      val variable = Variable(freshName("intLiteral"), Type.Int());
      Binding { k =>
        LiteralInt(variable, value, k(variable))
      }

    case lifted.Literal(value: Boolean, _) =>
      val variable = Variable(freshName("booleanLiteral"), Positive())
      Binding { k =>
        Construct(variable, if (value) builtins.True else builtins.False, List(), k(variable))
      }

    case lifted.Literal(v: Double, _) =>
      val literal_binding = Variable(freshName("doubleLiteral"), Type.Double());
      Binding { k =>
        LiteralDouble(literal_binding, v, k(literal_binding))
      }

    case lifted.Literal(javastring: String, _) =>
      val literal_binding = Variable(freshName("utf8StringLiteral"), Type.String());
      Binding { k =>
        LiteralUTF8String(literal_binding, javastring.getBytes("utf-8"), k(literal_binding))
      }

    case lifted.PureApp(lifted.BlockVar(blockName: symbols.ExternFunction, tpe: lifted.BlockType.Function), targs, args) =>
      if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

      val variable = Variable(freshName("pureApp"), transform(tpe.result))
      transform(args).flatMap { values =>
        Binding { k =>
          ForeignCall(variable, transform(blockName), values, k(variable))
        }
      }

    case lifted.Make(data, constructor, args) =>
      val variable = Variable(freshName("make"), transform(data));
      val tag = DeclarationContext.getConstructorTag(constructor)

      transform(args).flatMap { values =>
        Binding { k =>
          Construct(variable, tag, values, k(variable))
        }
      }

    case lifted.Select(target, field, tpe) if DeclarationContext.findField(field).isDefined =>
      // TODO all of this can go away, if we desugar records in the translation to core!
      val fields = DeclarationContext.getField(field).constructor.fields
      val fieldIndex = fields.indexWhere(_.id == field)
      val variables = fields.map { f => Variable(freshName("select"), transform(tpe)) }
      transform(target).flatMap { value =>
        Binding { k =>
          Switch(value, List(0 -> Clause(variables, k(variables(fieldIndex)))), None)
        }
      }

    case lifted.Run(stmt) =>
      // NOTE: `stmt` is guaranteed to be of type `tpe`.
      val variable = Variable(freshName("run"), transform(stmt.tpe))
      Binding { k =>
        PushFrame(Clause(List(variable), k(variable)), transform(stmt))
      }

    case lifted.Box(block) =>
      transform(block)

    case _ =>
      ErrorReporter.abort(s"Unsupported expression: $expr")
  }

  def transform(args: List[lifted.Argument])(using BlocksParamsContext, DeclarationContext, ErrorReporter): Binding[List[Variable]] =
    args match {
      case Nil => pure(Nil)
      case arg :: args => transform(arg).flatMap { value => transform(args).flatMap { values => pure(value :: values) } }
    }

  def transform(impl: lifted.Implementation, prompt: Option[Variable])(using BlocksParamsContext, DeclarationContext, ErrorReporter): List[Clause] =
    impl.operations.sortBy {
      case lifted.Operation(operationName, _) =>
        DeclarationContext.getInterface(impl.interface.name).properties.indexWhere(_.id == operationName)
    }.map(op => transform(op, prompt))

  def transform(op: lifted.Operation, prompt: Option[Variable])(using BlocksParamsContext, DeclarationContext, ErrorReporter): Clause =
    (prompt, op) match {
      // Since at the moment shift is evidence and not prompt based, here we inline the implementation of shift
      case (Some(prompt), lifted.Operation(name, lifted.BlockLit(tparams, params, lifted.Shift(ev, lifted.Block.BlockLit(tparams2, List(kparam), body))))) =>
        noteResumption(kparam.id)
        Clause(params.map(transform),
          PopStacksPrompt(Variable(transform(kparam).name, Type.Stack()), prompt,
            transform(body)))

      // fall back to evidence based solution, this makes it easier to comment out the above line and check whether the evidence
      // version still works.
      case (_, lifted.Operation(name, lifted.BlockLit(tparams, params, body))) =>
        // TODO note block parameters
        Clause(params.map(transform), transform(body))
    }

  def transform(param: lifted.Param)(using BlocksParamsContext, ErrorReporter): Variable =
    param match {
      case lifted.ValueParam(name, tpe) =>
        Variable(transform(name), transform(tpe))
      case lifted.BlockParam(name, tpe) =>
        Variable(transform(name), transform(tpe))
      case lifted.EvidenceParam(name) =>
        Variable(transform(name), builtins.Evidence)
    }

  def transform(tpe: lifted.ValueType)(using ErrorReporter): Type = tpe match {
    case lifted.ValueType.Var(name) => Positive() // assume all value parameters are data
    case lifted.ValueType.Boxed(tpe) => Negative()
    case lifted.Type.TUnit => builtins.UnitType
    case lifted.Type.TInt => Type.Int()
    case lifted.Type.TChar => Type.Int()
    case lifted.Type.TByte => Type.Byte()
    case lifted.Type.TBoolean => builtins.BooleanType
    case lifted.Type.TDouble => Type.Double()
    case lifted.Type.TString => Type.String()
    case lifted.ValueType.Data(symbol, targs) => Positive()
  }

  def transform(tpe: lifted.BlockType)(using ErrorReporter): Type = tpe match {
    case lifted.BlockType.Function(tparams, cparams, vparams, bparams, result) => Negative()
    case lifted.BlockType.Interface(symbol, targs) => Negative()
  }

  def transform(id: Symbol): String =
    s"${id.name}_${id.id}"

  def requiresBoxing(tpe: lifted.ValueType): Boolean = {
    tpe match
      case lifted.ValueType.Var(_) => false // assume by induction all type variables must be data
      case lifted.ValueType.Data(_, args) => {
        args.exists(requiresBoxing)
      }
      case lifted.ValueType.Boxed(_) => false // TODO check somehow?
  }

  def freshName(baseName: String): String = baseName + "_" + symbols.Symbol.fresh.next()

  def findToplevelBlocksParams(definitions: List[lifted.Definition])(using BlocksParamsContext, ErrorReporter): Unit =
    definitions.foreach {
      case Definition.Def(blockName, lifted.BlockLit(tparams, params, body)) =>
        noteDefinition(blockName, params.map(transform), Nil)
        noteParameters(params)
      case _ => ()
    }


  /**
   * Extra info in context
   */

  class BlocksParamsContext() {
    var info: Map[Symbol, BlockInfo] = Map()

    def definition(id: Symbol): BlockInfo.Definition = info(id) match {
      case d : BlockInfo.Definition => d
      case BlockInfo.Parameter(tpe) => sys error s"Expected a function definition, but got a block parameter: ${id}"
      case BlockInfo.Resumption => sys error s"Expected a function definition, but got a continuation: ${id}"
    }
    def blockParams(id: Symbol): Environment = definition(id).blockParams
    def freeParams(id: Symbol): Environment = definition(id).freeParams
  }
  enum BlockInfo {
    case Definition(freeParams: Environment, blockParams: Environment)
    case Parameter(tpe: lifted.BlockType)
    case Resumption
  }

  def DeclarationContext(using DC: DeclarationContext): DeclarationContext = DC

  def noteDefinition(id: Symbol, blockParams: Environment, freeParams: Environment)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: BlockInfo)")
    BC.info += (id -> BlockInfo.Definition(freeParams, blockParams))

  def noteResumption(id: Symbol)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: Resumption)")
    BC.info += (id -> BlockInfo.Resumption)


  def noteParameter(id: Symbol, tpe: lifted.BlockType)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: Parameter)")
    BC.info += (id -> BlockInfo.Parameter(tpe))

  def noteParameters(ps: List[lifted.Param])(using BC: BlocksParamsContext): Unit =
    ps.foreach {
      case lifted.Param.BlockParam(id, tpe) => noteParameter(id, tpe)
      case lifted.Param.ValueParam(id, tpe) => () // do not register value parameters
      case lifted.Param.EvidenceParam(id) => () // do not register evidence parameters
    }

  def getBlocksParams(id: Symbol)(using BC: BlocksParamsContext): Environment = BC.definition(id) match {
    case BlockInfo.Definition(freeParams, blockParams) => blockParams ++ freeParams
  }

  def isResumption(id: Symbol)(using BC: BlocksParamsContext): Boolean =
    BC.info(id) match {
      case BlockInfo.Resumption => true
      case _ => false
    }

  def isDefinition(id: Symbol)(using BC: BlocksParamsContext): Boolean =
    BC.info(id) match {
      case d: BlockInfo.Definition => true
      case _ => false
    }

  case class Binding[A](run: (A => Statement) => Statement) {
    def flatMap[B](rest: A => Binding[B]): Binding[B] = {
      Binding(k => run(a => rest(a).run(k)))
    }
  }

  def pure[A](a: A): Binding[A] = Binding(k => k(a))
}
