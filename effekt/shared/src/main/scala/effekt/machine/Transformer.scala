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
    case lifted.Extern.Def(name, tps, params, ret, body) =>
      val transformedParams = params.flatMap {
        case lifted.ValueParam(id, tpe) => Some(Variable(id.name.name, transform(tpe)))
        case lifted.BlockParam(id, tpe) => ErrorReporter.abort("Foreign functions currently cannot take block arguments.")
        // for now, in machine we do not pass evidence to externs
        case lifted.EvidenceParam(id) => None // Variable(id.name.name, builtins.Evidence)
      }
      noteBlockParams(name, params map transform, List.empty)
      Extern(transform(name), transformedParams, transform(ret), body)

    case lifted.Extern.Include(contents) =>
      Include(contents)
  }

  def transform(stmt: lifted.Stmt)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Statement =
    stmt match {
      case lifted.Scope(definitions, rest) =>

        definitions.foreach {
          case Definition.Def(id,  block @ lifted.BlockLit(tparams, params, body)) =>
            // TODO does not work for mutually recursive local definitions
            val freeParams = lifted.freeVariables(block).toList.toSet.flatMap {
              case lifted.ValueParam(id, tpe) => Set(Variable(transform(id), transform(tpe)))
              case lifted.BlockParam(pid, lifted.BlockType.Interface(tpe, List(stTpe))) if tpe == symbols.builtins.TState.interface =>
                Set(Variable(transform(pid), Type.Reference(transform(stTpe))))
              case lifted.BlockParam(resume: symbols.TrackedParam.ResumeParam, _) =>
                // resume parameters are represented as Stacks in machine
                // TODO How can we not match on the symbol here?
                Set(Variable(transform(resume), Type.Stack()))
              case lifted.BlockParam(pid, tpe)
                if !BPC.blockParams.contains(pid) && id != pid && DC.findConstructor(pid).isEmpty =>
                Set(Variable(transform(pid), transform(tpe)))
              case lifted.BlockParam(pid, tpe) if BPC.freeParams.contains(pid) && id != pid =>
                BPC.freeParams(pid).toSet
              case lifted.EvidenceParam(id) => Set(Variable(transform(id), builtins.Evidence))
              case _ => Set.empty
            }
            noteBlockParams(id, params.map(transform), freeParams.toList)
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


      // hardcoded translation for get and put.
      // TODO remove this when interfaces are correctly translated
      case lifted.App(lifted.Member(lifted.BlockVar(x, lifted.BlockType.Interface(_, List(stateType))), TState.get, annotatedTpe), targs, List(ev)) =>
        if(targs.exists(requiresBoxing)){ ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        val tpe = transform(stateType)
        val variable = Variable(freshName("x"), tpe)
        val reference = Variable(transform(x), Type.Reference(tpe))
        transform(ev).run { evValue =>
          Load(variable, reference, evValue, Return(List(variable)))
        }

      case lifted.App(lifted.Member(lifted.BlockVar(x, lifted.BlockType.Interface(_, List(stateType))), TState.put, annotatedTpe), targs, List(ev, arg)) =>
        if(targs.exists(requiresBoxing)){ ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        val tpe = transform(stateType)
        val variable = Variable(freshName("x"), Positive("Unit"));
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
        val variable = Variable(freshName("a"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val delimiter = Variable(freshName("returnClause"), Type.Stack())

        LiteralEvidence(transform(ev), builtins.There,
          NewStack(delimiter, returnClause,
            PushStack(delimiter,
              (ids zip handlers).foldRight(transform(body)){
                case ((id, handler), body) =>
                  New(transform(id), transform(handler), body)
              })))

      // TODO what about the evidence passed to resume?
      case lifted.Shift(ev, lifted.Block.BlockLit(tparams, List(kparam), body)) =>
        transform(ev).run { evValue =>
          PopStacks(Variable(transform(kparam).name, Type.Stack()), evValue,
            transform(body))
        }

      case lifted.Region(lifted.BlockLit(tparams, List(ev, id), body)) =>
        val variable = Variable(freshName("a"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val delimiter = Variable(freshName("returnClause"), Type.Stack())

        LiteralEvidence(transform(ev), builtins.There,
          NewStack(delimiter, returnClause,
            PushStack(delimiter, transform(body))))

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
        val variable = Variable(freshName("x"), stateType)

        transform(ev).run { evidence =>
          Load(variable, reference, evidence,
            Return(List(variable)))
        }

      case lifted.Put(id, ev, arg) =>
        val stateType = transform(arg.tpe)
        val reference = Variable(transform(id), Type.Reference(stateType))
        val variable = Variable(freshName("x"), Positive("Unit"))

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

  def transform(l: lifted.Lift): Variable = l match {
    case Lift.Var(ev) => Variable(transform(ev), builtins.Evidence)
    case Lift.Try() => ???
    case Lift.Reg() => ???
  }

  def transform(arg: lifted.Argument)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Binding[Variable] = arg match {
    case expr: lifted.Expr => transform(expr)
    case block: lifted.Block => transform(block)
    case lifted.Evidence(scopes) => {
      scopes.map(transform).foldRight {
        val res = Variable(freshName("ev_zero"), builtins.Evidence)
        Binding { k =>
          LiteralEvidence(res, builtins.Here, k(res))
        }: Binding[Variable]
      } { (evi, acc) =>
        val res = Variable(freshName("ev_acc"), builtins.Evidence)
        acc.flatMap({accV => Binding { k =>
          ComposeEvidence(res, evi, accV, k(res))
        }})
      }
    }
  }

  def transform(block: lifted.Block)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] = block match {
    case lifted.BlockVar(id, tpe) if BPC.blockParams.contains(id) =>
      // passing a function directly, so we need to eta-expand
      // TODO cache the closure somehow to prevent it from being created on every call
      val parameters = BPC.blockParams(id)
      val variable = Variable(freshName(id.name.name ++ "$closure"), Negative("<function>"))
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

  def transform(handler: lifted.Implementation)(using BlocksParamsContext, DeclarationContext, ErrorReporter): List[Clause] =
    handler.operations.sortBy {
      case lifted.Operation(operationName, _) =>
        DeclarationContext.getInterface(handler.interface.name).properties.indexWhere(_.id == operationName)
    }.map(transform)

  def transform(op: lifted.Operation)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Clause = op match {
    case lifted.Operation(name, lifted.BlockLit(tparams, params, body)) =>
      Clause(params.map(transform), transform(body))
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
    case lifted.Type.TUnit => builtins.UnitType
    case lifted.Type.TInt => Type.Int()
    case lifted.Type.TBoolean => builtins.BooleanType
    case lifted.Type.TDouble => Type.Double()
    case lifted.Type.TString => Type.String()
    case lifted.ValueType.Data(symbol, targs) => Positive(symbol.name.name)
  }

  def transform(tpe: lifted.BlockType)(using ErrorReporter): Type = tpe match {
    case lifted.BlockType.Function(tparams, cparams, vparams, bparams, result) => Negative("<function>")
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
        noteBlockParams(blockName, params.map(transform), Nil)
      case _ => ()
    }


  /**
   * Extra info in context
   */

  class BlocksParamsContext() {
    var freeParams: Map[Symbol, Environment] = Map()
    var blockParams: Map[Symbol, Environment] = Map()
  }

  def DeclarationContext(using DC: DeclarationContext): DeclarationContext = DC

  def noteBlockParams(id: Symbol, blockParams: Environment, freeParams: Environment)(using BC: BlocksParamsContext): Unit = {
    BC.blockParams = BC.blockParams + (id -> blockParams)
    BC.freeParams = BC.freeParams + (id -> freeParams)
  }

  def getBlocksParams(id: Symbol)(using BC: BlocksParamsContext): Environment = {
    // TODO what if this is not found?
    BC.blockParams(id) ++ BC.freeParams(id)
  }

  case class Binding[A](run: (A => Statement) => Statement) {
    def flatMap[B](rest: A => Binding[B]): Binding[B] = {
      Binding(k => run(a => rest(a).run(k)))
    }
  }

  def pure[A](a: A): Binding[A] = Binding(k => k(a))
}
