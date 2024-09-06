package effekt
package machine

import effekt.context.Context
import effekt.core.{ DeclarationContext, Definition, given }
import effekt.symbols.{ Symbol, TermSymbol }
import effekt.symbols.builtins.TState
import effekt.util.messages.ErrorReporter

object Transformer {

  private def ErrorReporter(using E: ErrorReporter): ErrorReporter = E

  def transform(main: CoreTransformed, mainSymbol: TermSymbol)(using C: Context): Program = {
    C.using(module = main.mod) {
      transform(mainSymbol, main.core);
    }
  }

  def transform(mainSymbol: TermSymbol, mod: core.ModuleDecl)(using E: ErrorReporter): Program = {

    val mainName = transform(mainSymbol)
    given BC: BlocksParamsContext = BlocksParamsContext();
    given DC: DeclarationContext = core.DeclarationContext(mod.declarations, mod.externs)

    // collect all information
    val declarations = mod.externs.map(transform)
    val definitions = mod.definitions
    val mainEntry = Jump(Label(mainName, Nil))

    // println(core.PrettyPrinter.show(mod))

    findToplevelBlocksParams(definitions)

    val transformedDefinitions = definitions.foldLeft(mainEntry) {
      case (rest, core.Definition.Def(id, core.BlockLit(tparams, cparams, vparams, bparams, body))) =>
        Def(Label(transform(id), vparams.map(transform) ++ bparams.map(transform)), transform(body), rest)
      case (rest, d) =>
        ErrorReporter.abort(s"Toplevel def and let bindings not yet supported: ${d}")
    }

    Program(declarations, transformedDefinitions)
  }

  def transform(extern: core.Extern)(using BlocksParamsContext, ErrorReporter): Declaration = extern match {
    case core.Extern.Def(name, tps, cparams, vparams, bparams, ret, capture, body) =>
      if (bparams.nonEmpty) then ErrorReporter.abort("Foreign functions currently cannot take block arguments.")

      val transformedParams = vparams.map {
        //  we do not use transform(ValueParam) here because we use the original name (e.g. %x and not %x1234)
        case core.ValueParam(id, tpe) => Variable(id.name.name, transform(tpe))
      }
      noteDefinition(name, transformedParams, Nil) // TODO maybe use vparams.map(transform) here
      val tBody = body match {
        case core.ExternBody.StringExternBody(ff, Template(strings, args)) =>
          ExternBody.StringExternBody(ff, Template(strings, args map {
            case core.ValueVar(id, tpe) => Variable(id.name.name, transform(tpe))
            case _ => ErrorReporter.abort("In the LLVM backend, only variables are allowed in templates")
          }))
        case core.ExternBody.Unsupported(err) =>
          ExternBody.Unsupported(err)
      }
      Extern(transform(name), transformedParams, transform(ret), tBody)

    case core.Extern.Include(ff, contents) =>
      Include(ff, contents)
  }

  def transform(stmt: core.Stmt)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Statement =
    stmt match {
      case core.Scope(definitions, rest) =>

        definitions.foreach {
          case Definition.Def(id,  block @ core.BlockLit(tparams, cparams, vparams, bparams, body)) =>

            noteParameters(bparams)

            // TODO does not work for mutually recursive local definitions
            val freeParams = core.Variables.free(block).toSet.flatMap {
              case core.Variable.Value(id, tpe) => Set(Variable(transform(id), transform(tpe)))

              // TODO is this necessary???
              case core.Variable.Block(id, core.BlockType.Interface(tpe, List(stTpe)), capt)
                if tpe == symbols.builtins.TState.interface => Set(Variable(transform(id), Type.Reference(transform(stTpe))))

              case core.Variable.Block(pid, tpe, capt) if pid != id => BPC.info(pid) match {
                  case BlockInfo.Definition(freeParams, blockParams) =>
                    BPC.freeParams(pid).toSet
                  case BlockInfo.Parameter(tpe) =>
                    Set(Variable(transform(pid), transform(tpe)))
                  case BlockInfo.Resumption =>
                    Set(Variable(transform(pid), Type.Stack()))
                }
              case _ => Set.empty
            }

            noteDefinition(id, vparams.map(transform) ++ bparams.map(transform), freeParams.toList)
          case _ => ()
        }

        definitions.foldRight(transform(rest)) {
          case (core.Definition.Let(id, tpe, binding), rest) =>
            transform(binding).run { value =>
              // TODO consider passing the environment to [[transform]] instead of explicit substitutions here.
              Substitute(List(Variable(transform(id), transform(binding.tpe)) -> value), rest)
            }

          case (core.Definition.Def(id, block @ core.BlockLit(tparams, cparams, vparams, bparams, body)), rest) =>
            Def(Label(transform(id), getBlocksParams(id)), transform(body), rest)

          case (core.Definition.Def(id, block @ core.New(impl)), rest) =>
            val interfaceId = impl.interface.name
            // TODO freeParams?
            // TODO deal with evidence?
            val properties = DeclarationContext.getInterface(interfaceId).properties
            val implTransformed = properties.map({ prop =>
              impl.operations.find(_._1 == prop.id).get
            }).map({
              case core.Operation(_, tparams, cparams, vparams, bparams, kparam, body) =>
                assert(kparam.isEmpty)
                // TODO we assume that there are no block params in methods
                Clause(vparams.map(transform) ++ bparams.map(transform), transform(body))
            })
            New(Variable(transform(id), transform(impl.interface)), implTransformed, rest)

          case (d @ core.Definition.Def(_, _: core.BlockVar | _: core.Member | _: core.Unbox), rest) =>
            ErrorReporter.abort(s"block definition: $d")
        }

      case core.Return(expr) =>
        transform(expr).run { value => Return(List(value)) }

      // TODO check whether the optimizer already removes this as part of normalization
      case core.Val(id, annot, binding, core.Return(core.ValueVar(id2, tpe))) if id == id2 =>
        transform(binding)

      case core.Val(id, annot, binding, rest) =>
        PushFrame(
          Clause(List(Variable(transform(id), transform(binding.tpe))), transform(rest)),
            transform(binding)
        )

      // TODO deal with BlockLit
      case core.App(core.BlockVar(id, tpe, capt), targs, vargs, bargs) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        BPC.info(id) match {
          // Unknown Jump
          case BlockInfo.Parameter(t : core.BlockType.Function) =>
            transform(vargs, bargs).run { (values, blocks) =>
              Invoke(Variable(transform(id), transform(tpe)), builtins.Apply, values ++ blocks)
            }

          case BlockInfo.Parameter(t : core.BlockType.Interface) =>
            E.panic("Cannot call an object (need to select a member first)")

          // Continuation Call
          case BlockInfo.Resumption =>
            // TODO what to do when blocks is non empty?
            transform(vargs, bargs).run { (values, blocks) =>
              PushStack(Variable(transform(id), Type.Stack()),
                Return(values))
            }

          // Known Jump
          case BlockInfo.Definition(freeParams, blockParams) =>
            val environment = blockParams ++ freeParams
            transform(vargs, bargs).run { (values, blocks) =>
              val args = values ++ blocks
              // Here we actually need a substitution to prepare the environment for the jump
              Substitute(environment.zip(args), Jump(Label(transform(id), environment)))
            }
        }

      case core.App(core.Unbox(e), targs, vargs, bargs) =>
        transform(e).run { x =>
          transform(vargs, bargs).run { (values, blocks) =>
            Invoke(x, builtins.Apply, values ++ blocks)
          }
        }

      // hardcoded translation for get and put.
      // TODO remove this when interfaces are correctly translated
      case core.App(core.Member(core.BlockVar(x, core.BlockType.Interface(_, List(stateType)), _), TState.get, annotatedTpe), targs, Nil, Nil) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        val tpe = transform(stateType)
        val variable = Variable(freshName("app"), tpe)
        val reference = Variable(transform(x), Type.Reference(tpe))
        ??? // Load(variable, reference, evValue, Return(List(variable)))

      case core.App(core.Member(core.BlockVar(x, core.BlockType.Interface(_, List(stateType)), _), TState.put, annotatedTpe), targs, List(arg), Nil) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        val tpe = transform(stateType)
        val variable = Variable(freshName("app"), Positive());
        val reference = Variable(transform(x), Type.Reference(tpe))
        transform(arg).run { value =>
          //          Store(reference, value, evValue,
          //            Construct(variable, builtins.Unit, List(), Return(List(variable))))
          ???
        }

      case core.App(core.Member(core.BlockVar(id, tpe, capt), op, annotatedTpe), targs, vargs, bargs) =>
        if(targs.exists(requiresBoxing)){ ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }
        val opTag = {
          tpe match
            case core.BlockType.Interface(ifceId, _) =>
              DeclarationContext.getPropertyTag(op)
            case _ => ErrorReporter.abort(s"Unsupported receiver type $tpe")
        }
        transform(vargs, bargs).run { (values, blocks) =>
          Invoke(Variable(transform(id), transform(tpe)), opTag, values ++ blocks)
        }

      case core.App(core.Member(core.Unbox(core.ValueVar(id, core.ValueType.Boxed(tpe, capt))), op, annotatedTpe), targs, vargs, bargs) =>
        if(targs.exists(requiresBoxing)){ ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }
        val opTag = {
          tpe match
            case core.BlockType.Interface(ifceId, _) =>
              DeclarationContext.getPropertyTag(op)
            case _ => ErrorReporter.abort(s"Unsupported receiver type $tpe")
        }
        transform(vargs, bargs).run { (values, blocks) =>
          Invoke(Variable(transform(id), transform(tpe)), opTag, values ++ blocks)
        }

      case core.If(cond, thenStmt, elseStmt) =>
        transform(cond).run { value =>
          Switch(value, List(0 -> Clause(List(), transform(elseStmt)), 1 -> Clause(List(), transform(thenStmt))), None)
        }

      case core.Match(scrutinee, clauses, default) =>
        val transformedClauses = clauses.map { case (constr, core.BlockLit(tparams, cparams, vparams, bparams, body)) =>
          DeclarationContext.getConstructorTag(constr) -> Clause(vparams.map(transform), transform(body))
        }
        val transformedDefault = default.map { clause =>
          Clause(List(), transform(clause))
        }

        transform(scrutinee).run { value =>
          Switch(value, transformedClauses, transformedDefault)
        }

      case core.Try(core.BlockLit(tparams, cparams, vparams, bparams, body), handlers) =>

        noteParameters(bparams)

        val variable = Variable(freshName("try"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val delimiter = Variable(freshName("returnClause"), Type.Stack())
        val prompt = Variable(freshName("prompt"), builtins.Prompt)

        FreshPrompt(prompt,
          NewStack(delimiter, prompt, returnClause,
            PushStack(delimiter,
              (bparams zip handlers).foldRight(transform(body)){
                case ((id, handler), body) =>
                  New(transform(id), transform(handler, Some(prompt)), body)
              })))

      case core.Region(core.BlockLit(tparams, cparams, vparams, bparams, body)) =>
        val variable = Variable(freshName("region"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val delimiter = Variable(freshName("returnClause"), Type.Stack())
        val prompt = Variable(freshName("prompt"), builtins.Prompt)

        FreshPrompt(prompt,
          NewStack(delimiter, prompt, returnClause,
            PushStack(delimiter, transform(body))))

      case core.Alloc(id, init, region, body) =>
        transform(init).run { value =>
          val tpe = value.tpe;
          val name = transform(id)
          val variable = Variable(name, tpe)
          val reference = Variable(transform(id), Type.Reference(tpe))
          val loadVariable = Variable(freshName(name), tpe)
          val getter = Clause(List(),
                        Load(loadVariable, reference, ???,
                          Return(List(loadVariable))))

          val setterVariable = Variable(freshName(name), tpe)
          val setter = Clause(List(setterVariable),
                                Store(reference, setterVariable, ???,
                                  Return(List())))

          // TODO use interface when it's implemented
          Allocate(reference, value, ???,
            //New(variable, List(getter, setter),
              transform(body))
        }

      case core.Var(id, init, capture, body) =>
        val stateType = transform(init.tpe)
        val reference = Variable(transform(id), Type.Reference(stateType))

        transform(init).run { value =>
          Allocate(reference, value, ???,
            transform(body))
        }

      case core.Get(id, capt, tpe) =>
        val stateType = transform(tpe)
        val reference = Variable(transform(id), Type.Reference(stateType))
        val variable = Variable(freshName("get"), stateType)


        Load(variable, reference, ???,
            Return(List(variable)))

      case core.Put(id, capt, arg) =>
        val stateType = transform(arg.tpe)
        val reference = Variable(transform(id), Type.Reference(stateType))
        val variable = Variable(freshName("put"), Positive())

        transform(arg).run { value =>
          Store(reference, value, ???,
            Construct(variable, builtins.Unit, List(),
              Return(List(variable))))
        }

      case core.Hole() => machine.Statement.Hole

      case _ =>
        ErrorReporter.abort(s"Unsupported statement: $stmt")
    }

  def transform(vargs: List[core.Pure], bargs: List[core.Block])(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[(List[Variable], List[Variable])] =
    (vargs, bargs) match {
      case (Nil, Nil) => pure((Nil, Nil))
      case (Nil, barg :: bargs) => transform(barg).flatMap { block =>
        transform(Nil, bargs).flatMap { (values, blocks) => pure(values, block :: blocks) }
      }
      case (varg :: vargs, bargs) => transform(varg).flatMap { value =>
        transform(vargs, bargs).flatMap { (values, blocks) => pure(value :: values, blocks) }
      }
    }

  def transform(block: core.Block)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] = block match {
    case core.BlockVar(id, tpe, capt) if isDefinition(id) =>
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

    case core.BlockVar(id, tpe, capt) =>
      pure(Variable(transform(id), transform(tpe)))

    case core.BlockLit(tparams, cparams, vparams, bparams, body) =>
      noteParameters(bparams)
      val parameters = vparams.map(transform) ++ bparams.map(transform);
      val variable = Variable(freshName("blockLit"), Negative())
      Binding { k =>
        New(variable, List(Clause(parameters, transform(body))), k(variable))
      }

    case core.New(impl) =>
      val variable = Variable(freshName("new"), Negative())
      Binding { k =>
        New(variable, transform(impl, None), k(variable))
      }

    case core.Member(b, field, annotatedTpe) => ???
    case core.Unbox(e) => ???
  }

  def transform(expr: core.Expr)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Binding[Variable] = expr match {
    case core.ValueVar(id, tpe) =>
      pure(Variable(transform(id), transform(tpe)))

    case core.Literal((), _) =>
      val variable = Variable(freshName("literal"), Positive());
      Binding { k =>
        Construct(variable, builtins.Unit, List(), k(variable))
      }

    case core.Literal(value: Long, _) =>
      val variable = Variable(freshName("longLiteral"), Type.Int());
      Binding { k =>
        LiteralInt(variable, value, k(variable))
      }

    // for characters
    case core.Literal(value: Int, _) =>
      val variable = Variable(freshName("intLiteral"), Type.Int());
      Binding { k =>
        LiteralInt(variable, value, k(variable))
      }

    case core.Literal(value: Boolean, _) =>
      val variable = Variable(freshName("booleanLiteral"), Positive())
      Binding { k =>
        Construct(variable, if (value) builtins.True else builtins.False, List(), k(variable))
      }

    case core.Literal(v: Double, _) =>
      val literal_binding = Variable(freshName("doubleLiteral"), Type.Double());
      Binding { k =>
        LiteralDouble(literal_binding, v, k(literal_binding))
      }

    case core.Literal(javastring: String, _) =>
      val literal_binding = Variable(freshName("utf8StringLiteral"), Type.String());
      Binding { k =>
        LiteralUTF8String(literal_binding, javastring.getBytes("utf-8"), k(literal_binding))
      }

    case core.PureApp(core.BlockVar(blockName: symbols.ExternFunction, tpe: core.BlockType.Function, capt), targs, vargs) =>
      if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

      val variable = Variable(freshName("pureApp"), transform(tpe.result))
      transform(vargs, Nil).flatMap { (values, blocks) =>
        Binding { k =>
          ForeignCall(variable, transform(blockName), values ++ blocks, k(variable))
        }
      }

    case core.DirectApp(core.BlockVar(blockName: symbols.ExternFunction, tpe: core.BlockType.Function, capt), targs, vargs, bargs) =>
      if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

      val variable = Variable(freshName("pureApp"), transform(tpe.result))
      transform(vargs, bargs).flatMap { (values, blocks) =>
        Binding { k =>
          ForeignCall(variable, transform(blockName), values ++ blocks, k(variable))
        }
      }

    case core.Make(data, constructor, vargs) =>
      val variable = Variable(freshName("make"), transform(data));
      val tag = DeclarationContext.getConstructorTag(constructor)

      transform(vargs, Nil).flatMap { (values, blocks) =>
        Binding { k =>
          Construct(variable, tag, values ++ blocks, k(variable))
        }
      }

    case core.Select(target, field, tpe) if DeclarationContext.findField(field).isDefined =>
      // TODO all of this can go away, if we desugar records in the translation to core!
      val fields = DeclarationContext.getField(field).constructor.fields
      val fieldIndex = fields.indexWhere(_.id == field)
      val variables = fields.map { f => Variable(freshName("select"), transform(tpe)) }
      transform(target).flatMap { value =>
        Binding { k =>
          Switch(value, List(0 -> Clause(variables, k(variables(fieldIndex)))), None)
        }
      }

    case core.Run(stmt) =>
      // NOTE: `stmt` is guaranteed to be of type `tpe`.
      val variable = Variable(freshName("run"), transform(stmt.tpe))
      Binding { k =>
        PushFrame(Clause(List(variable), k(variable)), transform(stmt))
      }

    case core.Box(block, annot) =>
      transform(block)

    case _ =>
      ErrorReporter.abort(s"Unsupported expression: $expr")
  }

  def transform(impl: core.Implementation, prompt: Option[Variable])(using BlocksParamsContext, DeclarationContext, ErrorReporter): List[Clause] =
    impl.operations.sortBy {
      case core.Operation(operationName, _, _, _, _, _, _) =>
        DeclarationContext.getInterface(impl.interface.name).properties.indexWhere(_.id == operationName)
    }.map(op => transform(op, prompt))

  def transform(op: core.Operation, prompt: Option[Variable])(using BlocksParamsContext, DeclarationContext, ErrorReporter): Clause =
    (prompt, op) match {
      // Since at the moment shift is evidence and not prompt based, here we inline the implementation of shift
      case (Some(prompt), core.Operation(name, tparams, cparams, vparams, bparams, Some(kparam), body)) =>
        noteResumption(kparam.id)
        // TODO deal with bidirectional effects
        Clause(vparams.map(transform),
          PopStacksPrompt(Variable(transform(kparam).name, Type.Stack()), prompt,
            transform(body)))

      // fall back to evidence based solution, this makes it easier to comment out the above line and check whether the evidence
      // version still works.
      case (_, core.Operation(name, tparams, cparams, vparams, bparams, k, body)) =>
        // TODO note block parameters
        Clause(vparams.map(transform) ++ bparams.map(transform), transform(body))
    }

  def transform(param: core.ValueParam)(using BlocksParamsContext, ErrorReporter): Variable =
    param match {
      case core.ValueParam(name, tpe) =>
        Variable(transform(name), transform(tpe))
    }

  def transform(param: core.BlockParam)(using BlocksParamsContext, ErrorReporter): Variable =
    param match {
      case core.BlockParam(name, tpe, capt) =>
        Variable(transform(name), transform(tpe))
    }

  def transform(tpe: core.ValueType)(using ErrorReporter): Type = tpe match {
    case core.ValueType.Var(name) => Positive() // assume all value parameters are data
    case core.ValueType.Boxed(tpe, capt) => Negative()
    case core.Type.TUnit => builtins.UnitType
    case core.Type.TInt => Type.Int()
    case core.Type.TChar => Type.Int()
    case core.Type.TByte => Type.Byte()
    case core.Type.TBoolean => builtins.BooleanType
    case core.Type.TDouble => Type.Double()
    case core.Type.TString => Type.String()
    case core.ValueType.Data(symbol, targs) => Positive()
  }

  def transform(tpe: core.BlockType)(using ErrorReporter): Type = tpe match {
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) => Negative()
    case core.BlockType.Interface(symbol, targs) => Negative()
  }

  def transform(id: Symbol): String =
    s"${id.name}_${id.id}"

  def requiresBoxing(tpe: core.ValueType): Boolean = {
    tpe match
      case core.ValueType.Var(_) => false // assume by induction all type variables must be data
      case core.ValueType.Data(_, args) => {
        args.exists(requiresBoxing)
      }
      case core.ValueType.Boxed(_, _) => false // TODO check somehow?
  }

  def freshName(baseName: String): String = baseName + "_" + symbols.Symbol.fresh.next()

  def findToplevelBlocksParams(definitions: List[core.Definition])(using BlocksParamsContext, ErrorReporter): Unit =
    definitions.foreach {
      case Definition.Def(blockName, core.BlockLit(tparams, cparams, vparams, bparams, body)) =>
        noteDefinition(blockName, vparams.map(transform) ++ bparams.map(transform), Nil)
        noteParameters(bparams)
      case other => ()
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
    case Parameter(tpe: core.BlockType)
    case Resumption
  }

  def DeclarationContext(using DC: DeclarationContext): DeclarationContext = DC

  def noteDefinition(id: Symbol, blockParams: Environment, freeParams: Environment)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: BlockInfo)")
    BC.info += (id -> BlockInfo.Definition(freeParams, blockParams))

  def noteResumption(id: Symbol)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: Resumption)")
    BC.info += (id -> BlockInfo.Resumption)


  def noteParameter(id: Symbol, tpe: core.BlockType)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: Parameter)")
    BC.info += (id -> BlockInfo.Parameter(tpe))

  def noteParameters(ps: List[core.BlockParam])(using BC: BlocksParamsContext): Unit =
    ps.foreach {
      case core.BlockParam(id, tpe, capt) => noteParameter(id, tpe)
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
