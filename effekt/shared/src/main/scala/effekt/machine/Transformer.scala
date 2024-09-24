package effekt
package machine

import effekt.context.Context
import effekt.core.{ DeclarationContext, Definition, Id, given }
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

    findToplevelBlocksParams(definitions)

    val transformedDefinitions = definitions.foldLeft(mainEntry) {
      case (rest, core.Definition.Def(id, core.BlockLit(tparams, cparams, vparams, bparams, body))) =>
        Def(Label(transform(id), vparams.map(transform) ++ bparams.map(transform)), transform(body), rest)
      case (rest, core.Definition.Let(id, tpe, binding)) =>
        Def(BC.globals(id), transform(binding).run { value => Return(List(value)) }, rest)
      case (rest, d) =>
        ErrorReporter.abort(s"Toplevel object definitions not yet supported: ${d}")
    }

    Program(declarations, transformedDefinitions)
  }

  def transform(extern: core.Extern)(using BlocksParamsContext, ErrorReporter): Declaration = extern match {
    case core.Extern.Def(name, tps, cparams, vparams, bparams, ret, capture, body) =>
      if (bparams.nonEmpty) then ErrorReporter.abort("Foreign functions currently cannot take block arguments.")

      val transformedParams = vparams.map(transform)
      noteDefinition(name, transformedParams, Nil)
      val tBody = body match {
        case core.ExternBody.StringExternBody(ff, Template(strings, args)) =>
          ExternBody.StringExternBody(ff, Template(strings, args map {
            case core.ValueVar(id, tpe) => Variable(transform(id), transform(tpe))
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

        // (1) Collect all the information about free variabls of local definitions
        definitions.foreach {
          case Definition.Def(id,  block @ core.BlockLit(tparams, cparams, vparams, bparams, body)) =>

            noteParameters(bparams)

            // TODO use existing lambda lifting code

            // Does not work for mutually recursive local definitions (which are not supported anyway, at the moment)
            val freeVariables = core.Variables.free(block).toSet
            val freeParams = freeVariables.flatMap {
              case core.Variable.Value(id, tpe) =>
                Set(Variable(transform(id), transform(tpe)))

              // Mutable variables are blocks and can be free, but do not have info.
              case core.Variable.Block(id, core.Type.TState(stTpe), capt) =>
                Set(Variable(transform(id), Type.Reference(transform(stTpe))))

              case core.Variable.Block(pid, tpe, capt) if pid != id => BPC.info.get(pid) match {
                  // For each known free block we have to add its free variables to this one (flat closure)
                  case Some(BlockInfo.Definition(freeParams, blockParams)) =>
                    freeParams.toSet

                  // Unknown free blocks stay free variables
                  case Some(BlockInfo.Parameter(tpe)) =>
                    Set(Variable(transform(pid), transform(tpe)))

                  // The resumption is free
                  case Some(BlockInfo.Resumption) =>
                    Set(Variable(transform(pid), Type.Stack()))

                  // Everything else is considered bound or global
                  case None =>
                    // TODO should not happen, abort with error
                    Set.empty
                }
              case _ => Set.empty
            }

            noteDefinition(id, vparams.map(transform) ++ bparams.map(transform), freeParams.toList)
          case _ => ()
        }

        // (2) Actually translate the definitions
        definitions.foldRight(transform(rest)) {
          case (core.Definition.Let(id, tpe, binding), rest) =>
            transform(binding).run { value =>
              // TODO consider passing the environment to [[transform]] instead of explicit substitutions here.
              // TODO it is important that we use the inferred [[binding.tpe]] and not the annotated type [[tpe]], but why?
              Substitute(List(Variable(transform(id), transform(binding.tpe)) -> value), rest)
            }

          case (core.Definition.Def(id, core.BlockLit(tparams, cparams, vparams, bparams, body)), rest) =>
            Def(Label(transform(id), getBlocksParams(id)), transform(body), rest)

          case (core.Definition.Def(id, core.New(impl)), rest) =>
            New(Variable(transform(id), transform(impl.interface)), transform(impl, None), rest)

          case (d @ core.Definition.Def(_, _: core.BlockVar | _: core.Member | _: core.Unbox), rest) =>
            ErrorReporter.abort(s"block definition: $d")
        }

      case core.Return(expr) =>
        transform(expr).run { value => Return(List(value)) }

      case core.Val(id, annot, binding, rest) =>
        PushFrame(
          Clause(List(Variable(transform(id), transform(binding.tpe))), transform(rest)),
            transform(binding)
        )

      // hardcoded translation for get and put.
      // TODO remove this when interfaces are correctly translated
      case core.App(core.Member(core.BlockVar(x, core.Type.TState(stateType), _), TState.get, _), targs, Nil, Nil) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        val tpe = transform(stateType)
        val variable = Variable(freshName(x.name.name + "_value"), tpe)
        val reference = Variable(transform(x), Type.Reference(tpe))
        Load(variable, reference, Return(List(variable)))

      case core.App(core.Member(core.BlockVar(x, core.Type.TState(stateType), _), TState.put, _), targs, List(arg), Nil) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        val tpe = transform(stateType)
        val variable = Variable(freshName("ignored"), Positive());
        val reference = Variable(transform(x), Type.Reference(tpe))
        transform(arg).run { value =>
          Store(reference, value,
            Construct(variable, builtins.Unit, List(),
              Return(List(variable))))
        }

      case core.App(callee, targs, vargs, bargs) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        transformCallee(callee).run { callee =>
          transform(vargs, bargs).run { (values, blocks) =>
            callee match {
              // Here we actually need a substitution to prepare the environment for the jump
              case Callee.Known(label) =>
                Substitute(label.environment.zip(values ++ blocks), Jump(label))

              case Callee.UnknownFunction(variable, tpe) =>
                Invoke(variable, builtins.Apply, values ++ blocks)

              case Callee.Method(receiver, tpe, tag) =>
                Invoke(receiver, tag, values ++ blocks)

              case Callee.Continuation(variable) =>
                PushStack(variable, Return(values))

              case Callee.UnknownObject(variable, tpe) =>
                E.panic("Cannot call an object.")
            }
          }
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

        val variable = Variable(freshName("returned"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val delimiter = Variable(freshName("delimiter"), Type.Stack())
        val prompt = Variable(freshName("prompt"), Type.Prompt())

        FreshPrompt(prompt,
          NewStack(delimiter, prompt, returnClause,
            PushStack(delimiter,
              (bparams zip handlers).foldRight(transform(body)){
                case ((id, handler), body) =>
                  New(transform(id), transform(handler, Some(prompt)), body)
              })))

      case core.Region(core.BlockLit(tparams, cparams, vparams, List(region), body)) =>
        val variable = Variable(freshName("returned"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val delimiter = Variable(freshName("delimiter"), Type.Stack())
        val prompt = transform(region)

        FreshPrompt(prompt,
          NewStack(delimiter, prompt, returnClause,
            PushStack(delimiter, transform(body))))

      case core.Alloc(id, init, region, body) =>
        transform(init).run { value =>
          val tpe = value.tpe;
          val name = transform(id)
          val variable = Variable(name, tpe)
          val reference = Variable(transform(id), Type.Reference(tpe))
          val prompt = Variable(transform(region), Type.Prompt())

          region match {
            case symbols.builtins.globalRegion =>
              // TODO currently we use prompt 1 as a quick fix...
              //    However, this will not work when reinstalling a fresh stack
              //    We need to truly special case global memory!
              LiteralInt(prompt, 1L,
                Allocate(reference, value, prompt,
                  transform(body)))
            case _ =>
              Allocate(reference, value, prompt,
                  transform(body))
          }
        }

      case core.Var(id, init, capture, body) =>
        val stateType = transform(init.tpe)
        val reference = Variable(transform(id), Type.Reference(stateType))
        val prompt = Variable(freshName("prompt"), Type.Prompt())

        transform(init).run { value =>
          Var(reference, value, transform(body.tpe),
            transform(body))
        }

      case core.Get(id, capt, tpe) =>
        val stateType = transform(tpe)
        val reference = Variable(transform(id), Type.Reference(stateType))
        val variable = Variable(freshName("get"), stateType)

        LoadVar(variable, reference,
            Return(List(variable)))

      case core.Put(id, capt, arg) =>
        val stateType = transform(arg.tpe)
        val reference = Variable(transform(id), Type.Reference(stateType))
        val variable = Variable(freshName("put"), Positive())

        transform(arg).run { value =>
          StoreVar(reference, value,
            Construct(variable, builtins.Unit, List(),
              Return(List(variable))))
        }

      case core.Hole() => machine.Statement.Hole

      case _ =>
        ErrorReporter.abort(s"Unsupported statement: $stmt")
    }

  // Merely sequences the transformation of the arguments monadically
  def transform(vargs: List[core.Pure], bargs: List[core.Block])(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[(List[Variable], List[Variable])] =
    for {
      values <- traverse(vargs)(transform)
      blocks <- traverse(bargs)(transformBlockArg)
    } yield (values, blocks)

  enum Callee {
    case Known(label: machine.Label)
    case UnknownFunction(variable: machine.Variable, tpe: core.BlockType.Function)
    case UnknownObject(variable: machine.Variable, tpe: core.BlockType.Interface)
    case Method(receiver: machine.Variable, tpe: core.BlockType.Interface, tag: Int)
    case Continuation(variable: machine.Variable)
  }

  def transformCallee(block: core.Block)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Callee] = block match {
    case core.BlockVar(id, tpe, capt) =>
        BPC.info(id) match {
          // Unknown Jump to function
          case BlockInfo.Parameter(tpe: core.BlockType.Function) =>
            pure(Callee.UnknownFunction(Variable(transform(id), transform(tpe)), tpe))

          // Unknown object as a receiver
          case BlockInfo.Parameter(tpe: core.BlockType.Interface) =>
            pure(Callee.UnknownObject(Variable(transform(id), transform(tpe)), tpe))

          // Continuation Call
          case BlockInfo.Resumption =>
            pure(Callee.Continuation(Variable(transform(id), Type.Stack())))

          // Known Jump
          case BlockInfo.Definition(freeParams, blockParams) =>
            pure(Callee.Known(machine.Label(transform(id), blockParams ++ freeParams)))
        }
    case core.Member(block, op, annotatedTpe) => transformCallee(block).flatMap {
      case Callee.UnknownObject(id, tpe) =>
        val opTag = DeclarationContext.getPropertyTag(op)
        pure(Callee.Method(id, tpe, opTag))
      case _ =>
        E.panic("Receiver of a method call needs to be an object")
    }
    case core.BlockLit(tparams, cparams, vparams, bparams, body) =>
      E.panic("Optimizer / normalizer should have removed the beta reduction ({() => ...}())!")
    case core.Unbox(pure) =>
      transform(pure).map { f =>
        pure.tpe match {
          case core.ValueType.Boxed(tpe: core.BlockType.Function, capt) => Callee.UnknownFunction(f, tpe)
          case core.ValueType.Boxed(tpe: core.BlockType.Interface, capt) => Callee.UnknownObject(f, tpe)
          case _ => E.panic("Can only unbox boxed types")
        }
      }
    case core.New(impl) =>
      E.panic("Optimizer / normalizer should have removed the beta reduction ({() => ...}())!")
  }

  def transformBlockArg(block: core.Block)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] = block match {
    case core.BlockVar(id, tpe, capt) if isDefinition(id) =>
      // Passing a top-level function directly, so we need to eta-expand turning it into a closure
      // TODO cache the closure somehow to prevent it from being created on every call
      val parameters = BPC.params(id)
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

    case core.Member(b, field, annotatedTpe) =>
      E.panic("Cannot pass member selection as argument")

    case core.Unbox(pure) =>
      transform(pure)
  }

  def transform(expr: core.Expr)(using C: BlocksParamsContext, D: DeclarationContext, E: ErrorReporter): Binding[Variable] = expr match {

    case core.ValueVar(id, tpe) if C.globals contains id =>
      val variable = Variable(freshName("run"), transform(tpe))
      Binding { k =>
        // TODO this might introduce too many pushes.
        PushFrame(Clause(List(variable), k(variable)),
          Substitute(Nil, Jump(C.globals(id))))
      }

    case core.ValueVar(id, tpe) =>
      pure(Variable(transform(id), transform(tpe)))

    case core.Literal((), _) =>
      val variable = Variable(freshName("unitLiteral"), Positive());
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
      val variables = fields.map { f => Variable(freshName("select"), transform(f.tpe)) }
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
      transformBlockArg(block)

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
      // Effectful operation, capture the continuation
      case (Some(prompt), core.Operation(name, tparams, cparams, vparams, bparams, Some(kparam), body)) =>
        noteResumption(kparam.id)
        // TODO deal with bidirectional effects
        Clause(vparams.map(transform),
          PopStacks(Variable(transform(kparam).name, Type.Stack()), prompt,
            transform(body)))

      // No continuation, implementation of an object
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
    case core.Type.TRegion => Type.Prompt()
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) => Negative()
    case core.BlockType.Interface(symbol, targs) => Negative()
  }

  def transform(id: Id): String =
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

  def findToplevelBlocksParams(definitions: List[core.Definition])(using C: BlocksParamsContext, E: ErrorReporter): Unit =
    definitions.foreach {
      case Definition.Def(id, core.BlockLit(tparams, cparams, vparams, bparams, body)) =>
        noteDefinition(id, vparams.map(transform) ++ bparams.map(transform), Nil)
        noteParameters(bparams)
      case Definition.Let(id, tpe, binding) =>
        noteDefinition(id, Nil, Nil)
        C.globals = C.globals + (id -> Label(transform(id), Nil))
      case other => ()
    }

  /**
   * Extra info in context
   */

  class BlocksParamsContext() {
    var info: Map[Symbol, BlockInfo] = Map.empty

    var globals: Map[Id, Label] = Map.empty

    def definition(id: Id): BlockInfo.Definition = info(id) match {
      case d : BlockInfo.Definition => d
      case BlockInfo.Parameter(tpe) => sys error s"Expected a function definition, but got a block parameter: ${id}"
      case BlockInfo.Resumption => sys error s"Expected a function definition, but got a continuation: ${id}"
    }
    def params(id: Id): Environment = definition(id).params
    def free(id: Id): Environment = definition(id).free
  }
  enum BlockInfo {
    case Definition(free: Environment, params: Environment)
    case Parameter(tpe: core.BlockType)
    case Resumption
  }

  def DeclarationContext(using DC: DeclarationContext): DeclarationContext = DC

  def noteDefinition(id: Id, params: Environment, free: Environment)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: BlockInfo)")
    BC.info += (id -> BlockInfo.Definition(free, params))

  def noteResumption(id: Id)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: Resumption)")
    BC.info += (id -> BlockInfo.Resumption)


  def noteParameter(id: Id, tpe: core.BlockType)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: Parameter)")
    BC.info += (id -> BlockInfo.Parameter(tpe))

  def noteParameters(ps: List[core.BlockParam])(using BC: BlocksParamsContext): Unit =
    ps.foreach {
      case core.BlockParam(id, tpe, capt) => noteParameter(id, tpe)
    }

  def getBlocksParams(id: Id)(using BC: BlocksParamsContext): Environment = BC.definition(id) match {
    case BlockInfo.Definition(freeParams, blockParams) => blockParams ++ freeParams
  }

  def isResumption(id: Id)(using BC: BlocksParamsContext): Boolean =
    BC.info(id) match {
      case BlockInfo.Resumption => true
      case _ => false
    }

  def isDefinition(id: Id)(using BC: BlocksParamsContext): Boolean =
    BC.info(id) match {
      case d: BlockInfo.Definition => true
      case _ => false
    }

  case class Binding[A](run: (A => Statement) => Statement) {
    def flatMap[B](rest: A => Binding[B]): Binding[B] = {
      Binding(k => run(a => rest(a).run(k)))
    }
    def map[B](f: A => B): Binding[B] = flatMap { a => pure(f(a)) }
  }

  def traverse[S, T](l: List[S])(f: S => Binding[T]): Binding[List[T]] =
    l match {
      case Nil => pure(Nil)
      case head :: tail => for { x <- f(head); xs <- traverse(tail)(f) } yield x :: xs
    }

  def pure[A](a: A): Binding[A] = Binding(k => k(a))
}
