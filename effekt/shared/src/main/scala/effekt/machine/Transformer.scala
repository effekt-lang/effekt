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
      if bparams.nonEmpty then ErrorReporter.abort("Foreign functions currently cannot take block arguments.")

      val transformedParams = vparams.map(transform)
      noteDefinition(name, transformedParams, Nil)
      Extern(transform(name), transformedParams, transform(ret), capture.contains(symbols.builtins.AsyncCapability.capture), transform(body))

    case core.Extern.Include(ff, contents) =>
      Include(ff, contents)
  }

  def transform(body: core.ExternBody)(using ErrorReporter): machine.ExternBody = body match {
    case core.ExternBody.StringExternBody(ff, Template(strings, args)) =>
      ExternBody.StringExternBody(ff, Template(strings, args map {
        case core.ValueVar(id, tpe) => Variable(transform(id), transform(tpe))
        case _ => ErrorReporter.abort("In the LLVM backend, only variables are allowed in templates")
      }))
    case core.ExternBody.Unsupported(err) =>
      ExternBody.Unsupported(err)
  }

  def transform(stmt: core.Stmt)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Statement =
    stmt match {
      case core.Scope(definitions, rest) =>
        // (1) Collect all the information about free variables of local definitions
        definitions.foreach {
          case Definition.Def(id,  block @ core.BlockLit(tparams, cparams, vparams, bparams, body)) =>

            noteParameters(bparams)

            // TODO use existing lambda lifting code

            // Does not work for mutually recursive local definitions (which are not supported anyway, at the moment)
            val freeVariables = core.Variables.free(block).toSet
              .filterNot(x => BPC.globals.contains(x.id)) // globals are NOT free

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

                  // Everything else is considered bound or global
                  case None =>
                    // TODO should not happen, abort with error
                    Set.empty
                }
              case _ => Set.empty
            }

            noteDefinition(id, vparams.map(transform) ++ bparams.map(transform), freeParams.toList)

          case Definition.Def(id, b @ core.New(impl)) =>
            // this is just a hack...
            noteParameter(id, b.tpe)
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
            New(Variable(transform(id), transform(impl.interface)), transform(impl), rest)

          case (d @ core.Definition.Def(_, _: core.BlockVar | _: core.Unbox), rest) =>
            ErrorReporter.abort(s"block definition: $d")
        }

      case core.Return(expr) =>
        transform(expr).run { value => Return(List(value)) }

      case core.Val(id, annot, binding, rest) =>
        PushFrame(
          Clause(List(Variable(transform(id), transform(binding.tpe))), transform(rest)),
            transform(binding)
        )

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

              case Callee.UnknownObject(variable, tpe) =>
                E.panic("Cannot call an object.")
            }
          }
        }

      case core.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        transformCallee(callee).run { callee =>
          transform(vargs, bargs).run { (values, blocks) =>
            callee match {
              case Callee.UnknownObject(variable, tpe) =>
                val opTag = DeclarationContext.getPropertyTag(method)
                Invoke(variable, opTag, values ++ blocks)

              case _ =>
                E.panic("Receiver of a method call needs to be an object")
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

      case core.Reset(core.BlockLit(Nil, cparams, Nil, List(prompt), body)) =>
        noteParameters(List(prompt))

        val variable = Variable(freshName("returned"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))

        Reset(Variable(transform(prompt.id), Type.Prompt()), returnClause, transform(body))

      case core.Shift(prompt, core.BlockLit(Nil, cparams, Nil, List(k), body)) =>

        noteParameter(k.id, core.Type.TResume(core.Type.TUnit, core.Type.TUnit))

        Shift(Variable(transform(k.id), Type.Stack()), Variable(transform(prompt.id), Type.Prompt()),
          transform(body))

      case core.Resume(k, body) =>
        Resume(Variable(transform(k.id), Type.Stack()), transform(body))

      case core.Region(core.BlockLit(tparams, cparams, vparams, List(region), body)) =>
        val variable = Variable(freshName("returned"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val prompt = transform(region)

        Reset(prompt, returnClause, transform(body))

      case core.Alloc(id, init, region, body) =>
        transform(init).run { value =>
          val tpe = value.tpe;
          val name = transform(id)
          val variable = Variable(name, tpe)
          val reference = Variable(transform(id), Type.Reference(tpe))
          val prompt = Variable(transform(region), Type.Prompt())
          val temporary = Variable(freshName("temporaryStack"), Type.Stack())

          region match {
            case symbols.builtins.globalRegion =>
              val globalPrompt = Variable("global", Type.Prompt())
              Shift(temporary, globalPrompt,
                Var(reference, value, Type.Positive(),
                  Resume(temporary, transform(body))))
            case _ =>
              Shift(temporary, prompt,
                Var(reference, value, Type.Positive(),
                  Resume(temporary, transform(body))))
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
  }

  def transformCallee(block: core.Block)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Callee] = block match {
    case core.BlockVar(id, tpe, capt) =>
        BPC.info.getOrElse(id, sys.error(s"Cannot find block info for ${id}.\n${BPC.info}")) match {
          // Unknown Jump to function
          case BlockInfo.Parameter(tpe: core.BlockType.Function) =>
            pure(Callee.UnknownFunction(Variable(transform(id), transform(tpe)), tpe))

          // Unknown object as a receiver
          case BlockInfo.Parameter(tpe: core.BlockType.Interface) =>
            pure(Callee.UnknownObject(Variable(transform(id), transform(tpe)), tpe))

          // Known Jump
          case BlockInfo.Definition(freeParams, blockParams) =>
            pure(Callee.Known(machine.Label(transform(id), blockParams ++ freeParams)))
        }
    case core.Unbox(pure) =>
      transform(pure).map { f =>
        pure.tpe match {
          case core.ValueType.Boxed(tpe: core.BlockType.Function, capt) => Callee.UnknownFunction(f, tpe)
          case core.ValueType.Boxed(tpe: core.BlockType.Interface, capt) => Callee.UnknownObject(f, tpe)
          case _ => E.panic("Can only unbox boxed types")
        }
      }
    case core.BlockLit(tparams, cparams, vparams, bparams, body) =>
      E.panic("Optimizer / normalizer should have removed the beta reduction ({() => ...}())!")
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
        New(variable, transform(impl), k(variable))
      }

    case core.Unbox(pure) =>
      transform(pure)
  }

  def transform(expr: core.Expr)(using BC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] = expr match {

    case core.ValueVar(id, tpe) if BC.globals contains id =>
      val variable = Variable(freshName("run"), transform(tpe))
      Binding { k =>
        // TODO this might introduce too many pushes.
        PushFrame(Clause(List(variable), k(variable)),
          Substitute(Nil, Jump(BC.globals(id))))
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

  def transform(impl: core.Implementation)(using BlocksParamsContext, DeclarationContext, ErrorReporter): List[Clause] =
    impl.operations.sortBy {
      case core.Operation(operationName, _, _, _, _, _) =>
        DeclarationContext.getInterface(impl.interface.name).properties.indexWhere(_.id == operationName)
    }.map(op => transform(op))

  def transform(op: core.Operation)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Clause =
    op match {
      // No continuation, implementation of an object
      case core.Operation(name, tparams, cparams, vparams, bparams, body) =>
        noteParameters(bparams)
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
    case core.Type.TResume(result, answer) => Type.Stack()
    case core.Type.TPrompt(answer) => Type.Prompt()
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

  def findToplevelBlocksParams(definitions: List[core.Definition])(using BlocksParamsContext, ErrorReporter): Unit =
    definitions.foreach {
      case Definition.Def(id, core.BlockLit(tparams, cparams, vparams, bparams, body)) =>
        noteDefinition(id, vparams.map(transform) ++ bparams.map(transform), Nil)
        noteParameters(bparams)
      case Definition.Let(id, tpe, binding) =>
        noteDefinition(id, Nil, Nil)
        noteGlobal(id)
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
    }
    def params(id: Id): Environment = definition(id).params
    def free(id: Id): Environment = definition(id).free
  }
  enum BlockInfo {
    case Definition(free: Environment, params: Environment)
    case Parameter(tpe: core.BlockType)
  }

  def DeclarationContext(using DC: DeclarationContext): DeclarationContext = DC

  def noteDefinition(id: Id, params: Environment, free: Environment)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: BlockInfo)")
    BC.info += (id -> BlockInfo.Definition(free, params))

  def noteParameter(id: Id, tpe: core.BlockType)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: Parameter)")
    BC.info += (id -> BlockInfo.Parameter(tpe))

  def noteParameters(ps: List[core.BlockParam])(using BC: BlocksParamsContext): Unit =
    ps.foreach {
      case core.BlockParam(id, tpe, capt) => noteParameter(id, tpe)
    }

  def noteGlobal(id: Id)(using BC: BlocksParamsContext): Unit =
    BC.globals += (id -> Label(transform(id), Nil))

  def getBlocksParams(id: Id)(using BC: BlocksParamsContext): Environment = BC.definition(id) match {
    case BlockInfo.Definition(freeParams, blockParams) => blockParams ++ freeParams
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
