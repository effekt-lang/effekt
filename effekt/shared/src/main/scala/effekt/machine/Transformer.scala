package effekt
package machine

import effekt.context.Context
import effekt.core.{ Block, DeclarationContext, Toplevel, Id, given }
import effekt.symbols.{ Symbol, TermSymbol }
import effekt.symbols.builtins.TState
import effekt.util.messages.ErrorReporter
import effekt.symbols.ErrorMessageInterpolator
import scala.annotation.tailrec


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
    val mainEntry = Label(mainName, Nil)

    findToplevelBlocksParams(definitions)

    val toplevelDefinitions = definitions.map {
      case core.Toplevel.Def(id, core.BlockLit(tparams, cparams, vparams, bparams, body)) =>
        Definition(Label(transform(id), vparams.map(transform) ++ bparams.map(transform)), transform(body))
      case core.Toplevel.Val(id, tpe, binding) =>
        Definition(BC.globals(id), transform(binding))
      case core.Toplevel.Def(id, block @ core.New(impl)) =>
        val variable = Variable(freshName("returned"), transform(block.tpe))
        Definition(BC.globals(id), New(variable, transform(impl), Return(List(variable))))
      case d =>
        ErrorReporter.abort(s"Other toplevel definitions not yet supported: ${d}")
    }

    val localDefinitions = BC.definitions

    Program(declarations, toplevelDefinitions ++ localDefinitions, mainEntry)
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

      case core.Def(id, block @ core.BlockLit(tparams, cparams, vparams, bparams, body), rest) =>
        // (1) Collect all the information about free variables of local definitions
        noteParameters(bparams)

        // Does not work for mutually recursive local definitions (which are not supported anyway, at the moment)
        val freeVariables = core.Variables.free(block).toSet
          .filterNot(x => BPC.globals.contains(x.id)) // globals are NOT free

        val freeParams = freeVariables.flatMap {
          case core.Variable.Value(id, tpe) =>
            Set(Variable(transform(id), transform(tpe)))

          // Mutable variables are blocks and can be free, but do not have info.
          case core.Variable.Block(id, core.Type.TState(stTpe), capt) =>
            Set(Variable(transform(id), Type.Reference(transform(stTpe))))

          // Regions are blocks and can be free, but do not have info.
          case core.Variable.Block(id, core.Type.TRegion, capt) =>
            Set(Variable(transform(id), Type.Prompt()))

          case core.Variable.Block(pid, tpe, capt) if pid != id => BPC.info.get(pid) match {
              // For each known free block we have to add its free variables to this one (flat closure)
              case Some(BlockInfo.Definition(freeParams, blockParams)) =>
                freeParams.toSet

              // Unknown free blocks stay free variables
              case Some(BlockInfo.Parameter(tpe)) =>
                Set(Variable(transform(pid), transform(tpe)))

              // Everything else is considered bound or global
              case None =>
                ErrorReporter.panic(s"Could not find info for free variable $pid")
            }
          case _ => Set.empty
        }

        noteDefinition(id, vparams.map(transform) ++ bparams.map(transform), freeParams.toList)

        // (2) Actually translate the definitions
        emitDefinition(transformLabel(id), transform(body))
        transform(rest)

      case core.Def(id, block @ core.New(impl), rest) =>
        // this is just a hack...
        noteParameter(id, block.tpe)
        New(Variable(transform(id), transform(impl.interface)), transform(impl), transform(rest))

      case core.Def(id, core.BlockVar(other, tpe, capt), rest) =>
        getBlockInfo(other) match {
          case BlockInfo.Definition(free, params) =>
            noteDefinition(id, free, params)
            emitDefinition(transformLabel(id), Jump(transformLabel(other)))
            transform(rest)
          case BlockInfo.Parameter(_) =>
            noteParameter(id, tpe)
            Substitute(List(Variable(transform(id), transform(tpe)) -> Variable(transform(other), transform(tpe))), transform(rest))
        }

      case core.Def(id, block @ core.Unbox(pure), rest) =>
        noteParameter(id, block.tpe)
        transform(pure).run { boxed =>
          ForeignCall(Variable(transform(id), Type.Negative()), "unbox", List(boxed), transform(rest))
        }

      case core.Let(id, tpe, binding, rest) =>
        transform(binding).run { value =>
          // TODO consider passing the environment to [[transform]] instead of explicit substitutions here.
          // TODO it is important that we use the inferred [[binding.tpe]] and not the annotated type [[tpe]], but why?
          Substitute(List(Variable(transform(id), transform(binding.tpe)) -> value), transform(rest))
        }

      case core.Return(expr) =>
        transform(expr).run { value => Return(List(value)) }

      case core.Val(id, annot, binding, rest) =>
        PushFrame(
          Clause(List(Variable(transform(id), transform(binding.tpe))), transform(rest)),
            transform(binding)
        )

      case core.App(callee, targs, vargs, bargs) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.panic(s"Types ${targs} are used as type parameters but would require boxing.") }
        transform(vargs, bargs).run { (values, blocks) =>
          callee match {
            case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
              BPC.info.getOrElse(id, sys.error(pp"In ${stmt}. Cannot find block info for ${id}: ${annotatedTpe}.\n${BPC.info}")) match {
                // Unknown Jump to function
                case BlockInfo.Parameter(tpe: core.BlockType.Function) =>
                  Invoke(Variable(transform(id), transform(tpe)), builtins.Apply, values ++ blocks)

                // Known Jump
                case BlockInfo.Definition(freeParams, blockParams) =>
                  val label = machine.Label(transform(id), blockParams ++ freeParams)
                  Substitute(label.environment.zip(values ++ blocks), Jump(label))

                case _ => ErrorReporter.panic("Applying an object")
              }

            case Block.Unbox(pure) =>
              transform(pure).run { boxedCallee =>
                val callee = Variable(freshName(boxedCallee.name), Type.Negative())

                ForeignCall(callee, "unbox", List(boxedCallee),
                  Invoke(callee, builtins.Apply, values ++ blocks))
              }

            case Block.New(impl) =>
              ErrorReporter.panic("Applying an object")

            case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
              ErrorReporter.panic(pp"Call to block literal should have been reduced: ${stmt}")
          }
        }

      case core.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
        if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

        val opTag = DeclarationContext.getPropertyTag(method)
        transform(vargs, bargs).run { (values, blocks) =>
          callee match {
            case Block.BlockVar(id, tpe, capt) if BPC.globals contains id =>
              val variable = Variable(freshName("receiver"), transform(tpe))
              PushFrame(Clause(List(variable), Invoke(variable, opTag, values ++ blocks)), Jump(BPC.globals(id)))

            case Block.BlockVar(id, tpe, capt) =>
              Invoke(Variable(transform(id), transform(tpe)), opTag, values ++ blocks)

            case Block.Unbox(pure) =>
              transform(pure).run { boxedCallee =>
                val callee = Variable(freshName(boxedCallee.name), Type.Negative())

                ForeignCall(callee, "unbox", List(boxedCallee),
                  Invoke(callee, opTag, values ++ blocks))
              }

            case Block.New(impl) =>
              ErrorReporter.panic("Method call to known object should have been reduced")

            case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
              ErrorReporter.panic("Invoking a method on a function")
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

        val answerType = stmt.tpe
        val variable = Variable(freshName("returned"), transform(answerType))
        val returnClause = Clause(List(variable), Return(List(variable)))

        Reset(Variable(transform(prompt.id), Type.Prompt()), returnClause, transform(body))

      case core.Shift(prompt, core.BlockLit(Nil, cparams, Nil, List(k), body)) =>

        noteParameter(k.id, core.Type.TResume(core.Type.TUnit, core.Type.TUnit))

        Shift(Variable(transform(k.id), Type.Stack()), Variable(transform(prompt.id), Type.Prompt()),
          transform(body))

      case core.Resume(k, body) =>
        Resume(Variable(transform(k.id), Type.Stack()), transform(body))

      case core.Region(core.BlockLit(tparams, cparams, vparams, List(region), body)) =>
        noteParameters(List(region))

        val variable = Variable(freshName("returned"), transform(body.tpe))
        val returnClause = Clause(List(variable), Return(List(variable)))
        val prompt = transform(region)

        Reset(prompt, returnClause, transform(body))

      case core.Alloc(id, init, region, body) =>
        transform(init).run { value =>
          val reference = Variable(transform(id), Type.Reference(value.tpe))
          val prompt = Variable(transform(region), Type.Prompt())
          val temporary = Variable(freshName("temporaryStack"), Type.Stack())

          Shift(temporary, prompt,
            Var(reference, value, Type.Positive(),
              Resume(temporary, transform(body))))
        }

      case core.Var(ref, init, capture, body) =>
        val stateType = transform(init.tpe)
        val reference = Variable(transform(ref), Type.Reference(stateType))
        val prompt = Variable(freshName("prompt"), Type.Prompt())

        transform(init).run { value =>
          Var(reference, value, transform(body.tpe),
            transform(body))
        }

      case core.Get(id, tpe, ref, capt, body) =>
        val stateType = transform(tpe)
        val reference = Variable(transform(ref), Type.Reference(stateType))
        val variable = Variable(transform(id), stateType)

        LoadVar(variable, reference, transform(body))

      case core.Put(ref, capt, arg, body) =>
        val stateType = transform(arg.tpe)
        val reference = Variable(transform(ref), Type.Reference(stateType))
        val variable = Variable(freshName("put"), Positive())

        transform(arg).run { value =>
          StoreVar(reference, value, transform(body))
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

  def transformBlockArg(block: core.Block)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] = block match {
    case core.BlockVar(id, tpe, _) if BPC.globals contains id =>
      val variable = Variable(transform(id), transform(tpe))
      shift { k =>
        PushFrame(Clause(List(variable), k(variable)), Jump(BPC.globals(id)))
      }
    case core.BlockVar(id, tpe, capt) => getBlockInfo(id) match {
      case BlockInfo.Definition(_, parameters) =>
        // Passing a top-level function directly, so we need to eta-expand turning it into a closure
        // TODO cache the closure somehow to prevent it from being created on every call
        val variable = Variable(freshName(id.name.name ++ "$closure"), Negative())
        shift { k =>
          New(variable, List(Clause(parameters,
            // conceptually: Substitute(parameters zip parameters, Jump(...)) but the Substitute is a no-op here
            Jump(transformLabel(id))
          )), k(variable))
        }
      case BlockInfo.Parameter(tpe) =>
        pure(Variable(transform(id), transform(tpe)))
    }

    case core.BlockLit(tparams, cparams, vparams, bparams, body) =>
      noteParameters(bparams)
      val parameters = vparams.map(transform) ++ bparams.map(transform);
      val variable = Variable(freshName("blockLit"), Negative())
      shift { k =>
        New(variable, List(Clause(parameters, transform(body))), k(variable))
      }

    case core.New(impl) =>
      val variable = Variable(freshName("new"), Negative())
      shift { k =>
        New(variable, transform(impl), k(variable))
      }

    case core.Unbox(pure) =>
      transform(pure)
  }

  def transform(expr: core.Expr)(using BC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] = expr match {

    case core.ValueVar(id, tpe) if BC.globals contains id =>
      val variable = Variable(freshName("run"), transform(tpe))
      shift { k =>
        // TODO this might introduce too many pushes.
        PushFrame(Clause(List(variable), k(variable)),
          Substitute(Nil, Jump(BC.globals(id))))
      }

    case core.ValueVar(id, tpe) =>
      pure(Variable(transform(id), transform(tpe)))

    case core.Literal((), _) =>
      val variable = Variable(freshName("unitLiteral"), Positive());
      shift { k =>
        Construct(variable, builtins.Unit, List(), k(variable))
      }

    case core.Literal(value: Long, _) =>
      val variable = Variable(freshName("longLiteral"), Type.Int());
      shift { k =>
        LiteralInt(variable, value, k(variable))
      }

    // for characters
    case core.Literal(value: Int, _) =>
      val variable = Variable(freshName("intLiteral"), Type.Int());
      shift { k =>
        LiteralInt(variable, value, k(variable))
      }

    case core.Literal(value: Boolean, _) =>
      val variable = Variable(freshName("booleanLiteral"), Positive())
      shift { k =>
        Construct(variable, if (value) builtins.True else builtins.False, List(), k(variable))
      }

    case core.Literal(v: Double, _) =>
      val literal_binding = Variable(freshName("doubleLiteral"), Type.Double());
      shift { k =>
        LiteralDouble(literal_binding, v, k(literal_binding))
      }

    case core.Literal(javastring: String, _) =>
      val literal_binding = Variable(freshName("utf8StringLiteral"), builtins.StringType);
      shift { k =>
        LiteralUTF8String(literal_binding, javastring.getBytes("utf-8"), k(literal_binding))
      }

    case core.PureApp(core.BlockVar(blockName: symbols.ExternFunction, tpe: core.BlockType.Function, capt), targs, vargs) =>
      if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

      val variable = Variable(freshName("pureApp"), transform(tpe.result))
      transform(vargs, Nil).flatMap { (values, blocks) =>
        shift { k =>
          ForeignCall(variable, transform(blockName), values ++ blocks, k(variable))
        }
      }

    case core.DirectApp(core.BlockVar(blockName: symbols.ExternFunction, tpe: core.BlockType.Function, capt), targs, vargs, bargs) =>
      if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

      val variable = Variable(freshName("pureApp"), transform(tpe.result))
      transform(vargs, bargs).flatMap { (values, blocks) =>
        shift { k =>
          ForeignCall(variable, transform(blockName), values ++ blocks, k(variable))
        }
      }

    case core.Make(data, constructor, targs, vargs) =>
      if (targs.exists(requiresBoxing)) { ErrorReporter.abort(s"Types ${targs} are used as type parameters but would require boxing.") }

      val variable = Variable(freshName("make"), transform(data));
      val tag = DeclarationContext.getConstructorTag(constructor)

      transform(vargs, Nil).flatMap { (values, blocks) =>
        shift { k =>
          Construct(variable, tag, values ++ blocks, k(variable))
        }
      }

    case core.Box(block, annot) =>
      transformBlockArg(block).flatMap { unboxed =>
        shift { k =>
          val boxed = Variable(freshName(unboxed.name), Type.Positive())
          ForeignCall(boxed, "box", List(unboxed), k(boxed))
        }
      }

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
    case core.ValueType.Boxed(tpe, capt) => Positive()
    case core.Type.TUnit => builtins.UnitType
    case core.Type.TInt => Type.Int()
    case core.Type.TChar => Type.Int()
    case core.Type.TByte => Type.Byte()
    case core.Type.TBoolean => builtins.BooleanType
    case core.Type.TDouble => Type.Double()
    case core.Type.TString => Positive()
    case core.ValueType.Data(symbol, targs) => Positive()
  }

  def transform(tpe: core.BlockType)(using ErrorReporter): Type = tpe match {
    case core.Type.TRegion => Type.Prompt()
    case core.Type.TResume(result, answer) => Type.Stack()
    case core.Type.TPrompt(answer) => Type.Prompt()
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) => Negative()
    case core.BlockType.Interface(symbol, targs) => Negative()
  }

  def transformLabel(id: Id)(using BPC: BlocksParamsContext): Label = getBlockInfo(id) match {
    case BlockInfo.Definition(freeParams, boundParams) => Label(transform(id), boundParams ++ freeParams)
    case BlockInfo.Parameter(_) => sys error s"Expected a function definition, but got a block parameter: ${id}"
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

  def findToplevelBlocksParams(definitions: List[core.Toplevel])(using BlocksParamsContext, ErrorReporter): Unit =
    definitions.foreach {
      case Toplevel.Def(id, core.BlockLit(tparams, cparams, vparams, bparams, body)) =>
        noteDefinition(id, vparams.map(transform) ++ bparams.map(transform), Nil)
        noteParameters(bparams)
      case Toplevel.Val(id, tpe, binding) =>
        noteDefinition(id, Nil, Nil)
        noteGlobal(id)
      case Toplevel.Def(id, core.New(impl)) =>
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
    var definitions: List[Definition] = List.empty
  }

  enum BlockInfo {
    case Definition(free: Environment, params: Environment)
    case Parameter(tpe: core.BlockType)
  }

  def DeclarationContext(using DC: DeclarationContext): DeclarationContext = DC

  def noteDefinition(id: Id, params: Environment, free: Environment)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: Definition)")
    BC.info += (id -> BlockInfo.Definition(free, params))

  def noteParameter(id: Id, tpe: core.BlockType)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: Parameter)")
    BC.info += (id -> BlockInfo.Parameter(tpe))

  def noteParameters(ps: List[core.BlockParam])(using BC: BlocksParamsContext): Unit =
    ps.foreach {
      case core.BlockParam(id, tpe, capt) => noteParameter(id, tpe)
    }

  def noteGlobal(id: Id)(using BPC: BlocksParamsContext): Unit =
    BPC.globals += (id -> Label(transform(id), Nil))

  def emitDefinition(label: Label, statement: Statement)(using BPC: BlocksParamsContext): Unit =
    BPC.definitions = Definition(label, statement) :: BPC.definitions

  def getBlockInfo(id: Id)(using BPC: BlocksParamsContext): BlockInfo =
    BPC.info.getOrElse(id, sys error s"No block info for ${util.show(id)}")

  def shift[A](body: (A => Statement) => Statement): Binding[A] =
    Binding { k => Trampoline.Done(body { x => trampoline(k(x)) }) }

  case class Binding[A](body: (A => Trampoline[Statement]) => Trampoline[Statement]) {
    def flatMap[B](rest: A => Binding[B]): Binding[B] = {
      Binding(k => Trampoline.More { () => body(a => Trampoline.More { () => rest(a).body(k) }) })
    }
    def run(k: A => Statement): Statement = trampoline(body { x => Trampoline.Done(k(x)) })
    def map[B](f: A => B): Binding[B] = flatMap { a => pure(f(a)) }
  }

  enum Trampoline[A] {
    case Done(value: A)
    case More(thunk: () => Trampoline[A])
  }

  @tailrec
  def trampoline[A](body: Trampoline[A]): A = body match {
    case Trampoline.Done(value) => value
    case Trampoline.More(thunk) => trampoline(thunk())
  }

  def traverse[S, T](l: List[S])(f: S => Binding[T]): Binding[List[T]] =
    l match {
      case Nil => pure(Nil)
      case head :: tail => for { x <- f(head); xs <- traverse(tail)(f) } yield x :: xs
    }

  def pure[A](a: A): Binding[A] = Binding(k => k(a))
}
