package effekt
package machine

import effekt.context.Context
import effekt.core.substitutions.{Substitution, substitute}
import effekt.core.{Block, DeclarationContext, Id, Toplevel, given}
import effekt.symbols.{Symbol, TermSymbol}
import effekt.symbols.builtins.TState
import effekt.util.messages.ErrorReporter
import effekt.util.Trampoline
import effekt.symbols.ErrorMessageInterpolator
import effekt.util.UByte
import effekt.util.{ DB, toDB }

import scala.annotation.tailrec
import effekt.core.ExternBody.StringExternBody
import effekt.core.ExternBody.Unsupported


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
        Definition(Label(transform(id), vparams.map(transform) ++ bparams.map(transform)), transform(body).run())
      case core.Toplevel.Val(id, binding) =>
        Definition(BC.globals(id), transform(binding).run())
      case core.Toplevel.Def(id, block @ core.New(impl)) =>
        val variable = Variable(freshName("returned"), transform(block.tpe))
        Definition(BC.globals(id), New(variable, transform(impl), Return(List(variable))))
      case d =>
        ErrorReporter.abort(s"Other toplevel definitions not yet supported: ${d}")
    }

    val localDefinitions = BC.definitions

    Program(declarations, toplevelDefinitions ++ localDefinitions, mainEntry)
  }

  def transform(extern: core.Extern)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Declaration = extern match {
    case core.Extern.Def(name, qualifiedSignature, tps, cparams, vparams, bparams, ret, capture, body) =>
      // TODO delete, and/or enforce at call site (ImpureApp)
      if bparams.nonEmpty then ErrorReporter.abort("Foreign functions currently cannot take block arguments.")

      val transformedParams = vparams.map {
        case core.ValueParam(id, tpe) => Variable(transform(id), transformExtern(tpe))
      }
      val transformedRet = transformExtern(ret)
      val isExternAsync = capture.contains(symbols.builtins.AsyncCapability.capture)
      noteDefinition(name, transformedParams, Nil, isExternAsync)
      Extern(transform(name), transformedParams, transformedRet, isExternAsync, transform(body))

    case core.Extern.Include(ff, contents) =>
      Include(ff, contents)

    case core.Extern.Data(id, tparams, body) =>
      val tBody = body match {
        case core.ExternBody.StringExternBody(featureFlag, Template(strings, args)) =>
          ExternBody.StringExternBody(featureFlag, Template(strings, args.map{ absurd => absurd }))
        case core.ExternBody.Unsupported(err) => ExternBody.Unsupported(err)
      }
      ExternType(transform(id), tparams.map(transform), tBody)

    case core.Extern.Interface(id, tparams, body) =>
      val tBody = body match {
        case core.ExternBody.StringExternBody(featureFlag, Template(strings, args)) =>
          ExternBody.StringExternBody(featureFlag, Template(strings, args.map{ absurd => absurd }))
        case core.ExternBody.Unsupported(err) => ExternBody.Unsupported(err)
      }
      ExternInterface(transform(id), tparams.map(transform), tBody)
  }

  val validCTypes = List("ptr", "%CObject", "i64", "double", "float", "void")

  def parseCType(tpe: String)(using ErrorReporter): Option[CType] = tpe match {
    case "ptr"      => Some(machine.CType.Ptr)
    case "%CObject" => Some(machine.CType.Obj)
    case "i64"      => Some(machine.CType.I64)
    case "double"   => Some(machine.CType.Double)
    case "float"    => Some(machine.CType.Float)
    case "void"     => Some(machine.CType.Void)
    case o          => None
  } 

  def parseExternCTpe(name: Id)(using DC: DeclarationContext)(using ErrorReporter): Option[machine.Type.CTpe] = 
    DC.findExternData(name).flatMap(value => value.body match {
      case StringExternBody(_, contents) => 
        parseCType(contents.strings.head).map(machine.Type.CTpe.apply)
      case Unsupported(err) => None
    })

  def getExternCTpe(name: Id)(using DC: DeclarationContext)(using ErrorReporter): machine.Type.CTpe =
    parseExternCTpe(name).get

  def isValidExternC(name: Id)(using DC: DeclarationContext)(using ErrorReporter): Boolean = 
    parseExternCTpe(name).isDefined

  def handleExternC(template: Template[core.Expr])(using DC: DeclarationContext)(using ErrorReporter): Template[Variable] = template match {
    case Template(strings, args) => 
      val cFunName = strings.head.split(" ").head.trim()

      Template(List(cFunName), args map {
        case core.ValueVar(id, core.ValueType.Data(name, targs)) =>
          parseExternCTpe(name) match {
            case Some(tpe) => Variable(transform(id), tpe)
            case None => ErrorReporter.abort(s"In the C backend, only types '${validCTypes}' are allowed in templates")
          }
        case _ => ErrorReporter.abort(s"In the C backend, only valid extern data types are allowed")
      })
  }

  def transform(body: core.ExternBody[core.Expr])(using DeclarationContext, ErrorReporter): machine.ExternBody[Variable] = body match {
    case core.ExternBody.StringExternBody(ff, template) if ff.matches("c") => 
      ExternBody.StringExternBody(ff, handleExternC(template))
    case core.ExternBody.StringExternBody(ff, Template(strings, args)) =>
      ExternBody.StringExternBody(ff, Template(strings, args map {
        case core.ValueVar(id, tpe) => Variable(transform(id), transformExtern(tpe))
        case _ => ErrorReporter.abort("In the LLVM backend, only variables are allowed in templates")
      }))
    case core.ExternBody.Unsupported(err) =>
      ExternBody.Unsupported(err)
  }

  def transform(stmt: core.Stmt)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Trampoline[Statement] =
    stmt match {

      case core.Def(id, block @ core.BlockLit(tparams, cparams, vparams, bparams, body), rest) =>
        // (1) Collect all the information about free variables of local definitions
        noteParameters(bparams)

        // Does not work for mutually recursive local definitions (which are not supported anyway, at the moment)
        val freeValueParams = block.free.values.iterator.collect {
          // globals are NOT free
          case (id, tpe) if !BPC.globals.contains(id) => Variable(transform(id), transform(tpe))
        }
        val freeBlockParams = block.free.blocks.iterator.flatMap {

          // Function itself
          case (pid, (tpe, capt)) if pid == id => Set.empty

          case (pid, (tpe, capt)) =>
            BPC.info.get(pid) match {
              // For each known free block we have to add its free variables to this one (flat closure)
              case Some(BlockInfo.Definition(freeParams, blockParams, _)) =>
                freeParams.toSet
              // Unknown free blocks stay free variables
              case Some(BlockInfo.Parameter(tpe)) =>
                Set(Variable(transform(pid), transform(tpe)))
              // Everything else is considered bound or global
              case None =>
                ErrorReporter.panic(s"Could not find info for free variable $pid")
            }
        }
        val freeParams = (freeValueParams ++ freeBlockParams).toSet

        noteDefinition(id, vparams.map(transform) ++ bparams.map(transform), freeParams.toList)

        // (2) Actually translate the definitions
        emitDefinition(transformLabel(id), transform(body).run())
        transform(rest)

      case core.Def(id, block @ core.New(impl), rest) =>
        // this is just a hack...
        noteParameter(id, block.tpe)
        transform(rest).map { rest =>
          New(Variable(transform(id), transform(impl.interface)), transform(impl), rest)
        }

      case core.Def(id, core.BlockVar(other, tpe, capt), rest) =>
        getBlockInfo(other) match {
          case BlockInfo.Definition(free, params, _) =>
            noteDefinition(id, free, params)
            val label = transformLabel(id)
            emitDefinition(label, Jump(transformLabel(other), label.environment))
            transform(rest)
          case BlockInfo.Parameter(_) =>
            noteParameter(id, tpe)
            transform(substitute(rest)(using Substitution(DB.empty, DB.empty, DB.empty, DB(id, core.BlockVar(other, tpe, capt)))))
        }

      case core.Def(id, block @ core.Unbox(pure: core.ValueVar), rest) =>
        noteParameter(id, block.tpe)
        transform(substitute(rest)(using Substitution(DB.empty, DB.empty, DB.empty, DB(id, core.Unbox(pure)))))

      case core.Def(id, block @ core.Unbox(expr), rest) =>
        ErrorReporter.panic(s"Unbox of a non-variable expression: ${expr}")

      case core.Let(id, core.ValueVar(otherId, otherTpe), rest) =>
        transform(substitute(rest)(using Substitution(DB.empty, DB.empty, DB(id, core.ValueVar(otherId, otherTpe)), DB.empty)))

      case core.Let(id, expr @ core.Box(block, _), rest) => block match {
        case core.BlockVar(other, tpe, capt) if !(BPC.globals contains other) && isParameter(other) =>
          transform(substitute(rest)(using Substitution(DB.empty, DB.empty, DB(id, expr), DB.empty)))
        case core.Unbox(pure) =>
          transform(substitute(rest)(using Substitution(DB.empty, DB.empty, DB(id, pure), DB.empty)))
        case _ =>
          transformBlockArg(Variable(transform(id), transform(expr.tpe)), block).run { _ =>
            transform(rest)
          }
      }

      case core.Let(id, expr, rest) =>
        transformNamed(Variable(transform(id), transform(expr.tpe)), expr).run { _ =>
          transform(rest)
        }

      case app @ core.ImpureApp(id, core.BlockVar(blockName, core.BlockType.Function(_, _, vparamTypes, _, resultType), capt), targs, vargs, bargs, rest) =>
        val variable = Variable(transform(id), transform(core.Type.bindingType(app)))
        transform(rest).flatMap { rest =>
          transform(vargs, bargs).run { (values, blocks) =>
            coerce(values, vparamTypes map transformExtern).run { coerced =>
              Trampoline.Done(foreignCall(variable, transform(blockName), coerced ++ blocks, transformExtern(resultType), rest))
            }
          }
        }

      case core.Return(expr) =>
        transform(expr).run { value => Trampoline.Done(Return(List(value))) }

      case core.Val(id, binding, rest) =>
        val tpe = binding.tpe
        transform(rest).flatMap { rest =>
          transform(binding).map { binding =>
            PushFrame(
              Clause(List(Variable(transform(id), transform(tpe))), rest), binding)
           }
        }

      case core.App(callee, targs, vargs, bargs) =>
        transform(vargs, bargs).run { (values, blocks) =>
          callee match {
            case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
              BPC.info.getOrElse(id, sys.error(pp"In ${stmt}. Cannot find block info for ${id}: ${annotatedTpe}.\n${BPC.info}")) match {
                // Unknown Jump to function
                case BlockInfo.Parameter(tpe: core.BlockType.Function) =>
                  Trampoline.Done(Invoke(Variable(transform(id), transform(tpe)), builtins.Apply, values ++ blocks))

                // Known Jump
                case BlockInfo.Definition(freeParams, blockParams, false) =>
                  Trampoline.Done(Jump(Label(transform(id), blockParams ++ freeParams), values ++ blocks ++ freeParams))

                // Extern Async
                case BlockInfo.Definition(freeParams, blockParams, true) =>
                  // TODO better way to deal with extern async functions
                  annotatedTpe match {
                    case core.BlockType.Function(_, _, vparamTypes, _, resultType) =>
                      val returnType = transformExtern(resultType)
                      val expectedType = transform(stmt.tpe)
                      coerce(values, vparamTypes map transformExtern).run { coerced =>
                        val jump = Jump(Label(transform(id), blockParams ++ freeParams), coerced ++ blocks ++ freeParams)
                        if returnType == expectedType then Trampoline.Done(jump)
                        else
                          val returned = Variable(freshName("returned"), returnType)
                          val expected = Variable(freshName("coerced"), expectedType)
                          Trampoline.Done(PushFrame(Clause(List(returned), Coerce(expected, returned, Return(List(expected)))), jump))
                      }
                    case _ =>
                      ErrorReporter.panic("Extern definition does not have function type")
                  }

                case _ => ErrorReporter.panic("Applying an object")
              }

            case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
              transform(substitute(Block.BlockLit(tparams, cparams, vparams, bparams, body), targs, vargs, bargs))

            case Block.Unbox(pure) =>
              transform(pure).run { callee =>
                Trampoline.Done(Invoke(callee, builtins.Apply, values ++ blocks))
              }

            case Block.New(impl) =>
              ErrorReporter.panic("Applying an object")
          }
        }

      case core.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
        val opTag = DeclarationContext.getPropertyTag(method)
        transform(vargs, bargs).run { (values, blocks) =>
          callee match {
            case Block.BlockVar(id, tpe, capt) if BPC.globals contains id =>
              val label = BPC.globals(id)
              val variable = Variable(freshName("receiver"), transform(tpe))
              Trampoline.Done(PushFrame(Clause(List(variable), Invoke(variable, opTag, values ++ blocks)), Jump(label, label.environment)))

            case Block.BlockVar(id, tpe, capt) =>
              Trampoline.Done(Invoke(Variable(transform(id), transform(tpe)), opTag, values ++ blocks))

            case Block.Unbox(pure) =>
              transform(pure).run { callee =>
                Trampoline.Done(Invoke(callee, opTag, values ++ blocks))
              }

            case Block.New(impl) =>
              ErrorReporter.panic("Method call to known object should have been reduced")

            case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
              ErrorReporter.panic("Invoking a method on a function")
          }
        }

      case core.If(cond, thenStmt, elseStmt) =>
        transform(cond).run { value =>
          transform(elseStmt).flatMap { elseStmt =>
            transform(thenStmt).map { thenStmt =>
              Switch(value, List(0 -> Clause(List(), elseStmt), 1 -> Clause(List(), thenStmt)), None)
            }
          }
        }

      case core.Match(scrutinee, tpe, Nil, Some(default)) =>
        transform(default)

      case core.Match(scrutinee, tpe, clauses, default) =>
        val transformedClauses = clauses.map { case (constr, core.BlockLit(tparams, cparams, vparams, bparams, body)) =>
          // TODO monadic sequencing to avoid stack overflow
          DeclarationContext.getConstructorTag(constr) -> Clause(vparams.map(transform), transform(body).run())
        }
        val transformedDefault = default.map { clause =>
          // TODO monadic sequencing to avoid stack overflow
          Clause(List(), transform(clause).run())
        }

        transform(scrutinee).run { value =>
          Trampoline.Done(Switch(value, transformedClauses, transformedDefault))
        }

      case core.Reset(core.BlockLit(Nil, cparams, Nil, List(prompt), body)) =>
        noteParameters(List(prompt))

        val answerType = stmt.tpe
        val variable = Variable(freshName("returned"), transform(answerType))
        val returnClause = Clause(List(variable), Return(List(variable)))

        transform(body).map { body =>
          Reset(Variable(transform(prompt.id), Type.Prompt()), returnClause, body)
        }

      case core.Shift(prompt, k, body) =>

        noteParameter(k.id, core.Type.TResume(core.Type.TUnit, core.Type.TUnit))

        transform(body).map { body =>
          Shift(Variable(transform(k.id), Type.Stack()), Variable(transform(prompt.id), Type.Prompt()), body)
        }

      case core.Resume(k, body) =>
        transform(body).map { body =>
            Resume(Variable(transform(k.id), Type.Stack()), body)
        }

      case core.Region(core.BlockLit(tparams, cparams, vparams, List(region), body)) =>
        val prompt = transform(region)
        noteParameters(List(region))

        // Variables allocated in the region live in frames below the reset, so we box the result of the region
        transform(body.tpe) match {
          case Positive() =>
            val returned = Variable(freshName("returned"), Positive())
            transform(body).map { body =>
              Reset(prompt, Clause(List(returned), Return(List(returned))), body)
            }
          case returnType =>
            val returned = Variable(freshName("returned"), returnType)
            val boxed = Variable(freshName("boxed"), Positive())
            val received = Variable(freshName("boxed"), Positive())
            val unboxed = Variable(freshName("unboxed"), returnType)
            transform(body).map { body =>
              PushFrame(Clause(List(received), Coerce(unboxed, received, Return(List(unboxed)))),
                Reset(prompt, Clause(List(returned), Coerce(boxed, returned, Return(List(boxed)))), body))
            }
        }

      case core.Alloc(ref, init, region, body) =>
        val temporary = Variable(freshName("temporaryStack"), Type.Stack())

        // TODO ref should be BlockParam
        noteParameter(ref, core.Type.TState(init.tpe))

        transform(body).flatMap { body =>
          transform(init).map { value =>
            Shift(temporary, Variable(transform(region), Type.Prompt()),
              Var(Variable(transform(ref), Type.Reference(value.tpe)), value, Positive(),
                Resume(temporary, body)))
          }.run(x => Trampoline.Done(x))
        }

      case core.Var(ref, init, capture, body) =>

        // TODO ref should be BlockParam
        noteParameter(ref, core.Type.TState(init.tpe))

        val returnType = transform(body.tpe)
        transform(body).flatMap { body =>
          transform(init).map { value =>
            Var(Variable(transform(ref), Type.Reference(value.tpe)), value, returnType, body)
          }.run(x => Trampoline.Done(x))
        }

      case core.Get(id, tpe, ref, capt, body) =>
        val variable = Variable(transform(id), transform(tpe))
        transform(body).map { body =>
          LoadVar(variable, Variable(transform(ref), Type.Reference(variable.tpe)), body)
        }

      case core.Put(ref, capt, arg, body) =>
        transform(body).flatMap { body =>
          transform(arg).map { value =>
            StoreVar(Variable(transform(ref), Type.Reference(value.tpe)), value, body)
          }.run(x => Trampoline.Done(x))
        }

      case core.Hole(tpe, span) => Trampoline.Done(machine.Statement.Hole(span))

      case _ =>
        ErrorReporter.abort(s"Unsupported statement: $stmt")
    }

  def transform(vargs: List[core.Expr])(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[List[Variable]] =
    traverse(vargs)(transform)

  def transformBlockArgs(bargs: List[core.Block])(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[List[Variable]] =
    traverse(bargs)(transformBlockArg)

  def transform(vargs: List[core.Expr], bargs: List[core.Block])(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[(List[Variable], List[Variable])] =
    for {
      values <- transform(vargs)
      blocks <- transformBlockArgs(bargs)
    } yield (values, blocks)

  def transformBlockArg(block: core.Block)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] =
    transformBlockArg(Variable(freshName("block"), Negative()), block)

  /**
   * Binds [[variable]] to the block, except for block parameters and unboxed values, which are returned as they are.
   */
  def transformBlockArg(variable: Variable, block: core.Block)(using BPC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] = block match {
    case core.BlockVar(id, tpe, _) if BPC.globals contains id =>
      val label = BPC.globals(id)
      shift { k =>
        PushFrame(Clause(List(variable), k(variable)), Jump(label, label.environment))
      }
    case core.BlockVar(id, tpe, capt) => getBlockInfo(id) match {
      case BlockInfo.Definition(_, parameters, _) =>
        // Passing a top-level function directly, so we need to eta-expand turning it into a closure
        // TODO cache the closure somehow to prevent it from being created on every call
        val label = transformLabel(id)
        shift { k =>
          New(variable, List(Clause(parameters,
            Jump(label, label.environment)
          )), k(variable))
        }
      case BlockInfo.Parameter(tpe) =>
        pure(Variable(transform(id), transform(tpe)))
    }

    case core.BlockLit(tparams, cparams, vparams, bparams, body) =>
      noteParameters(bparams)
      val parameters = vparams.map(transform) ++ bparams.map(transform);
      shift { k =>
        New(variable, List(Clause(parameters, transform(body).run())), k(variable))
      }

    case core.New(impl) =>
      shift { k =>
        New(variable, transform(impl), k(variable))
      }

    case core.Unbox(pure) =>
      transform(pure)
  }

  def transform(expr: core.Expr)(using BC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] = expr match {
    case core.ValueVar(id, tpe) if BC.globals contains id =>
      val label = BC.globals(id)
      val variable = Variable(freshName("x"), transform(expr.tpe))
      shift { k =>
        // TODO this might introduce too many pushes.
        PushFrame(Clause(List(variable), k(variable)),
          Jump(label, label.environment))
      }
    case core.ValueVar(id, tpe) =>
      pure(Variable(transform(id), transform(tpe)))
    case _ =>
      transformNamed(Variable(freshName("x"), transform(expr.tpe)), expr)
  }

  /**
    Must not be called on an expression that is a variable.
  */
  def transformNamed(variable: Variable, expr: core.Expr)(using BC: BlocksParamsContext, DC: DeclarationContext, E: ErrorReporter): Binding[Variable] = expr match {

    case core.ValueVar(id, tpe) =>
      ErrorReporter.panic(s"Must not be called on an expression that is a variable $expr.")

    case core.Literal((), core.Type.TUnit) =>
      shift { k =>
        Construct(variable, builtins.Unit, List(), k(variable))
      }

    case core.Literal(value: Boolean, core.Type.TBoolean) =>
      shift { k =>
        Construct(variable, if (value) builtins.True else builtins.False, List(), k(variable))
      }

    case core.Literal(value: Long, core.Type.TInt) =>
      shift { k => LiteralInt(variable, value, k(variable)) }

    case core.Literal(value: Int, core.Type.TChar) =>
      shift { k => LiteralInt(variable, value, k(variable)) }

    case core.Literal(value: Byte, core.Type.TByte) =>
      shift { k => LiteralByte(variable, UByte.unsafeFromByte(value).toInt, k(variable)) }

    case core.Literal(v: Double, core.Type.TDouble) =>
      shift { k => LiteralDouble(variable, v, k(variable)) }

    case core.Literal(javastring: String, core.Type.TString) =>
      shift { k =>
        LiteralUTF8String(variable, javastring.getBytes("utf-8"), k(variable))
      }

    case core.PureApp(core.BlockVar(blockName, core.BlockType.Function(_, _, vparamTypes, _, resultType), _), _, vargs) =>
      transform(vargs).flatMap { values =>
        coerce(values, vparamTypes map transformExtern).flatMap { coerced =>
          shift { k => foreignCall(variable, transform(blockName), coerced, transformExtern(resultType), k(variable)) }
        }
      }

    case core.Make(data, constructor, targs, vargs) =>
      val tag = DeclarationContext.getConstructorTag(constructor)

      transform(vargs).flatMap { values =>
        shift { k =>
          Construct(variable, tag, values, k(variable))
        }
      }

    case core.Box(block, annot) =>
      transformBlockArg(variable, block)

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
        Clause(vparams.map(transform) ++ bparams.map(transform), transform(body).run())
    }

  def transform(param: core.ValueParam)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Variable =
    param match {
      case core.ValueParam(name, tpe) =>
        Variable(transform(name), transform(tpe))
    }

  def transform(param: core.BlockParam)(using BlocksParamsContext, DeclarationContext, ErrorReporter): Variable =
    param match {
      case core.BlockParam(name, tpe, capt) =>
        Variable(transform(name), transform(tpe))
    }

  def transform(tpe: core.ValueType)(using DeclarationContext, ErrorReporter): Type = tpe match {
    case core.Type.TInt => Type.Int()
    case core.Type.TChar => Type.Int()
    case core.Type.TByte => Type.Byte()
    case core.Type.TDouble => Type.Double()
    case core.ValueType.Data(name, _) if isValidExternC(name) => getExternCTpe(name)
    case core.ValueType.Data(_, _) => Positive()
    case core.ValueType.Boxed(_, _) => Negative()
    case core.ValueType.Var(name) => ErrorReporter.panic(s"Unexpected type variable ${name} after monomorphization")
  }

  /**
   * Types in the signatures of externs, where type variables and boxed extern interfaces are represented as positive values.
   */
  def transformExtern(tpe: core.ValueType)(using DeclarationContext, ErrorReporter): Type = tpe match {
    case core.ValueType.Var(_) => Positive()
    case core.ValueType.Boxed(core.BlockType.Interface(_: symbols.ExternInterface, _), _) => Positive()
    case _ => transform(tpe)
  }

  def transform(tpe: core.BlockType)(using DeclarationContext, ErrorReporter): Type = tpe match {
    case core.Type.TState(stateType) => Type.Reference(transform(stateType))
    case core.Type.TPrompt(answer) => Type.Prompt()
    case core.Type.TResume(result, answer) => Type.Stack()
    case core.Type.TRegion => Type.Prompt()
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) => Negative()
    case core.BlockType.Interface(symbol, targs) => Negative()
  }

  def transformLabel(id: Id)(using BPC: BlocksParamsContext): Label = getBlockInfo(id) match {
    case BlockInfo.Definition(freeParams, boundParams, _) => Label(transform(id), boundParams ++ freeParams)
    case BlockInfo.Parameter(_) => sys error s"Expected a function definition, but got a block parameter: ${id}"
  }

  def transform(id: Id): String =
    s"${id.name}_${id.id}"

  def coerce(value: Variable, tpe: Type): Binding[Variable] =
    if value.tpe == tpe then pure(value)
    else
      val coerced = Variable(freshName("coerced"), tpe)
      shift { k => Coerce(coerced, value, k(coerced)) }

  def coerce(values: List[Variable], tpes: List[Type]): Binding[List[Variable]] =
    traverse(values.zip(tpes)) { case (value, tpe) => coerce(value, tpe) }

  def foreignCall(variable: Variable, name: String, arguments: Environment, returnType: Type, rest: Statement): Statement =
    if (returnType == variable.tpe) {
      ForeignCall(variable, name, arguments, rest)
    } else {
      val returned = Variable(freshName("returned"), returnType)
      ForeignCall(returned, name, arguments, Coerce(variable, returned, rest))
    }

  def isParameter(id: Id)(using BPC: BlocksParamsContext): Boolean =
    BPC.info.get(id).exists { case BlockInfo.Parameter(_) => true; case _ => false }

  def freshName(baseName: String): String = baseName + "_" + symbols.Symbol.fresh.next()

  def findToplevelBlocksParams(definitions: List[core.Toplevel])(using BlocksParamsContext, DeclarationContext, ErrorReporter): Unit =
    definitions.foreach {
      case Toplevel.Def(id, core.BlockLit(tparams, cparams, vparams, bparams, body)) =>
        noteDefinition(id, vparams.map(transform) ++ bparams.map(transform), Nil)
        noteParameters(bparams)
      case Toplevel.Val(id, binding) =>
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
    var info: Map[Symbol, BlockInfo] = Map()
    var globals: Map[Id, Label] = Map()
    var definitions: List[Definition] = List.empty
  }

  enum BlockInfo {
    case Definition(free: Environment, params: Environment, async: Boolean)
    case Parameter(tpe: core.BlockType)
  }

  def DeclarationContext(using DC: DeclarationContext): DeclarationContext = DC

  def noteDefinition(id: Id, params: Environment, free: Environment, async: Boolean)(using BC: BlocksParamsContext): Unit =
    assert(!BC.info.isDefinedAt(id), s"Registering info twice for ${id} (was: ${BC.info(id)}, now: Definition)")
    BC.info += (id -> BlockInfo.Definition(free, params, async))

  def noteDefinition(id: Id, params: Environment, free: Environment)(using BC: BlocksParamsContext): Unit =
    noteDefinition(id, params, free, false)

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
    Binding { k => Trampoline.Done(body { x => k(x).run() }) }

  case class Binding[A](body: (A => Trampoline[Statement]) => Trampoline[Statement]) {
    def flatMap[B](rest: A => Binding[B]): Binding[B] = {
      Binding(k => Trampoline.More { () => body(a => Trampoline.More { () => rest(a).body(k) }) })
    }
    def run(k: A => Trampoline[Statement]): Trampoline[Statement] = body(k)
    def map[B](f: A => B): Binding[B] = flatMap { a => pure(f(a)) }
  }

  def traverse[S, T](l: List[S])(f: S => Binding[T]): Binding[List[T]] =
    l match {
      case Nil => pure(Nil)
      case head :: tail => for { x <- f(head); xs <- traverse(tail)(f) } yield x :: xs
    }

  def pure[A](a: A): Binding[A] = Binding(k => k(a))
}
