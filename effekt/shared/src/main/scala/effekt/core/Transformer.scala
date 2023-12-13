package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{Annotations, Context, ContextOps}
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.context.assertions.*
import effekt.source.{MatchPattern, Term}
import effekt.typer.Substitutions

object Transformer extends Phase[Typechecked, CoreTransformed] {

  val phaseName = "transformer"

  def run(input: Typechecked)(using Context) =
    val Typechecked(source, tree, mod) = input
    Context.initTransformerState()
    Some(CoreTransformed(source, tree, mod, transform(mod, tree)))

  def transform(mod: Module, tree: source.ModuleDecl)(using Context): ModuleDecl = {
    val source.ModuleDecl(path, imports, defs) = tree
    val exports = mod.terms.flatMap {
      case (name, syms) => syms.collect {
        // TODO export valuebinders properly
        case sym: Callable if !sym.isInstanceOf[Operation] && !sym.isInstanceOf[Field] => sym
        case sym: ValBinder => sym
      }
    }.toList

    val toplevelDeclarations = defs.flatMap(d => transformToplevel(d))

    val definitions = toplevelDeclarations.collect { case d: Definition => d }
    val externals = toplevelDeclarations.collect { case d: Extern => d }
    val declarations = toplevelDeclarations.collect { case d: Declaration => d }

    // We use the imports on the symbol (since they include the prelude)
    ModuleDecl(path, mod.imports.map { _.path }, declarations, externals, definitions, exports)
  }

  def transformToplevel(d: source.Def)(using Context): List[Definition | Declaration | Extern] = d match {
    case f @ source.FunDef(id, tps, vps, bps, ret, body) =>
      val tparams = tps.map { p => p.symbol }
      val cparams = bps.map { b => b.symbol.capture }
      val vparams = vps map transform
      val bparams = bps map transform
      List(Definition.Def(f.symbol, BlockLit(tparams, cparams, vparams, bparams, transform(body))))

    case d @ source.DataDef(id, _, ctors) =>
      val datatype = d.symbol
      List(Data(datatype, datatype.tparams, datatype.constructors.map(transform)))

    case d @ source.RecordDef(id, _, _) =>
      val rec = d.symbol
      List(Data(rec, rec.tparams, List(transform(rec.constructor))))

    case v @ source.ValDef(_, binding, _) if pureOrIO(binding) =>
      List(Definition.Let(v.boundSymbols, Run(transform(binding)))) // TODO MRV: symbol must return list

    case v @ source.ValDef(_, binding, _) =>
      Context.at(d) { Context.abort("Effectful bindings not allowed on the toplevel") }

    case v @ source.DefDef(id, annot, binding) =>
      val sym = v.symbol
      val (definition, bindings) = Context.withBindings {
        Definition.Def(sym, transformAsBlock(binding))
      }

      // convert binding into Definition.
      val additionalDefinitions = bindings.toList.map {
        case Binding.Let(names, binding) => Definition.Let(names, binding)
        case Binding.Def(name, binding) => Definition.Def(name, binding)
        case Binding.Val(names, binding) => Context.at(d) { Context.abort("Effectful bindings not allowed on the toplevel") }
      }
      additionalDefinitions ++ List(definition)

    case v @ source.VarDef(id, _, reg, binding) =>
      Context.at(d) { Context.abort("Mutable variable bindings currently not allowed on the toplevel") }

    case d @ source.InterfaceDef(id, tparamsInterface, ops, isEffect) =>
      val interface = d.symbol
      List(core.Interface(interface, interface.tparams, interface.operations.map {
        case op @ symbols.Operation(name, tps, vps, resultType, effects, interface) =>
          // like in asSeenFrom we need to make up cparams, they cannot occur free in the result type
          val capabilities = effects.canonical
          val tparams = tps.drop(tparamsInterface.size)
          val bparams = capabilities.map(transform)
          val vparams = vps.map(p => transform(p.tpe.get))
          val cparams = capabilities.map { tpe => symbols.CaptureParam(tpe.name) }

          // here we reconstruct the block type
          val btpe = core.BlockType.Function(tparams, cparams, vparams, bparams, resultType map transform)
          core.Property(op, btpe)

      }))

    case f @ source.ExternDef(pure, id, _, vps, bps, _, body) =>
      val sym@ExternFunction(name, tps, _, _, ret, effects, capt, _) = f.symbol
      assert(effects.isEmpty)
      val cps = bps.map(b => b.symbol.capture)
      List(Extern.Def(sym, tps, cps, vps map transform, bps map transform, ret.map(transform), transform(capt), body))


    case e @ source.ExternInclude(path, contents, _) =>
      List(Extern.Include(contents.get))

    // For now we forget about all of the following definitions in core:
    case d: source.Def.Extern => Nil
    case d: source.Def.Alias => Nil
  }

  def transform(c: symbols.Constructor)(using Context): core.Constructor =
    core.Constructor(c, c.fields.map(f => core.Field(f, transform(f.returnType))))

  def transform(tree: source.Stmt)(using Context): Stmt = tree match {
    // { e; stmt } --> { let _ = e; stmt }
    case source.ExprStmt(e, rest) if pureOrIO(e) =>
      val (expr, bs) = Context.withBindings { transformAsExpr(e) }
      val wildcards = List.tabulate(expr.tpe.size)(_ => Wildcard())
      val let = Let(wildcards, expr, transform(rest)) // TODO MRV: Wildcards()
      Context.reifyBindings(let, bs)

    // { e; stmt } --> { val _ = e; stmt }
    case source.ExprStmt(e, rest) =>
      val (_, bindings) = Context.withBindings { transformAsPure(e) }
      Context.reifyBindings(transform(rest), bindings)

    // return e
    case source.Return(e) => insertBindings {
      Return(e.flatMap(transformAsPure))
    }

    // simply drop superfluous {}s
    case source.BlockStmt(b) =>
      transform(b)

    case source.DefStmt(d, rest) => d match {
      case f @ source.FunDef(id, tps, vps, bps, ret, body) =>
        val tparams = tps.map { p => p.symbol }
        val cparams = bps.map { b => b.symbol.capture }
        val vparams = vps map transform
        val bparams = bps map transform
        Def(f.symbol, BlockLit(tparams, cparams, vparams, bparams, transform(body)), transform(rest))

      case v @ source.ValDef(_, binding, _) if pureOrIO(binding) =>
        Let(v.boundSymbols, Run(transform(binding)), transform(rest))

      case v @ source.ValDef(_, binding, _) =>
        Val(v.boundSymbols, transform(binding), transform(rest))

      case v @ source.DefDef(id, annot, binding) =>
        val sym = v.symbol
        insertBindings {
          Def(sym, transformAsBlock(binding), transform(rest))
        }

      case v @ source.VarDef(id, _, reg, binding) =>
        val sym = v.symbol
        insertBindings {
          Context.bind(transform(binding)) match {
            case List(v) => State(sym, v, sym.region, transform(rest))
            case _ => ??? // TODO MRV
          }
        }

      case d: source.Def.Extern => Context.panic("Only allowed on the toplevel")
      case d: source.Def.Declaration => Context.panic("Only allowed on the toplevel")

      // For now we forget about all of the following definitions in core:
      case d: source.Def.Alias => transform(rest)
    }
  }

  def transformUnbox(tree: source.Term)(implicit C: Context): Block =
    transformAsPure(tree) match {
      case List(v) => Unbox(v)
      case _ => ??? // TODO MRV 29: Unbox
    }

  def transformBox(tree: source.Term)(implicit C: Context): Pure =
    Box(transformAsBlock(tree), transform(Context.inferredCapture(tree)))

  /**
   * Transforms the source to a function (expecting to be called using [[core.Stmt.App]] or an interface.
   */
  def transformAsBlock(tree: source.Term)(using Context): Block = tree match {
    case v: source.Var =>
      val sym = v.definition
      Context.blockTypeOf(sym) match {
        case _: BlockType.FunctionType => transformAsControlBlock(tree)
        case _: BlockType.InterfaceType => transformAsObject(tree)
      }
    case _: source.BlockLiteral => transformAsControlBlock(tree)
    case _: source.New => transformAsObject(tree)
    case _ => transformUnboxOrSelect(tree)
  }
  private def transformUnboxOrSelect(tree: source.Term)(using Context): Block = tree match {
    case s @ source.Select(receiver, id) =>
      Member(transformAsObject(receiver), s.definition, transform(Context.inferredBlockTypeOf(tree)))

    case source.Unbox(b) => transformAsPure(b) match {
      case List(v) => Unbox(v)
      case _ => ??? // TODO MRV 29: Unbox
    }

    case _ =>
      transformUnbox(tree)
  }
  /**
   * Transforms the source to an interface block
   */
  def transformAsObject(tree: source.Term)(using Context): Block = tree match {
    case v: source.Var =>
      BlockVar(v.definition.asInstanceOf[BlockSymbol])

    case source.BlockLiteral(tparams, vparams, bparams, body) =>
      Context.panic(s"Using block literal ${tree} but an object was expected.")

    case source.New(impl) =>
      New(transform(impl, false))

    case _ => transformUnboxOrSelect(tree)
  }
  /**
   * Transforms the source to a function block that expects to be called using [[core.Stmt.App]].
   */
  def transformAsControlBlock(tree: source.Term)(using Context): Block = tree match {
    case v: source.Var =>
      val sym = v.definition
      val tpe = Context.blockTypeOf(sym)
      tpe match {
        case BlockType.FunctionType(tparams, cparams, vparamtps, bparamtps, restpe, effects) =>
          // if this block argument expects to be called using PureApp or DirectApp, make sure it is
          // by wrapping it in a BlockLit
          val targs = tparams.map(core.ValueType.Var.apply)
          val vparams: List[Param.ValueParam] = vparamtps.map { t => Param.ValueParam(TmpValue(), transform(t))}
          val vargs = vparams.map { case Param.ValueParam(id, tpe) => Pure.ValueVar(id, tpe) }

          // [[ f ]] = { (x) => f(x) }
          def etaExpandPure(b: Constructor | ExternFunction): BlockLit = {
            assert(bparamtps.isEmpty)
            assert(effects.isEmpty)
            assert(cparams.isEmpty)
            BlockLit(tparams, Nil, vparams, Nil,
              Stmt.Return(List(PureApp(BlockVar(b), targs, vargs))))
          }

          // [[ f ]] = { (x){g} => let r = f(x){g}; return r }
          def etaExpandDirect(f: ExternFunction): BlockLit = {
            assert(effects.isEmpty)
            val bparams: List[Param.BlockParam] = bparamtps.map { t => Param.BlockParam(TmpBlock(), transform(t)) }
            val bargs = bparams.map {
              case Param.BlockParam(id, tpe) => Block.BlockVar(id, tpe, Set(id))
            }
            val result = TmpValue()
            val results = List.tabulate(restpe.size)(_ => TmpValue())

            BlockLit(tparams, bparams.map(_.id), vparams, bparams,
              core.Let(results,
                DirectApp(BlockVar(f), targs, vargs, bargs),
                Stmt.Return((results zip restpe.map(transform)).map { (res, tpe) => Pure.ValueVar(res, tpe) })))
          }

          sym match {
            case _: ValueSymbol => transformUnbox(tree)
            case cns: Constructor => etaExpandPure(cns)
            case f: ExternFunction if isPure(f.capture) => etaExpandPure(f)
            case f: ExternFunction if pureOrIO(f.capture) => etaExpandDirect(f)
            // does not require change of calling convention, so no eta expansion
            case sym: BlockSymbol => BlockVar(sym)
          }
        case t: BlockType.InterfaceType =>
          Context.abort(s"Expected a function but got an object of type ${t}")
      }

    case source.BlockLiteral(tps, vps, bps, body) =>
      val tparams = tps.map(t => t.symbol)
      val cparams = bps.map { b => b.symbol.capture }
      BlockLit(tparams, cparams, vps map transform, bps map transform, transform(body))

    case s @ source.New(impl) =>
      Context.abort(s"Expected a function but got an object instantiation: ${s}")

    case _ => transformUnboxOrSelect(tree)
  }

  def transformAsPure(tree: source.Term)(using Context): List[Pure] = transformAsExpr(tree) match {
    case p: Pure => List(p)
    case e: Expr => Context.bind(e)
  }

  def transformAsExpr(tree: source.Term)(using Context): Expr = tree match {
    case v: source.Var => v.definition match {
      case sym: VarBinder =>
        val stateType = Context.blockTypeOf(sym)
        val getType = operationType(stateType, TState.get)
        Context.bind(App(Member(BlockVar(sym), TState.get, transform(getType)), Nil, Nil, Nil)) match {
          case List(v) => v
          case _ => ??? // TODO MRV
        }
      case sym: ValueSymbol => ValueVar(sym)
      case sym: BlockSymbol => transformBox(tree)
    }

    case source.Literal(value, tpe) =>
      Literal(value, transform(tpe))

    case s @ source.Select(receiver, selector) =>
      Context.inferredTypeOf(s) match {
        case List(tpe) => transformAsPure(receiver) match {
          case List(receiver) => Select(receiver, s.definition, transform(tpe))
          case _ => Context.panic("Can only select from single valued expressions.") // TODO MRV
        }
        case _ => Context.panic("Can only select from single valued expressions.") // TODO MRV
      }

    case source.Box(capt, block) =>
      transformBox(block)

    case source.New(impl) =>
      transformBox(tree)

    case source.Unbox(b) =>
      transformBox(tree)

    case source.BlockLiteral(tps, vps, bps, body) =>
      transformBox(tree)

    case source.If(cond, thn, els) =>
      val c = transformAsPure(cond)
      if (c.isEmpty) Context.panic("Condition to if is not pure.")
      else if (c.length != 1) Context.panic("Condition to if must return exactly one value.")
      else Context.bind(If(c.head, transform(thn), transform(els))) match {
        case List(v) => v
        case _ => ??? // TODO MRV
      }

    // [[ while(cond) { body } ]] =
    //   def loop$13() = if ([[cond]]) { [[ body ]]; loop$13() } else { return () }
    //   loop$13()
    case source.While(cond, body) =>
      val loopName = TmpBlock()
      val loopType = core.BlockType.Function(Nil, Nil, Nil, Nil, List(core.Type.TUnit))
      val loopCapt = transform(Context.inferredCapture(body))
      val loopCall = Stmt.App(core.BlockVar(loopName, loopType, loopCapt), Nil, Nil, Nil)

      val condT = transformAsPure(cond)
      if (condT.isEmpty) Context.panic("No condition to while loop.")
      else if (condT.length != 1) Context.panic("Condition to while loop must return exactly one value.")

      val loop = Block.BlockLit(Nil, Nil, Nil, Nil,
        insertBindings {
          Stmt.If(condT.head,
            Stmt.Val(List(TmpValue()), transform(body), loopCall),
            Return(List(Literal((), core.Type.TUnit))))
        }
      )

      Context.bind(loopName, loop)

      // captures???
      if (Context.inferredCapture(cond) == CaptureSet.empty) Context.at(cond) {
        Context.warning(pp"Condition to while loop is pure, which might not be intended.")
      }

      Context.bind(loopCall) match {
        case List(v) => v
        case _ => ??? // TODO MRV
      }

    case source.Match(sc, cs) =>
      // (1) Bind scrutinee and all clauses so we do not have to deal with sharing on demand.
      val scrutinee: List[ValueVar] = transformAsPure(sc) match {
        case List(v) => Context.bind(v)
        case _ => ??? // TODO MRV
      }
      val clauses = cs.map(c => preprocess(scrutinee, c))
      val compiledMatch = Context.at(tree) { compileMatch(clauses) }
      Context.bind(compiledMatch) match {
        case List(v) => v
        case _ => ??? // TODO MRV
      }

    case source.TryHandle(prog, handlers) =>
      val (bps, cps) = handlers.map { h =>
        val cap = h.capability.get.symbol
        (BlockParam(cap), cap.capture)
      }.unzip

      val body = BlockLit(Nil, cps, Nil, bps, transform(prog))

      val transformedHandlers = handlers.map {
        case h @ source.Handler(cap, impl) => transform(impl, true)
      }
      Context.bind(Try(body, transformedHandlers)) match {
        case List(v) => v
        case _ => ??? // TODO MRV
      }

    case r @ source.Region(name, body) =>
      val region = r.symbol
      val tpe = Context.blockTypeOf(region)
      val cap: core.BlockParam = core.BlockParam(region, transform(tpe))
      Context.bind(Region(BlockLit(Nil, List(region.capture), Nil, List(cap), transform(body)))) match {
        case List(v) => v
        case _ => ??? // TODO MRV
      }

    case source.Hole(stmts) =>
      Context.bind(Hole()) match {
        case List(hole) => hole
        case _ => ??? // TODO MRV: error message?
      }

    case a @ source.Assign(id, expr) =>
      val e = transformAsPure(expr)
      val sym = a.definition
      val stateType = Context.blockTypeOf(sym)
      val putType = operationType(stateType, TState.put)
      Context.bind(App(Member(BlockVar(sym), TState.put, transform(putType)), Nil, e, Nil)) match {
        case List(v) => v
        case _ => ??? // TODO MRV: error message?
      }

    // methods are dynamically dispatched, so we have to assume they are `control`, hence no PureApp.
    case c @ source.MethodCall(receiver, id, targs, vargs, bargs) =>
      val rec = transformAsObject(receiver)
      val typeArgs = Context.typeArguments(c).map(transform)
      val valueArgs = vargs.flatMap(transformAsPure) // TODO MRV: flatMap ok?
      val blockArgs = bargs.map(transformAsBlock)

      // TODO if we always just use .capt, then why annotate it?
      // val captArgs = blockArgs.map(_.capt) //bargs.map(b => transform(Context.inferredCapture(b)))

      val receiverType = Context.inferredBlockTypeOf(receiver)
      val operation = c.definition.asOperation
      val opType = transform(operationType(receiverType, operation))

      // Do not pass type arguments for the type constructor of the receiver.
      val remainingTypeArgs = typeArgs.drop(operation.interface.tparams.size)

      Context.bind(App(Member(rec, operation, opType), remainingTypeArgs, valueArgs, blockArgs)) match {
        case List(v) => v
        case _ => ??? // TODO MRV
      }

    case c @ source.Call(source.ExprTarget(source.Unbox(expr)), targs, vargs, bargs) =>

      val (funTpe, capture) = Context.inferredTypeOf(expr) match {
        case List(BoxedType(s: FunctionType, c: CaptureSet)) => (s, c)
        case _ => Context.panic("Should be a boxed function type with a known capture set.")
      }
      val e = transformAsPure(expr) match {
        case List(v) => v
        case _ => ??? // TODO MRV
      }
      val typeArgs = Context.typeArguments(c).map(transform)
      val valueArgs = vargs.flatMap(transformAsPure)
      val blockArgs = bargs.map(transformAsBlock)
      // val captArgs = blockArgs.map(b => b.capt) //transform(Context.inferredCapture(b)))

      if (pureOrIO(capture) && bargs.forall { pureOrIO }) {
        Run(App(Unbox(e), typeArgs, valueArgs, blockArgs))
      } else {
        Context.bind(App(Unbox(e), typeArgs, valueArgs, blockArgs)) match {
          case List(v) => v
          case _ => ??? // TODO MRV 29: Unbox
        }
      }

    case c @ source.Call(fun: source.IdTarget, _, vargs, bargs) =>
      // assumption: typer removed all ambiguous references, so there is exactly one
      makeFunctionCall(c, fun.definition, vargs, bargs)

    case c @ source.Call(s: source.ExprTarget, targs, vargs, bargs) =>
      Context.panic("Should not happen. Unbox should have been inferred.")

    case source.Do(effect, id, targs, vargs) =>
      Context.panic("Should have been translated away (to explicit selection `@CAP.op()`) by capability passing.")
  }

  /**
   * Establishes a canonical ordering of methods by using
   * the order in which they are declared in the signature (like with handlers)
   */
  def transform(impl: source.Implementation, isHandler: Boolean)(using Context): core.Implementation = {
    val members = impl.clauses
    val clauses = members.map { cl => (cl.definition, cl) }.toMap
    val sig = impl.definition

    val coreType = transform(Context.inferredBlockTypeOf(impl)) match {
      case i: core.BlockType.Interface => i
      case _ => Context.panic("Should be an interface type.")
    }

    Implementation(coreType, sig.operations.map(clauses.apply).map {
      case op @ source.OpClause(id, tparams, vparams, ret, body, resume) =>
        val vps = vparams.map(transform)
        val tps = tparams.map { p => p.symbol }

        // We cannot annotate the transparent capture of resume here somewhere since all
        // block parameters are automatically tracked by our current encoding of core.Tree.
        // So resume is a separate parameter.
        val resumeParam = if (isHandler) {
          val resumeSymbol = resume.symbol.asInstanceOf[BlockSymbol]
          Some(BlockParam(resumeSymbol))
        } else { None }
        // TODO the operation could be effectful, so needs to take a block param here.
        val bps = Nil
        val cps = Nil
        core.Operation(op.definition, tps, cps, vps, bps, resumeParam, transform(body))
    })
  }


  /**
   * Computes the block type the selected symbol.
   *
   * For instance, receiver can be `State[Int]`, interface could be the symbol of `State` and member could be `get`.
   * If `member` is an operation, the type arguments of the receiver are substituted for the leading type parameters,
   * while the remaining type parameters are kept.
   */
  def operationType(receiver: symbols.BlockType, member: symbols.Operation)(using Context): BlockType = receiver.asInterfaceType match {
    case InterfaceType(i: Interface, targs) => member match {
      // For operations, we substitute the first type parameters by concrete type args.
      case Operation(name, tparams, vparams, resultType, effects, _) =>
        val substitution = Substitutions((tparams zip targs).toMap, Map.empty)
        val remainingTypeParams = tparams.drop(targs.size)
        val bparams = Nil
        // TODO this is exactly like in [[Typer.toType]] -- TODO repeated here:
        // TODO currently the return type cannot refer to the annotated effects, so we can make up capabilities
        //   in the future namer needs to annotate the function with the capture parameters it introduced.
        val cparams = effects.canonical.map { tpe => symbols.CaptureParam(tpe.name) }

        FunctionType(remainingTypeParams, cparams, vparams.map(t => substitution.substitute(t.tpe.get)), bparams, resultType.map(substitution.substitute), substitution.substitute(effects))
    }

    case InterfaceType(i: ExternInterface, targs) =>
      Context.panic("Cannot select from an extern interface")
  }

  def makeFunctionCall(call: source.CallLike, sym: TermSymbol, vargs: List[source.Term], bargs: List[source.Term])(using Context): Expr = {
    // the type arguments, inferred by typer
    val targs = Context.typeArguments(call).map(transform)
    // val cargs = bargs.map(b => transform(Context.inferredCapture(b)))

    val vargsT = vargs.flatMap(transformAsPure)
    val bargsT = bargs.map(transformAsBlock)

    sym match {
      case f: ExternFunction if isPure(f.capture) =>
        PureApp(BlockVar(f), targs, vargsT)
      case f: ExternFunction if pureOrIO(f.capture) =>
        DirectApp(BlockVar(f), targs, vargsT, bargsT)
      case r: Constructor =>
        if (bargs.nonEmpty) Context.abort("Constructors cannot take block arguments.")
        PureApp(BlockVar(r), targs, vargsT)
      case f: Operation =>
        Context.panic("Should have been translated to a method call!")
      case f: Field =>
        Context.panic("Should have been translated to a select!")
      case f: BlockSymbol if pureOrIO(f) && bargs.forall { pureOrIO } =>
        Run(App(BlockVar(f), targs, vargsT, bargsT))
      case f: BlockSymbol =>
        Context.bind(App(BlockVar(f), targs, vargsT, bargsT)) match {
          case List(x) => x
          case _ => ??? // TODO MRV
        }
      case f: ValueSymbol =>
        Context.bind(App(Unbox(ValueVar(f)), targs, vargsT, bargsT)) match {
          case List(x) => x
          case _ => ??? // TODO MRV 29: Unbox
        }
    }
  }

  def transform(p: source.BlockParam)(using Context): core.BlockParam = BlockParam(p.symbol)

  def transform(p: source.ValueParam)(using Context): core.ValueParam = ValueParam(p.symbol)

  def insertBindings(stmt: => Stmt)(using Context): Stmt = {
    val (body, bindings) = Context.withBindings { stmt }
    Context.reifyBindings(body, bindings)
  }

  // Translation on Types
  // --------------------
  def transform(tpe: ValueType)(using Context): core.ValueType = tpe match {
    case ValueType.BoxedType(tpe, capture) => core.ValueType.Boxed(transform(tpe), transform(capture))
    case ValueType.ValueTypeRef(tvar)      => core.ValueType.Var(tvar)
    case ValueType.ValueTypeApp(tc, args)  => core.ValueType.Data(tc, args.map(transform))
  }

  def transform(tpe: BlockType)(using Context): core.BlockType = tpe match {
    case BlockType.FunctionType(tparams, cparams, vparams, bparams, results, effects) =>

      val capabilityTypes = effects.canonical.map(transform)
      val allBlockParams = bparams.map(transform) ++ capabilityTypes

      assert(cparams.size == allBlockParams.size,
        s"""Internal error: number of block parameters does not match number of capture parameters.
           |
           |  Blockparams: ${bparams}
           |  Effects: ${capabilityTypes}
           |  Captures: ${cparams}
           |""".stripMargin)

      core.BlockType.Function(tparams, cparams, vparams.map(transform), allBlockParams, results.map(transform))

    case BlockType.InterfaceType(tc, args) => core.BlockType.Interface(tc, args.map(transform))
  }

  def transform(capt: Captures)(using Context): core.Captures = capt match {
    case CaptUnificationVar(role) => Context.panic(pp"$capt should be a concrete capture set in this phase.")
    case CaptureSet(captures) => captures.map(x => x: Symbol) // that is really a noop...
  }


  // Match Compiler
  // --------------
  // The implementation of the match compiler follows closely the short paper:
  //   Jules Jacob
  //   How to compile pattern matching
  //   https://julesjacobs.com/notes/patternmatching/patternmatching.pdf
  //
  // There also is a more advanced Rust implementation that we could look at:
  //   https://gitlab.com/yorickpeterse/pattern-matching-in-rust/-/tree/main/jacobs2021

  // case pats => label(args...)
  private case class Clause(patterns: Map[ValueVar, source.MatchPattern], label: BlockVar, args: List[ValueVar])

  // Uses the bind effect to bind the right hand sides of clauses!
  private def preprocess(sc: List[ValueVar], clause: source.MatchClause)(using Context): Clause = { // TODO MRV: scrutinee is list of ValueVar
    def boundVars(p: source.MatchPattern): List[ValueParam] = p match {
      case p @ source.AnyPattern(id) => List(p.symbol)
      case source.TagPattern(id, patterns) => patterns.flatMap(boundVars)
      case _ => Nil
    }
    val params = clause.patterns.flatMap(boundVars).map { p => (p, transform(Context.valueTypeOf(p))) }
    val body = transform(clause.body)
    val blockLit = BlockLit(Nil, Nil, params.map(core.ValueParam.apply), Nil, body)

    val joinpoint = Context.bind(TmpBlock(), blockLit)
    val patterns = (sc zip clause.patterns).toMap
    Clause(patterns, joinpoint, params.map{case (p, tpe) => core.ValueVar(p, tpe)})
  }

  /**
   * The match compiler works with
   * - a sequence of clauses that represent alternatives (disjunction)
   * - each sequence contains a list of patterns that all have to match (conjunction).
   */
  private def compileMatch(clauses: Seq[Clause])(using Context): core.Stmt = {

    // matching on void will result in this case
    if (clauses.isEmpty) return core.Hole()

    val normalizedClauses = clauses.map(normalize)

    def jumpToBranch(target: BlockVar, vargs: List[ValueVar]) =
      core.App(target, Nil, vargs, Nil)

    // (1) Check whether we are already successful
    val Clause(patterns, target, args) = normalizedClauses.head
    if (patterns.isEmpty) { return jumpToBranch(target, args) }

    def branchingHeuristic =
      patterns.keys.maxBy(v => normalizedClauses.count {
        case Clause(ps, _, _) => ps.contains(v)
      })

    // (2) Choose the variable to split on
    val splitVar = branchingHeuristic

    def mentionedVariants = normalizedClauses
      .flatMap(_.patterns.get(splitVar))
      .collect { case p : source.TagPattern => p.definition }

    val variants = mentionedVariants.distinct

    val clausesFor = collection.mutable.Map.empty[Constructor, Vector[Clause]]
    var defaults = Vector.empty[Clause]

    def addClause(c: Constructor, cl: Clause): Unit =
      val clauses = clausesFor.getOrElse(c, Vector.empty)
      clausesFor.update(c, clauses :+ cl)

    def addDefault(cl: Clause): Unit =
      defaults = defaults :+ cl

    var varsFor: Map[Constructor, List[core.ValueVar]] = Map.empty
    def fieldVarsFor(c: Constructor, pats: List[MatchPattern]): List[core.ValueVar] =
      varsFor.getOrElse(c, {
        val newVars: List[core.ValueVar] = pats.map { pat => Context.inferredTypeOf(pat) match {
          case List(tpe) => core.ValueVar(TmpValue(), transform(tpe))
          case _ => ??? // TODO MRV
        }
        }
        varsFor = varsFor.updated(c, newVars)
        newVars
      })

    normalizedClauses.foreach {
      case c @ Clause(patterns, target, args) => patterns.get(splitVar) match {
        case Some(p @ source.TagPattern(id, ps)) =>
          val constructor = p.definition
          val fieldVars = fieldVarsFor(constructor, ps)
          addClause(constructor, Clause(patterns - splitVar ++ fieldVars.zip(ps), target, args))
        case Some(_) => Context.panic("Should not happen")
        case None =>
          // Clauses that don't match on that var are duplicated.
          // So we want to choose our branching heuristic to minimize this
          addDefault(c)
          // THIS ONE IS NOT LINEAR
          variants.foreach { v => addClause(v, c) }
      }
    }

    val branches = variants.toList.map { v =>
      val body = compileMatch(clausesFor.getOrElse(v, Vector.empty))
      val params = varsFor(v).map { case ValueVar(id, tpe) => core.ValueParam(id, tpe): core.ValueParam }
      val blockLit: BlockLit = BlockLit(Nil, Nil, params, Nil, body)
      (v, blockLit)
    }

    val default = if defaults.isEmpty then None else Some(compileMatch(defaults))

    core.Match(splitVar, branches, default)
  }

  /**
   * Substitutes IdPatterns and removes wildcards (and literal patterns -- which are already ruled out by Typer);
   * only TagPatterns are left.
   */
  private def normalize(clause: Clause)(using Context): Clause = {
    val Clause(patterns, target, args) = clause
    val substitution = patterns.collect { case (v, source.AnyPattern(id)) => id.symbol -> v }
    val tagPatterns = patterns.collect { case (v, p: source.TagPattern) => v -> p }

    Clause(tagPatterns, target, args.map(v => substitution.getOrElse(v.id, v)))
  }


  // Helpers
  // -------

  // Helpers to constructed typed trees
  def ValueParam(id: ValueSymbol)(using Context): core.ValueParam =
    core.ValueParam(id, transform(Context.valueTypeOf(id)))

  def BlockParam(id: BlockSymbol)(using Context): core.BlockParam =
    core.BlockParam(id, transform(Context.blockTypeOf(id)))

  def ValueVar(id: ValueSymbol)(using Context): core.ValueVar =
    core.ValueVar(id, transform(Context.valueTypeOf(id)))

  def BlockVar(id: BlockSymbol)(using Context): core.BlockVar =
    core.BlockVar(id, transform(Context.blockTypeOf(id)), transform(Context.captureOf(id)))

  def asConcreteCaptureSet(c: Captures)(using Context): CaptureSet = c match {
    case c: CaptureSet => c
    case _ => Context.panic("All capture unification variables should have been replaced by now.")
  }

  // we can conservatively approximate to false, in order to disable the optimizations
  def pureOrIO(t: source.Tree)(using Context): Boolean =
    Context.inferredCaptureOption(t) match {
      case Some(capt) => pureOrIO(asConcreteCaptureSet(capt))
      case _         => false
    }

  def isPure(t: source.Tree)(using Context): Boolean = Context.inferredCaptureOption(t) match {
    case Some(capt) => asConcreteCaptureSet(capt).captures.isEmpty
    case _         => false
  }

  def pureOrIO(t: BlockSymbol)(using Context): Boolean = pureOrIO(asConcreteCaptureSet(Context.captureOf(t)))

  def isPure(r: CaptureSet): Boolean = r.captures.isEmpty

  def pureOrIO(r: CaptureSet): Boolean = r.captures.forall {
    c =>
      def isIO = c == builtins.IOCapability.capture
      def isMutableState = c.isInstanceOf[LexicalRegion]
      def isResource = c.isInstanceOf[Resource]
      def isControl = c == builtins.ControlCapability.capture
      !isControl && (isIO || isMutableState || isResource)
  }
}

private[core] enum Binding {
  case Val(names: List[TmpValue], binding: Stmt)
  case Let(names: List[TmpValue], binding: Expr)
  case Def(name: BlockSymbol, binding: Block)
}

trait TransformerOps extends ContextOps { Context: Context =>
  /**
   * A _mutable_ ListBuffer that stores all bindings to be inserted at the current scope
   */
  private var bindings: ListBuffer[Binding] = ListBuffer()

  private[core] def initTransformerState() = {
    bindings = ListBuffer()
  }

  /**
   * Introduces a binding for the given statement.
   *
   * @param tpe the type of the bound statement
   * @param s the statement to be bound
   */
  private[core] def bind(s: Stmt): List[ValueVar] = {

    // create a fresh symbol and assign the type
    val names = List.tabulate(s.tpe.size)(_ => TmpValue())

    val binding = Binding.Val(names, s)
    bindings += binding

    names zip s.tpe map { (x, v) => ValueVar(x, v) }
  }

  private[core] def bind(e: Expr): List[ValueVar] = {

    // create a fresh symbol and assign the type
    val names = List.tabulate(e.tpe.size)(_ => TmpValue())
    val binding = Binding.Let(names, e)
    bindings += binding

    names zip e.tpe map { (x, v) => ValueVar(x, v) }
  }

  private[core] def bind(name: BlockSymbol, b: Block): BlockVar = {
    val binding = Binding.Def(name, b)
    bindings += binding
    BlockVar(name, b.tpe, b.capt)
  }

  private[core] def withBindings[R](block: => R): (R, ListBuffer[Binding]) = Context in {
    // TODO MRV: the following looks very wrong
    val before = bindings
    val b = ListBuffer.empty[Binding]
    bindings = b
    val result = block
    bindings = before
    (result, b)
  }

  /**
   * When reifying bindings, insert let bindings and use RUN when statement is pure or IO.
   */
  private[core] def reifyBindings(body: Stmt, bindings: ListBuffer[Binding]): Stmt = {
    bindings.foldRight(body) {
      // optimization: remove unnecessary binds
      case (Binding.Val(x, b), Return(List(ValueVar(y, _)))) if x == y => b
      case (Binding.Val(x, b), body) => Val(x, b, body)
      case (Binding.Let(x, Run(s)), Return(List(ValueVar(y, _)))) if x == y => s
      case (Binding.Let(x, b: Pure), Return(List(ValueVar(y, _)))) if x == y => Return(List(b)) // TODO MRV
      case (Binding.Let(x, b), body) => Let(x, b, body)
      case (Binding.Def(x, b), body) => Def(x, b, body)
    }
  }
}
