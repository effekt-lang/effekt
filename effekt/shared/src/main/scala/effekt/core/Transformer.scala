package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.context.assertions.*
import effekt.core.PatternMatchingCompiler.Clause
import effekt.source.{ Many, MatchGuard, MatchPattern, ResolveExternDefs }
import effekt.symbols.Binder.{ RegBinder, VarBinder }
import effekt.typer.{ Coercion, Substitutions }
import effekt.util.messages.{ ErrorReporter, INTERNAL_ERROR }

object Transformer extends Phase[Typechecked, CoreTransformed] {

  val phaseName = "transformer"

  def run(input: Typechecked)(using Context) =
    val Typechecked(source, tree, mod) = input
    Context.initTransformerState()

    if (Context.messaging.hasErrors) {
      None
    } else {
      val transformed = Context.timed(phaseName, source.name) { transform(mod, tree) }
      Some(CoreTransformed(source, tree, mod, transformed))
    }

  enum CallingConvention {
    case Pure, Direct, Control
  }
  def callingConvention(callable: Callable)(using Context): CallingConvention = callable match {
    case f @ ExternFunction(name, _, _, _, _, _, capture, bodies, _) =>
      // resolve the preferred body again and hope it's the same
      val body = ResolveExternDefs.findPreferred(bodies)
      body match {
        case b: source.ExternBody.EffektExternBody => CallingConvention.Control
        case _ if f.capture.pure => CallingConvention.Pure
        case _ if f.capture.pureOrIO => CallingConvention.Direct
        case _ => CallingConvention.Control
      }
    case _ => CallingConvention.Control
  }

  def transform(mod: Module, tree: source.ModuleDecl)(using Context): ModuleDecl = Context.using(mod) {
    val source.ModuleDecl(path, imports, defs, doc, span) = tree
    val exports = transform(mod.exports)
    val toplevelDeclarations = defs.flatMap(d => transformToplevel(d))

    val definitions = toplevelDeclarations.collect { case d: Toplevel => d }
    val externals = toplevelDeclarations.collect { case d: Extern => d }
    val declarations = toplevelDeclarations.collect { case d: Declaration => d }

    // add data declarations for Bottom
    val preludeDeclarations = if (mod.isPrelude)
      List(Declaration.Data(builtins.BottomSymbol, Nil, Nil))
    else Nil

    // We use the includes on the symbol (since they include the prelude)
    ModuleDecl(path, mod.includes.map { _.path }, preludeDeclarations ++ declarations, externals, definitions, exports)
  }

  def transformToplevel(d: source.Def)(using Context): List[Toplevel | Declaration | Extern] = d match {
    case f @ source.FunDef(id, tps, vps, bps, cpt, ret, body, doc, span) =>
      val tparams = tps.map { p => p.symbol }
      val cparams = bps.map { b => b.symbol.capture }
      val vparams = vps map transform
      val bparams = bps map transform
      List(Toplevel.Def(f.symbol, BlockLit(tparams.unspan, cparams.unspan, vparams.unspan, bparams.unspan, transform(body))))

    case d @ source.DataDef(id, _, ctors, doc, span) =>
      val datatype = d.symbol
      List(Data(datatype, datatype.tparams, datatype.constructors.map(transform)))

    case d @ source.RecordDef(id, _, _, doc, span) =>
      val rec = d.symbol
      List(Data(rec, rec.tparams, List(transform(rec.constructor))))

    case v @ source.ValDef(id, tpe, binding, doc, span) if isPureOrIO(binding) =>
      val transformed = transform(binding)
      // TODO what to do with the potentially annotated type here?
      List(Toplevel.Val(v.symbol, transformed))

    case v @ source.ValDef(id, _, binding, doc, span) =>
      Context.at(d) { Context.abort("Effectful bindings not allowed on the toplevel") }

    case v @ source.DefDef(id, captures, annot, binding, doc, span) =>
      val sym = v.symbol
      val (definition, bindings) = Context.withBindings {
        Toplevel.Def(sym, transformAsBlock(binding))
      }

      bindings.map(core.Binding.toToplevel) ++ List(definition)

    case _: source.VarDef | _: source.RegDef =>
      Context.at(d) { Context.abort("Mutable variable bindings not allowed on the toplevel") }

    case d @ source.InterfaceDef(id, tparamsInterface, ops, doc, span) =>
      val interface = d.symbol
      List(core.Interface(interface, interface.tparams,
        interface.operations.map { op => core.Property(op, operationAtDeclaration(interface.tparams, op)) }))

    case f @ source.ExternDef(id, _, vps, bps, _, _, bodies, doc, span) =>
      val sym@ExternFunction(name, tps, _, _, ret, effects, capt, _, _) = f.symbol
      assert(effects.isEmpty)
      val cps = bps.map(b => b.symbol.capture)
      val tBody = bodies match {
        case source.ExternBody.StringExternBody(ff, body, span) :: Nil =>
          ExternBody.StringExternBody(ff, Template(body.strings, body.args.map(transformAsExpr)))
        case source.ExternBody.Unsupported(err) :: Nil =>
          ExternBody.Unsupported(err)
        case _ =>
          Context.abort("Externs should be resolved and desugared before core.Transformer")
      }
      List(Extern.Def(sym, tps, cps.unspan, vps.unspan map transform, bps.unspan map transform, transform(ret), transform(capt), tBody))

    case e @ source.ExternInclude(ff, path, contents, _, doc, span) =>
      List(Extern.Include(ff, contents.get))

    case d @ source.NamespaceDef(name, defs, doc, span) =>
      defs.flatMap(transformToplevel)

    // For now we forget about all of the following definitions in core:
    case d: source.Def.Extern => Nil
    case d: source.Def.Alias => Nil
  }

  /**
   * In core, we only export value binders and proper functions
   */
  def transform(exports: Bindings): List[Id] = exports.terms.flatMap {
    case (name, syms) => syms.collect {
      case sym: Callable if !sym.isInstanceOf[Operation] && !sym.isInstanceOf[Field] => sym
      case sym: ValBinder => sym
    }
  }.toList ++ exports.namespaces.values.flatMap(transform)

  def transform(c: symbols.Constructor)(using Context): core.Constructor =
    core.Constructor(c, c.tparams, c.fields.map(f => core.Field(f, transform(f.returnType))))

  def transform(tree: source.Stmt)(using Context): Stmt = coercing(tree) {
    // { e; stmt } --> { let _ = e; stmt }
    // TODO this doesn't preserve termination
    case source.ExprStmt(e, rest, span) if isPure(e) =>
      transform(rest)

    // { e; stmt } --> { val _ = e; stmt }
    case source.ExprStmt(e, rest, span) =>
      val binding = insertBindings { Return(transformAsExpr(e)) }
      Val(Wildcard(), binding, transform(rest))

    // return e
    case source.Return(e, span) =>
      insertBindings { Return(transformAsExpr(e)) }

    // simply drop superfluous {}s
    case source.BlockStmt(b, span) =>
      transform(b)

    case source.DefStmt(d, rest, span) => d match {
      case f @ source.FunDef(id, tps, vps, bps, cpt, ret, body, doc, span) =>
        val tparams = tps.map { p => p.symbol }
        val cparams = bps.map { b => b.symbol.capture }
        val vparams = vps map transform
        val bparams = bps map transform
        Def(f.symbol, BlockLit(tparams.unspan, cparams.unspan, vparams.unspan, bparams.unspan, transform(body)), transform(rest))

      case v @ source.ValDef(id, tpe, binding, doc, span) =>
        val transformed = transform(binding)
        Val(v.symbol, transformed, transform(rest))

      case v @ source.DefDef(id, captures, annot, binding, doc, span) =>
        val sym = v.symbol
        insertBindings {
          Def(sym, transformAsBlock(binding), transform(rest))
        }

      case v @ source.RegDef(id, _, reg, binding, doc, span) =>
        val sym = v.symbol
        insertBindings {
          Alloc(sym, Context.bind(transform(binding)), sym.region, transform(rest))
        }

      case v @ source.VarDef(id, _, binding, doc, span) =>
        val sym = v.symbol
        insertBindings {
          Var(sym, Context.bind(transform(binding)), sym.capture, transform(rest))
        }

      case d: source.Def.Extern => Context.panic("Only allowed on the toplevel")
      case d: source.Def.Declaration => Context.panic("Only allowed on the toplevel")
      case d: source.Def.NamespaceDef => Context.panic("Only allowed on the toplevel")

      // For now we forget about all of the following definitions in core:
      case d: source.Def.Alias => transform(rest)
    }
  }

  def transformUnbox(tree: source.Term)(implicit C: Context): Block = tree match {
    case source.Unbox(b, _) => Unbox(transformAsExpr(b))
    case _ => Unbox(transformAsExpr(tree))
  }

  def transformBox(tree: source.Term)(implicit C: Context): Expr = tree match {
    case source.Box(capt, block, _) => Box(transformAsBlock(tree), transform(Context.inferredCapture(block)))
    case _ => Box(transformAsBlock(tree), transform(Context.inferredCapture(tree)))
  }

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
    case _ => transformUnbox(tree)
  }

  /**
   * Transforms the source to an interface block
   */
  def transformAsObject(tree: source.Term)(using Context): Block = tree match {
    case v: source.Var =>
      BlockVar(v.definition.asInstanceOf[BlockSymbol])

    case source.BlockLiteral(tparams, vparams, bparams, body, _) =>
      Context.panic(s"Using block literal ${tree} but an object was expected.")

    case source.New(impl, _) =>
      New(transform(impl, None))

    case _ => transformUnbox(tree)
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
          // if this block argument expects to be called using PureApp or ImpureApp, make sure it is
          // by wrapping it in a BlockLit
          val targs = tparams.map(core.ValueType.Var.apply)
          val vparams = vparamtps.map { t => core.ValueParam(TmpValue("valueParam"), transform(t))}
          val vargs = vparams.map { case core.ValueParam(id, tpe) => Expr.ValueVar(id, tpe) }

          // [[ f ]] = { (x) => f(x) }
          def etaExpandPure(b: ExternFunction): BlockLit = {
            assert(bparamtps.isEmpty)
            assert(effects.isEmpty)
            assert(cparams.isEmpty)
            BlockLit(tparams, Nil, vparams, Nil,
              Stmt.Return(PureApp(BlockVar(b), targs, vargs)))
          }

          // [[ f ]] = { [A](x) => make f[A](x) }
          def etaExpandConstructor(b: Constructor): BlockLit = {
            assert(bparamtps.isEmpty)
            assert(effects.isEmpty)
            assert(cparams.isEmpty)
            BlockLit(tparams, Nil, vparams, Nil,
              Stmt.Return(Make(core.ValueType.Data(b.tpe, targs), b, targs, vargs)))
          }

          // [[ f ]] = { (x){g} => let r = f(x){g}; return r }
          def etaExpandDirect(f: ExternFunction): BlockLit = {
            assert(effects.isEmpty)
            val bparams = bparamtps.map { t => val id = TmpBlock("etaParam"); core.BlockParam(id, transform(t), Set(id)) }
            val bargs = bparams.map {
              case core.BlockParam(id, tpe, capt) => Block.BlockVar(id, tpe, capt)
            }
            val result = TmpValue("etaBinding")
            val callee = BlockVar(f)
            BlockLit(tparams, bparams.map(_.id), vparams, bparams,
              core.ImpureApp(result, callee, targs, vargs, bargs,
                Stmt.Return(Expr.ValueVar(result, transform(restpe)))))
          }

          sym match {
            case _: ValueSymbol => transformUnbox(tree)
            case cns: Constructor => etaExpandConstructor(cns)
            case f: ExternFunction if callingConvention(f) == CallingConvention.Pure => etaExpandPure(f)
            case f: ExternFunction if callingConvention(f) == CallingConvention.Direct => etaExpandDirect(f)
            // does not require change of calling convention, so no eta expansion
            case sym: BlockSymbol => BlockVar(sym)
          }
        case t: BlockType.InterfaceType =>
          Context.abort(s"Expected a function but got an object of type ${t}")
      }

    case source.BlockLiteral(tps, vps, bps, body, _) =>
      val tparams = tps.map(t => t.symbol)
      val cparams = bps.map { b => b.symbol.capture }
      BlockLit(tparams, cparams, vps map transform, bps map transform, transform(body))

    case s @ source.New(impl, _) =>
      Context.abort(s"Expected a function but got an object instantiation: ${s}")

    case _ => transformUnbox(tree)
  }

  def transformAsExpr(tree: source.Term)(using Context): Expr = coercing(tree) {
    case v: source.Var => v.definition match {
      case sym: RefBinder =>
        val stateType = Context.blockTypeOf(sym)
        val tpe = TState.extractType(stateType)
        val stateId = Id("s")
        // emits `let s = !ref; return s`
        Context.bind(Get(stateId, transform(tpe), sym, transform(Context.captureOf(sym)), Return(core.ValueVar(stateId, transform(tpe)))))
      case sym: ValueSymbol => ValueVar(sym)
      case sym: BlockSymbol => transformBox(tree)
    }

    case source.Literal(value, tpe, _) =>
      Literal(value, transform(tpe))

    // [[ sc.field ]] = val x = sc match { tag: { (_, _, x, _) => return x } }; ...
    case s @ source.Select(receiver, selector, _) =>
      val field: Field = s.definition

      val constructor = field.constructor
      val dataType: symbols.TypeConstructor = constructor.tpe
      val universals: List[symbols.TypeParam] = dataType.tparams

      // allTypeParams = universals ++ existentials
      val allTypeParams: List[symbols.TypeParam] = constructor.tparams

      assert(allTypeParams.length == universals.length, "Existentials on record selection not supported, yet.")

      val scrutineeTypeArgs = Context.inferredTypeOf(receiver) match {
        case effekt.symbols.ValueType.ValueTypeApp(constructor, args) => args
        case _ => Context.panic("Should not happen: selection from non ValueTypeApp")
      }

      val substitution = Substitutions((universals zip scrutineeTypeArgs).toMap, Map.empty)

      val selected = Id("x")
      val tpe = transform(Context.inferredTypeOf(s))
      val params = constructor.fields.map {
        case f: Field =>
          val tpe = transform(substitution.substitute(f.returnType))
          core.ValueParam(if f == field then selected else Id("_"), tpe)
      }
      Context.bind(Stmt.Match(transformAsExpr(receiver), tpe,
        List((constructor, BlockLit(Nil, Nil, params, Nil, Stmt.Return(Expr.ValueVar(selected, tpe))))), None))

    case source.Box(capt, block, _) =>
      transformBox(block)

    case source.New(impl, _) =>
      transformBox(tree)

    case source.Unbox(b, _) =>
      transformBox(tree)

    case source.BlockLiteral(tps, vps, bps, body, _) =>
      transformBox(tree)

    case source.If(List(MatchGuard.BooleanGuard(cond, _)), thn, els, _) =>
      val c = transformAsExpr(cond)
      Context.bind(If(c, transform(thn), transform(els)))

    case source.If(guards, thn, els, _) =>
      val thnClause = preprocess("thn", Nil, guards, transform(thn))
      val elsClause = preprocess("els", Nil, Nil, transform(els))
      val tpe = transform(Context.inferredTypeOf(tree))
      Context.bind(PatternMatchingCompiler.compile(List(thnClause, elsClause), tpe))

    //    case i @ source.If(guards, thn, els) =>
    //      val compiled = collectClauses(i)
    //        .map(PatternMatchingCompiler.compile)
    //        .getOrElse(Context.panic("Should not happen"))
    //      Context.bind(compiled)

    // [[ while(cond) { body } ]] =
    //   def loop$13() = if ([[cond]]) { [[ body ]]; loop$13() } else { return () }
    //   loop$13()
    case source.While(guards, body, default, _) =>
      val loopName = TmpBlock("while")
      val loopType = core.BlockType.Function(Nil, Nil, Nil, Nil, core.Type.TUnit)

      // TODO double check: probably we are forgetting the capture of the guards!
      val loopCapt = transform(Context.inferredCapture(body))
      val loopCall = Stmt.App(core.BlockVar(loopName, loopType, loopCapt), Nil, Nil, Nil)

      val thenBranch = Stmt.Val(TmpValue("while_thn"), transform(body), loopCall)
      val elseBranch = default.map(transform).getOrElse(Return(Literal((), core.Type.TUnit)))

      val loopBody = guards match {
        case List(MatchGuard.BooleanGuard(cond, _)) =>
          insertBindings { core.If(transformAsExpr(cond), thenBranch, elseBranch) }
        case _ =>
          insertBindings {
            val thenClause = preprocess("guard_thn", Nil, guards, thenBranch)
            val elseClause = preprocess("guard_els", Nil, Nil, elseBranch)
            PatternMatchingCompiler.compile(List(thenClause, elseClause), thenBranch.tpe)
          }
      }

      Context.bind(loopName, Block.BlockLit(Nil, Nil, Nil, Nil, loopBody))

      Context.bind(loopCall)

    // Empty match (matching on Nothing)
    case source.Match(List(sc), Nil, None, _) =>
      val scrutinee: ValueVar = Context.bind(transformAsExpr(sc))
      val tpe = transform(Context.inferredTypeOf(tree))
      Context.bind(core.Match(scrutinee, tpe, Nil, None))

    case source.Match(scs, cs, default, _) =>
      // (1) Bind scrutinee and all clauses so we do not have to deal with sharing on demand.
      val scrutinees: List[ValueVar] = scs.map{ sc => Context.bind(transformAsExpr(sc)) }
      val clauses = cs.zipWithIndex.map((c, i) => preprocess(s"k${i}", scrutinees, c))
      val defaultClause = default.map(stmt => preprocess("k_els", Nil, Nil, transform(stmt))).toList
      val tpe = transform(Context.inferredTypeOf(tree))
      val compiledMatch = PatternMatchingCompiler.compile(clauses ++ defaultClause, tpe)
      Context.bind(compiledMatch)

    case source.TryHandle(prog, handlers, _) =>

      val answerType = transform(Context.inferredTypeOf(prog))

      // create a fresh prompt, a variable referring to it, and a parameter binding it
      val promptId   = Id("p")
      val promptCapt = Id("pCapt")
      val promptTpe = Type.TPrompt(answerType)
      val promptVar: core.BlockVar = core.BlockVar(promptId, Type.TPrompt(answerType), Set(promptCapt))
      val promptParam: core.BlockParam = core.BlockParam(promptId, promptTpe, Set(promptCapt))

      val transformedHandlers = handlers.map {
        case h @ source.Handler(cap, impl, _) =>
          val id = h.capability.get.symbol
          Binding.Def(id, New(transform(impl, Some(promptVar))))
      }

      val body: BlockLit = BlockLit(Nil, List(promptCapt), Nil, List(promptParam),
        Binding(transformedHandlers, transform(prog)))

      Context.bind(Reset(body))

    case r @ source.Region(name, body, _) =>
      val region = r.symbol
      val tpe = Context.blockTypeOf(region)
      val cap: core.BlockParam = core.BlockParam(region, transform(tpe), Set(region.capture))
      Context.bind(Region(BlockLit(Nil, List(region.capture), Nil, List(cap), transform(body))))

    case source.Hole(id, stmts, span) =>
      Context.bind(core.Hole(transform(Context.inferredTypeOf(tree)), span))

    case a @ source.Assign(id, expr, _) =>
      val sym = a.definition
      // emits `ref := value; return ()`
      Context.bind(Put(sym, transform(Context.captureOf(sym)), transformAsExpr(expr), Return(Literal((), core.Type.TUnit))))
      Literal((), core.Type.TUnit)

    // methods are dynamically dispatched, so we have to assume they are `control`, hence no PureApp.
    case c @ source.MethodCall(receiver, id, targs, vargs, bargs, _) =>
      val rec = transformAsObject(receiver)
      val typeArgs = Context.typeArguments(c).map(transform)
      val valueArgs = vargs.map { a => transformAsExpr(a.value) }
      val blockArgs = bargs.map(transformAsBlock)

      // TODO if we always just use .capt, then why annotate it?
      // val captArgs = blockArgs.map(_.capt) //bargs.map(b => transform(Context.inferredCapture(b)))

      val receiverType = Context.inferredBlockTypeOf(receiver)
      val operation = c.definition.asOperation
      val opType = transform(operationAtCallsite(receiverType, operation))

      // Do not pass type arguments for the type constructor of the receiver.
      val remainingTypeArgs = typeArgs.drop(operation.interface.tparams.size)

      Context.bind(Invoke(rec, operation, opType, remainingTypeArgs, valueArgs, blockArgs))

    case c @ source.Call(source.ExprTarget(source.Unbox(expr, _)), targs, vargs, bargs, _) =>

      val (funTpe, capture) = Context.inferredTypeOf(expr) match {
        case BoxedType(s: FunctionType, c: CaptureSet) => (s, c)
        case _ => Context.panic("Should be a boxed function type with a known capture set.")
      }
      val e = transformAsExpr(expr)
      val typeArgs = Context.typeArguments(c).map(transform)
      val valueArgs = vargs.map { a => transformAsExpr(a.value) }
      val blockArgs = bargs.map(transformAsBlock)
      // val captArgs = blockArgs.map(b => b.capt) //transform(Context.inferredCapture(b)))

      Context.bind(App(Unbox(e), typeArgs, valueArgs, blockArgs))

    case c @ source.Call(fun: source.IdTarget, _, vargs, bargs, _) =>
      // assumption: typer removed all ambiguous references, so there is exactly one
      makeFunctionCall(c, fun.definition, vargs, bargs)

    case c @ source.Call(s: source.ExprTarget, targs, vargs, bargs, _) =>
      Context.panic("Should not happen. Unbox should have been inferred.")

    case source.Do(id, targs, vargs, bargs, _) =>
      Context.panic("Should have been translated away (to explicit selection `@CAP.op()`) by capability passing.")
  }

  /**
   * Aims to flatten sequenced ifs into a single match
   */
  def collectClauses(term: source.Term)(using Context): Option[List[Clause]] = term match {
    case source.If(guards, thn, els, _) =>
      val thenClause = preprocess("thn", Nil, guards, transform(thn))
      val elseClauses = collectClauses(els) match {
        case Some(clauses) => clauses
        case None => List(preprocess("els", Nil, Nil, transform(els)))
      }
      Some(thenClause :: elseClauses)
    case _ => None
  }
  def collectClauses(stmt: source.Stmt)(using Context): Option[List[Clause]] = stmt match {
    case source.Stmt.Return(d, span) => collectClauses(d)
    case _ => None
  }

  /**
   * Establishes a canonical ordering of methods by using
   * the order in which they are declared in the signature (like with handlers)
   */
  def transform(impl: source.Implementation, prompt: Option[core.BlockVar])(using Context): core.Implementation = {
    val members = impl.clauses
    val clauses = members.map { cl => (cl.definition, cl) }.toMap
    val sig = impl.definition

    val coreType = transform(Context.inferredBlockTypeOf(impl)) match {
      case i: core.BlockType.Interface => i
      case _ => Context.panic("Should be an interface type.")
    }

    Implementation(coreType, sig.operations.map(clauses.apply).map {
      case op @ source.OpClause(id, tparams, vparams, bparams, ret, body, resume, _) =>
        val vps = vparams.map(transform)
        val tps = tparams.map { p => p.symbol }

        prompt match {
          case Some(prompt) =>
            val resumeSymbol = resume.symbol.asInstanceOf[BlockSymbol]
            Context.blockTypeOf(resumeSymbol) match {

              // uni-directional
              // ---------------
              // [[ def op(x) = ... resume123(...) ... ]]
              //   =
              // def op(x) = shift(p) { k =>
              //   def resume123(y) = resume(k) { return y };
              //   ... resume123(...) ...
              // }
              //
              // Function resume123 will hopefully be inlined by the inliner / optimizer
              case BlockType.FunctionType(_, _, List(result), _, answer, _) =>

                val resultTpe = transform(result)
                val answerTpe = transform(answer)

                // (1) bind the continuation (k)itself

                // THIS IS NOT CORRECT: in the source language the capture of resume is transparent
                // This suggests we need to change the representation of Shift and its typing...
                val resumeCapture = transform(Context.captureOf(resumeSymbol))
                val resumeId = Id("k")
                val resumeTpe = core.Type.TResume(resultTpe, answerTpe)
                val resumeParam = core.BlockParam(resumeId, resumeTpe, resumeCapture)
                val resumeVar: core.BlockVar = core.BlockVar(resumeId, resumeTpe, resumeCapture)

                // (2) eta-expand and bind continuation as a function
                val resumeArgId = Id("a")
                val resumeArgParam: core.ValueParam = core.ValueParam(resumeArgId, resultTpe)
                val resumeFun: core.BlockLit = core.BlockLit(Nil, Nil, List(resumeArgParam), Nil,
                  core.Stmt.Resume(resumeVar, core.Stmt.Return(core.ValueVar(resumeArgId, resultTpe))))

                core.Operation(op.definition, tps, Nil, vps, Nil,
                  core.Shift(prompt, resumeParam,
                    core.Def(resumeSymbol, resumeFun,
                      transform(body))))

              // bi-directional
              // --------------
              // [[ def op(x) = ... resume123 { {f} => ... } ... ]]
              //   =
              // def op(x) {g} = shift(p) { k =>
              //   def resume123 {h} = resume(k) { h {g} }
              //   ... resume123 { {f} => ... } ...
              // }
              //
              // Again the typing is wrong in core now. `g` will be tracked, but resume should subtract it.
              case BlockType.FunctionType(_, _, _, List(argTpe @ BlockType.FunctionType(_, _, _, _, result, _)), answer, _) =>
                // IDEA (still not sound, but might be backwards compatible): have
                //    resume(k)) at {g} { h {g} }
                // where at {g} annotates the captures that should be _subtracted_ by the resume.
                val resultTpe = transform(result)
                val answerTpe = transform(answer)

                val resumeArgTpe @ core.BlockType.Function(_, cps, _, bps, _) = transform(argTpe) : @unchecked

                // (0) compute the block parameters from the type of the continuation (since this is what typer annotates)
                val bparams: List[core.BlockParam] = (cps zip bps) map { case (capt, tpe) =>
                  core.BlockParam(Id("g"), tpe, Set(capt))
                }
                val bvars = bparams.map { b => core.BlockVar(b.id, b.tpe, b.capt) }

                // (1) bind the continuation (k) itself
                val resumeCapture = transform(Context.captureOf(resumeSymbol))
                val resumeId = Id("k")
                val resumeTpe = core.Type.TResume(resultTpe, answerTpe)
                val resumeParam: core.BlockParam = core.BlockParam(resumeId, resumeTpe, resumeCapture)
                val resumeVar: core.BlockVar = core.BlockVar(resumeId, resumeTpe, resumeCapture)

                // (2) eta-expand and bind continuation as a function
                val resumeArgId = Id("h")
                val resumeArgCapture = Id("h")
                val resumeArgParam: core.BlockParam = core.BlockParam(resumeArgId, resumeArgTpe, Set(resumeArgCapture))
                val resumeArgVar: core.BlockVar = core.BlockVar(resumeArgId, resumeArgTpe, Set(resumeArgCapture))
                val resumeFun: core.BlockLit = core.BlockLit(Nil, List(resumeArgCapture), Nil, List(resumeArgParam),
                  core.Stmt.Resume(resumeVar, core.Stmt.App(resumeArgVar, Nil, Nil, bvars)))

                core.Operation(op.definition, tps, cps, vps, bparams,
                  core.Shift(prompt, resumeParam,
                    core.Stmt.Def(resumeSymbol, resumeFun,
                      transform(body))))

              case _ => ???
            }

          case None =>
            val bps = bparams map transform
            val cps = bparams map { b => b.symbol.capture }
            core.Operation(op.definition, tps, cps, vps, bps, transform(body))
        }
    })
  }

  def preprocess(label: String, scs: List[ValueVar], clause: source.MatchClause)(using Context): Clause = {
    val patterns = (clause.pattern, scs) match {
      case (source.MultiPattern(ps, _), scs) => scs.zip(ps)
      case (pattern, List(sc)) => List((sc, clause.pattern))
      case (_, _) => Context.abort("Malformed multi-match")
    }
    preprocess(label, patterns, clause.guards, transform(clause.body))
  }

  def preprocess(label: String, patterns: List[(ValueVar, source.MatchPattern)], guards: List[source.MatchGuard], body: core.Stmt)(using Context): Clause = {
    import PatternMatchingCompiler.*

    def boundInPattern(p: source.MatchPattern): List[core.ValueParam] = p match {
      case p @ source.AnyPattern(id, _) => List(ValueParam(p.symbol))
      case source.TagPattern(id, patterns, _) => patterns.flatMap(boundInPattern)
      case _: source.LiteralPattern | _: source.IgnorePattern => Nil
      case source.MultiPattern(patterns, _) => patterns.flatMap(boundInPattern)
    }
    def boundInGuard(g: source.MatchGuard): List[core.ValueParam] = g match {
      case MatchGuard.BooleanGuard(condition, _) => Nil
      case MatchGuard.PatternGuard(scrutinee, pattern, _) => boundInPattern(pattern)
    }
    def boundTypesInPattern(p: source.MatchPattern): List[Id] = p match {
      case source.AnyPattern(id, _) => List()
      case p @ source.TagPattern(id, patterns, _) => Context.annotation(Annotations.TypeParameters, p) ++ patterns.flatMap(boundTypesInPattern)
      case _: source.LiteralPattern | _: source.IgnorePattern => Nil
      case source.MultiPattern(patterns, _) => patterns.flatMap(boundTypesInPattern)
    }
    def boundTypesInGuard(g: source.MatchGuard): List[Id] = g match {
      case MatchGuard.BooleanGuard(condition, _) => Nil
      case MatchGuard.PatternGuard(scrutinee, pattern, _) => boundTypesInPattern(pattern)
    }
    def equalsFor(tpe: symbols.ValueType): (Expr, Expr) => Expr =
      tpe match {
        case tpe if tpe == TUnit => return (lhs, rhs) => Literal(true, Type.TBoolean)
        case _ =>
      }

      val prelude = Context.module.findDependency(QualifiedName(Nil, "effekt")).getOrElse {
        Context.panic(pp"${Context.module.name.name}: Cannot find 'effekt' in prelude, which is necessary to compile pattern matching.")
      }
      prelude.exports.terms("infixEq") collect {
        case sym: Callable => (sym, sym.toType)
      } collectFirst {
        // specialized version
        case (sym, FunctionType(Nil, Nil, List(`tpe`, `tpe`), Nil, builtins.TBoolean, _)) =>
          (lhs: Expr, rhs: Expr) => core.PureApp(BlockVar(sym), Nil, List(lhs, rhs))
        // generic version
        case (sym, FunctionType(List(tparam), Nil, List(ValueTypeRef(t1), ValueTypeRef(t2)), Nil, builtins.TBoolean, _))
            if t1 == tparam && t2 == tparam =>
          (lhs: Expr, rhs: Expr) => core.PureApp(BlockVar(sym), List(transform(tpe)), List(lhs, rhs))
      } getOrElse { Context.panic(pp"Cannot find == for type ${tpe} in prelude!") }

    // create joinpoint
    val tparams = patterns.flatMap { case (sc, p) => boundTypesInPattern(p) } ++ guards.flatMap(boundTypesInGuard)
    val params = patterns.flatMap { case (sc, p) => boundInPattern(p) } ++ guards.flatMap(boundInGuard)
    val joinpoint = Context.bind(TmpBlock(label), BlockLit(tparams, Nil, params, Nil, body))

    def transformPattern(p: source.MatchPattern): Pattern = p match {
      case source.AnyPattern(id, _) =>
        Pattern.Any(id.symbol)
      case p @ source.TagPattern(id, patterns, _) =>
        val tparams = Context.annotation(Annotations.TypeParameters, p)

        val allVariants: List[Id] = id.symbol match {
          case c: symbols.Constructor => c.tpe match {
            case TypeConstructor.DataType(name, tparams, constructors, decl) => constructors
            case TypeConstructor.Record(name, tparams, constructor, decl) => List(constructor)
            case _ => INTERNAL_ERROR("Constructors can only be part of a data types or records")
          }
          case _ => INTERNAL_ERROR("Can only use constructors as tags.")
        }

        Pattern.Tag(id.symbol, tparams, allVariants, patterns.map { p => (transformPattern(p), transform(Context.inferredTypeOf(p))) })
      case source.IgnorePattern(_) =>
        Pattern.Ignore()
      case source.LiteralPattern(source.Literal(value, tpe, _), _) =>
        Pattern.Literal(Literal(value, transform(tpe)), equalsFor(tpe))
      case source.MultiPattern(patterns, _) =>
        Context.panic("Multi-pattern should have been split on toplevel / nested MultiPattern")
    }

    def transformGuard(p: source.MatchGuard): List[Condition] =
      val (cond, bindings) = Context.withBindings {
        p match {
          case MatchGuard.BooleanGuard(condition, _) =>
            Condition.Predicate(transformAsExpr(condition))
          case MatchGuard.PatternGuard(scrutinee, pattern, _) =>
            val x = Context.bind(transformAsExpr(scrutinee))
            Condition.Patterns(Map(x -> transformPattern(pattern)))
        }
      }
      bindings.map {
        case Binding.Val(name, binding) => Condition.Val(name, binding)
        case Binding.Let(name, binding) => Condition.Let(name, binding)
        case Binding.ImpureApp(name, callee, targs, vargs, bargs) => Condition.ImpureApp(name, callee, targs, vargs, bargs)
        case Binding.Def(name, binding) => Context.panic("Should not happen")
      } :+ cond

    val transformedPatterns = patterns.map { case (sc, p) => sc -> transformPattern(p) }.toMap
    val transformedGuards   = guards.flatMap(transformGuard)
    val conditions = if transformedPatterns.isEmpty then transformedGuards else Condition.Patterns(transformedPatterns) :: guards.flatMap(transformGuard)

    Clause(conditions, joinpoint, tparams.map(x => core.ValueType.Var(x)), params.map(p => core.ValueVar(p.id, p.tpe)))
  }

  /**
   * Computes the block type the selected symbol.
   *
   * For instance, receiver can be `State[Int]`, interface could be the symbol of `State` and member could be `get`.
   * If `member` is an operation, the type arguments of the receiver are substituted for the leading type parameters,
   * while the remaining type parameters are kept.
   */
  def operationAtCallsite(receiver: symbols.BlockType, member: symbols.Operation)(using Context): BlockType = receiver.asInterfaceType match {
    case InterfaceType(i: Interface, targs) => member match {
      // For operations, we substitute the first type parameters by concrete type args.
      case Operation(name, tparams, vparams, bparams, resultType, effects, _, _) =>
        val substitution = Substitutions((tparams zip targs).toMap, Map.empty)
        val remainingTypeParams = tparams.drop(targs.size)
        // TODO this is exactly like in [[Callable.toType]] -- TODO repeated here:
        // TODO currently the return type cannot refer to the annotated effects, so we can make up capabilities
        //   in the future namer needs to annotate the function with the capture parameters it introduced.
        val cparams = bparams.map(b => b.capture) ++ CanonicalOrdering(effects.toList).map { tpe => symbols.CaptureParam(tpe.name) }
        val vparamTpes = vparams.map(t => substitution.substitute(t.tpe.getOrElse {
          INTERNAL_ERROR("Operation value parameters should have an annotated type.")
        }))
        val bparamTpes = bparams.map(b => substitution.substitute(b.tpe.getOrElse {
          INTERNAL_ERROR("Operation block parameters should have an annotated type.")
        }))

        FunctionType(remainingTypeParams, cparams, vparamTpes, bparamTpes, substitution.substitute(resultType), substitution.substitute(effects))
    }

    case InterfaceType(i: ExternInterface, targs) =>
      Context.panic("Cannot select from an extern interface")

    case InterfaceType(i: ErrorBlockType, targs) =>
      INTERNAL_ERROR("Error block type coming from Namer should not get to Transformer!")
  }

  def operationAtDeclaration(tparamsInterface: List[Id], op: symbols.Operation)(using Context): core.BlockType = op match {
    case symbols.Operation(name, tps, vps, bps, resultType, effects, interface, _) =>
      // like in asSeenFrom we need to make up cparams, they cannot occur free in the result type
      val capabilities = CanonicalOrdering(effects.toList)
      val tparams = tps.drop(tparamsInterface.size)
      val bparamsBlocks = bps.map(b => transform(b.tpe.getOrElse {
        INTERNAL_ERROR("Interface declarations should have annotated types.")
      }))
      val bparamsCapabilities = capabilities.map(transform)
      val bparams = bparamsBlocks ++ bparamsCapabilities
      val vparams = vps.map(p => transform(p.tpe.get))
      val cparams = bps.map(_.capture) ++ capabilities.map { tpe => symbols.CaptureParam(tpe.name) }

      // here we reconstruct the block type
      core.BlockType.Function(tparams, cparams, vparams, bparams, transform(resultType))
    }

  def makeFunctionCall(call: source.CallLike, sym: TermSymbol, vargs: List[source.ValueArg], bargs: List[source.Term])(using Context): Expr = {
    // the type arguments, inferred by typer
    val targs = Context.typeArguments(call).map(transform)
    // val cargs = bargs.map(b => transform(Context.inferredCapture(b)))

    val vargsT = vargs.map { a => transformAsExpr(a.value) }
    val bargsT = bargs.map(transformAsBlock)

    sym match {
      case f: Callable if callingConvention(f) == CallingConvention.Pure =>
        PureApp(BlockVar(f), targs, vargsT)
      case f: Callable if callingConvention(f) == CallingConvention.Direct =>
        Context.bind(BlockVar(f), targs, vargsT, bargsT)
      case r: Constructor =>
        if (bargs.nonEmpty) Context.abort("Constructors cannot take block arguments.")
        val universals = targs.take(r.tpe.tparams.length)
        val existentials = targs.drop(r.tpe.tparams.length)
        Make(core.ValueType.Data(r.tpe, universals), r, existentials, vargsT)
      case f: Operation =>
        Context.panic("Should have been translated to a method call!")
      case f: Field =>
        Context.panic("Should have been translated to a select!")
      case f: BlockSymbol =>
        Context.bind(App(BlockVar(f), targs, vargsT, bargsT))
      case f: ValueSymbol =>
        Context.bind(App(Unbox(ValueVar(f)), targs, vargsT, bargsT))
    }
  }

  def transform(p: source.BlockParam)(using Context): core.BlockParam = BlockParam(p.symbol)

  def transform(p: source.ValueParam)(using Context): core.ValueParam = ValueParam(p.symbol)

  def insertBindings(stmt: => Stmt)(using Context): Stmt = {
    val (body, bindings) = Context.withBindings { stmt }
    Binding(bindings, body)
  }

  // Translation on Types
  // --------------------
  def transform(tpe: ValueType)(using Context): core.ValueType = tpe match {
    case ValueType.BoxedType(tpe, capture) => core.ValueType.Boxed(transform(tpe), transform(capture))
    case ValueType.ValueTypeRef(tvar)      => core.ValueType.Var(tvar)
    case ValueType.ValueTypeApp(tc, args)  => core.ValueType.Data(tc, args.map(transform))
  }

  def transform(tpe: BlockType)(using Context): core.BlockType = tpe match {
    case BlockType.FunctionType(tparams, cparams, vparams, bparams, result, effects) =>

      val capabilityTypes = CanonicalOrdering(effects.toList).map(transform)
      val allBlockParams = bparams.map(transform) ++ capabilityTypes

      assert(cparams.size == allBlockParams.size,
        s"""Internal error: number of block parameters does not match number of capture parameters.
           |
           |  Blockparams: ${bparams}
           |  Effects: ${capabilityTypes}
           |  Captures: ${cparams}
           |""".stripMargin)

      core.BlockType.Function(tparams, cparams, vparams.map(transform), allBlockParams, transform(result))
    case BlockType.InterfaceType(tc, args) => core.BlockType.Interface(tc, args.map(transform))
  }

  def transform(capt: Captures)(using Context): core.Captures = capt match {
    case CaptUnificationVar(role) => Context.panic(pp"$capt should be a concrete capture set in this phase.")
    case CaptureSet(captures) => captures.map(x => x: Symbol) // that is really a noop...
  }

  // Helpers
  // -------

  // Helpers to constructed typed trees
  def ValueParam(id: ValueSymbol)(using Context): core.ValueParam =
    core.ValueParam(id, transform(Context.valueTypeOf(id)))

  def BlockParam(id: TrackedParam)(using Context): core.BlockParam =
    core.BlockParam(id, transform(Context.blockTypeOf(id)), Set(id.capture))

  def ValueVar(id: ValueSymbol)(using Context): core.ValueVar =
    core.ValueVar(id, transform(Context.valueTypeOf(id)))

  def BlockVar(id: BlockSymbol)(using Context): core.BlockVar =
    core.BlockVar(id, transform(Context.blockTypeOf(id)), transform(Context.captureOf(id)))

  def asConcreteCaptureSet(c: Captures)(using Context): CaptureSet = c match {
    case c: CaptureSet => c
    case _ => Context.panic("All capture unification variables should have been replaced by now.")
  }

  // we can conservatively approximate to false, in order to disable the optimizations
  def isPureOrIO(t: source.Tree)(using Context): Boolean =
    Context.inferredCaptureOption(t) match {
      case Some(capt) => asConcreteCaptureSet(capt).pureOrIO
      case _         => false
    }

  def isPure(t: source.Tree)(using Context): Boolean = Context.inferredCaptureOption(t) match {
    case Some(capt) => asConcreteCaptureSet(capt).pure
    case _         => false
  }

  def pureOrIO(t: BlockSymbol)(using Context): Boolean = asConcreteCaptureSet(Context.captureOf(t)).pureOrIO

  def coercing[T <: source.Tree](tree: T)(f: T => Stmt)(using Context): Stmt =
    val result = f(tree)
    Context.annotationOption(Annotations.ShouldCoerce, tree) match {
      case Some(Coercion.ToAny(from)) =>
        insertBindings {
          val sc = Context.bind(result)
          core.Stmt.Return(core.Expr.Literal((), core.Type.TUnit))
        }
      case Some(Coercion.FromNothing(to)) =>
        insertBindings {
          val sc = Context.bind(result)
          core.Stmt.Match(sc, transform(to), Nil, None)
        }
      case None => result
    }

  def coercing[T <: source.Tree](tree: T)(f: T => Expr)(using Context): Expr =
    val result = f(tree)
    Context.annotationOption(Annotations.ShouldCoerce, tree) match {
      case Some(Coercion.ToAny(from)) =>
        core.Expr.Literal((), core.Type.TUnit)
      case Some(Coercion.FromNothing(to)) =>
        Context.bind(core.Stmt.Match(result, transform(to), Nil, None))
      case None => result
    }
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
  private[core] def bind(s: Stmt): ValueVar = {

    // create a fresh symbol and assign the type
    val x = TmpValue("r")

    val binding = Binding.Val(x, s)
    bindings += binding

    ValueVar(x, s.tpe)
  }

  private[core] def bind(e: Expr): ValueVar = e match {
    case x: ValueVar => x
    case e =>
      // create a fresh symbol and assign the type
      val x = TmpValue("r")

      val binding = Binding.Let(x, e)
      bindings += binding

      ValueVar(x, e.tpe)
  }

  private[core] def bind(callee: Block.BlockVar, targs: List[core.ValueType], vargs: List[Expr], bargs: List[Block]): ValueVar = {
      // create a fresh symbol and assign the type
      val x = TmpValue("r")
      val binding: Binding.ImpureApp = Binding.ImpureApp(x, callee, targs, vargs, bargs)
      bindings += binding

      ValueVar(x, Type.bindingType(binding))
  }

  private[core] def bind(name: BlockSymbol, b: Block): BlockVar = {
    val binding = Binding.Def(name, b)
    bindings += binding
    BlockVar(name, b.tpe, b.capt)
  }

  private[core] def withBindings[R](block: => R): (R, List[Binding]) = Context in {
    val before = bindings
    val b = ListBuffer.empty[Binding]
    bindings = b
    val result = block
    bindings = before
    (result, b.toList)
  }
}
