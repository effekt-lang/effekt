package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.context.assertions.*
import effekt.source.ExternFlag

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
        case sym: Fun if !sym.isInstanceOf[Operation] && !sym.isInstanceOf[Field] => sym
        case sym: ValBinder => sym
      }
    }.toList

    val transformed = defs.foldRight(Ret(UnitLit()) : Stmt) {
      case (d, r) => transform(d, () => r)
    }

    val optimized = optimize(transformed)

    ModuleDecl(path, imports.map { _.path }, optimized, exports)
  }

  /**
   * the "rest" is a thunk so that traversal of statements takes place in the correct order.
   */
  def transform(d: source.Def, rest: () => Stmt)(using Context): Stmt = d match {
    case f @ source.FunDef(id, _, vps, bps, _, body) =>
      val sym = f.symbol
      val ps = (vps map transform) ++ (bps map transform)
      Def(sym, Context.blockTypeOf(sym), BlockLit(ps, transform(body)), rest())

    case d @ source.DataDef(id, _, ctors) =>
      Data(d.symbol, ctors.map { c => c.symbol }, rest())

    case d @ source.RecordDef(id, _, _) =>
      val rec = d.symbol
      core.Record(rec, rec.fields, rest())

    case v @ source.ValDef(id, _, binding) if pureOrIO(binding) =>
      Let(v.symbol, Run(transform(binding)), rest())

    case v @ source.ValDef(id, _, binding) =>
      Val(v.symbol, transform(binding), rest())

    case v @ source.DefDef(id, annot, binding) =>
      val sym = v.symbol
      insertBindings { Def(sym, Context.blockTypeOf(sym), transformAsBlock(binding), rest()) }

    case v @ source.VarDef(id, _, reg, binding) =>
      val sym = v.symbol
      val tpe = getStateType(sym)
      insertBindings {
        val b = Context.bind(tpe, transform(binding))
        State(sym, b, sym.region, rest())
      }

    case source.ExternType(id, tparams) =>
      rest()

    case source.TypeDef(id, tparams, tpe) =>
      rest()

    case source.EffectDef(id, tparams, effs) =>
      rest()

    case f @ source.ExternFun(pure, id, tps, vps, bps, ret, body) =>
      val sym = f.symbol
      Def(f.symbol, Context.functionTypeOf(sym), Extern((vps map transform) ++ (bps map transform), body), rest())

    case e @ source.ExternInclude(path) =>
      Include(e.contents, rest())

    case e: source.ExternEffect =>
      rest()

    case d @ source.InterfaceDef(id, tparams, ops, isEffect) =>
      core.Record(d.symbol, ops.map { e => e.symbol }, rest())
  }

  def transform(tree: source.Stmt)(using Context): Stmt = tree match {
    case source.DefStmt(d, rest) =>
      transform(d, () => transform(rest))

    // { e; stmt } --> { val _ = e; stmt }
    case source.ExprStmt(e, rest) if pureOrIO(e) =>
      val (expr, bs) = Context.withBindings { transformAsExpr(e) }
      val let = Let(freshWildcardFor(e), expr, transform(rest))
      if (bs.isEmpty) { let }
      else { Context.reifyBindings(let, bs) }

    case source.ExprStmt(e, rest) =>
      Val(freshWildcardFor(e), insertBindings { Ret(transformAsPure(e)) }, transform(rest))

    case source.Return(e) =>
      insertBindings { Ret(transformAsPure(e)) }

    case source.BlockStmt(b) =>
      transform(b)
  }

  def transformLit[T](tree: source.Literal[T])(using Context): Literal[T] = tree match {
    case source.UnitLit()         => UnitLit()
    case source.IntLit(value)     => IntLit(value)
    case source.BooleanLit(value) => BooleanLit(value)
    case source.DoubleLit(value)  => DoubleLit(value)
    case source.StringLit(value)  => StringLit(value)
  }

  def transformUnbox(tree: source.Term)(implicit C: Context): Block =
    Unbox(transformAsPure(tree))

  def transformBox(tree: source.Term)(implicit C: Context): Pure =
    Box(transformAsBlock(tree))

  def transformAsBlock(tree: source.Term)(using Context): Block = tree match {
    case v: source.Var => v.definition match {
      case sym: ValueSymbol => transformUnbox(tree)
      case sym: BlockSymbol => BlockVar(sym)
    }
    case s @ source.Select(receiver, selector) =>
      Member(transformAsBlock(receiver), s.definition)

    case s @ source.New(h @ source.Implementation(tpe, members)) =>
      // we need to establish a canonical ordering of methods
      // we use the order in which they are declared in the signature (like with handlers)
      val clauses = members.map { cl => (cl.definition, cl) }.toMap
      val sig = h.definition

      New(Handler(sig, sig.ops.map(clauses.apply).map {
        case op @ source.OpClause(id, tparams, vparams, body, resume) =>
          val vps = vparams map transform
          // currently the don't take block params
          val opBlock: BlockLit = BlockLit(vps, transform(body))
          (op.definition, opBlock)
      }))

    case source.Unbox(b) =>
      Unbox(transformAsPure(b))

    case source.BlockLiteral(tps, vps, bps, body) =>
      BlockLit((vps map transform) ++ (bps map transform), transform(body))

    case _ =>
      transformUnbox(tree)
  }
  def getStateType(v: VarBinder)(implicit C: Context): ValueType = C.blockTypeOf(v) match {
    case BlockTypeApp(TState.interface, List(tpe)) => tpe
    case _ => C.panic("Not a mutable variable")
  }

  def transformAsPure(tree: source.Term)(using Context): Pure = transformAsExpr(tree) match {
    case p: Pure => p
    case e: Expr => Context.bind(Context.inferredTypeOf(tree), e)
  }

  def transformAsExpr(tree: source.Term)(using Context): Expr = tree match {
    case v: source.Var => v.definition match {
      case sym: VarBinder => DirectApp(Member(BlockVar(sym), TState.get), Nil, Nil)
      case sym: ValueSymbol => ValueVar(sym)
      case sym: BlockSymbol => transformBox(tree)
    }

    case l: source.Literal[t] => transformLit(l)

    case source.Select(receiver, selector) =>
      Select(transformAsPure(receiver), selector.symbol)

    case l @ source.Box(capt, block) =>
      Box(transformAsBlock(block))

    case source.New(impl) => transformBox(tree)

    case source.Unbox(b) => transformBox(tree)

    case source.BlockLiteral(tps, vps, bps, body) => transformBox(tree)

    case source.If(cond, thn, els) =>
      val c = transformAsPure(cond)
      val exprTpe = Context.inferredTypeOf(tree)
      Context.bind(exprTpe, If(c, transform(thn), transform(els)))


    case source.While(cond, body) =>
      val exprTpe = Context.inferredTypeOf(tree)
      Context.bind(exprTpe, While(insertBindings { Ret(transformAsPure(cond)) }, transform(body)))

    case source.Match(sc, clauses) =>
      val scrutinee = transformAsPure(sc)

      val cs: List[(Pattern, BlockLit)] = clauses.map {
        case cl @ source.MatchClause(pattern, body) =>
          val (p, ps) = transform(pattern)
          (p, BlockLit(ps, transform(body)))
      }
      Context.bind(Context.inferredTypeOf(tree), Match(scrutinee, cs))

    case source.TryHandle(prog, handlers) =>
      val effects = handlers.map(_.definition)
      val caps = handlers.map { h =>
        val cap = h.capability.get.symbol
        core.BlockParam(cap, cap.tpe)
      }
      val body = BlockLit(caps, transform(prog))

      // to obtain a canonical ordering of operation clauses, we use the definition ordering
      val hs = handlers.map {
        case h @ source.Handler(cap, source.Implementation(eff, cls)) =>
          val clauses = cls.map { cl => (cl.definition, cl) }.toMap

          Handler(h.definition, h.definition.ops.map(clauses.apply).map {
            case op @ source.OpClause(id, tps, vps, body, resume) =>
              val ps = vps map transform

              // introduce a block parameter for resume
              val resumeParam = BlockParam(resume.symbol.asInstanceOf[BlockSymbol])

              val opBlock: BlockLit = BlockLit(ps :+ resumeParam, transform(body))
              (op.definition, opBlock)
          })
      }

      Context.bind(Context.inferredTypeOf(tree), Handle(body, hs))

    case r @ source.Region(name, body) =>
      val sym = r.symbol
      val tpe = sym match {
        case b: BlockParam => b.tpe
        case b: SelfParam => b.tpe
        case _ => Context.panic("Continuations cannot be regions")
      }
      val cap = core.BlockParam(sym, tpe)
      Context.bind(Context.inferredTypeOf(tree), Region(BlockLit(List(cap), transform(body))))

    case source.Hole(stmts) =>
      Context.bind(Context.inferredTypeOf(tree), Hole)

    case source.Do(effect, id, targs, vargs) =>
      Context.panic("Should have been translated away (to explicit selection `@CAP.op()`) by capability passing.")

    case a @ source.Assign(id, expr) =>
      val e = transformAsPure(expr)
      val sym = a.definition
      // val state = Context.annotation(Annotations.StateCapability, a.definition)
       DirectApp(Member(BlockVar(sym), TState.put), Nil, List(e))

    // methods are dynamically dispatched, so we have to assume they are `control`, hence no PureApp.
    case c @ source.MethodCall(receiver, id, targs, vargs, bargs) =>
      val rec = transformAsBlock(receiver)
      val typeArgs = Context.typeArguments(c)
      val valueArgs = vargs.map(transformAsPure)
      val blockArgs = bargs.map(transformAsBlock)

      c.definition match {
        case sym if sym == TState.put || sym == TState.get =>
          DirectApp(Member(rec, sym), Nil, valueArgs ++ blockArgs)
        case sym =>
          Context.bind(Context.inferredTypeOf(tree), App(Member(rec, sym), typeArgs, valueArgs ++ blockArgs))
      }

    case c @ source.Call(source.ExprTarget(source.Unbox(expr)), targs, vargs, bargs) =>
      val capture = Context.inferredTypeOf(expr) match {
        case BoxedType(_, c: CaptureSet) => c
        case _ => Context.panic("Should be a boxed function type with a known capture set.")
      }
      val e = transformAsPure(expr)
      val typeArgs = Context.typeArguments(c)
      val valueArgs = vargs.map(transformAsPure)
      val blockArgs = bargs.map(transformAsBlock)

      if (pureOrIO(capture) && bargs.forall { pureOrIO }) {
        Run(App(Unbox(e), typeArgs, valueArgs ++ blockArgs))
      } else {
        Context.bind(Context.inferredTypeOf(tree), App(Unbox(e), typeArgs, valueArgs ++ blockArgs))
      }

    case c @ source.Call(s : source.ExprTarget, targs, vargs, bargs) =>
      Context.panic("Should not happen. Unbox should have been inferred.")

    case c @ source.Call(fun: source.IdTarget, _, vargs, bargs) =>
      // assumption: typer removed all ambiguous references, so there is exactly one
      makeFunctionCall(c, fun.definition, vargs, bargs)
  }

  def makeFunctionCall(call: source.CallLike, sym: TermSymbol, vargs: List[source.Term], bargs: List[source.Term])(using Context): Expr = {
    // the type arguments, inferred by typer
    val targs = Context.typeArguments(call)

    val vargsT = vargs.map(transformAsPure)
    val bargsT = bargs.map(transformAsBlock)
    val as = vargsT ++ bargsT

    sym match {
      case f: BuiltinFunction if f.purity == ExternFlag.Pure =>
        if (bargs.nonEmpty) Context.abort("Pure builtin functions cannot take block arguments.")
        PureApp(BlockVar(f), targs, vargsT)
      case f: BuiltinFunction if f.purity == ExternFlag.IO =>
        DirectApp(BlockVar(f), targs, as)
      case r: Record =>
        if (bargs.nonEmpty) Context.abort("Constructors cannot take block arguments.")
        PureApp(BlockVar(r), targs, vargsT)
      case f: Operation =>
        Context.panic("Should have been translated to a method call!")
      case f: Field =>
        Context.panic("Should have been translated to a select!")
      case f: BlockSymbol if pureOrIO(f) && bargs.forall { pureOrIO } =>
        Run(App(BlockVar(f), targs, as))
      case f: BlockSymbol =>
        Context.bind(Context.inferredTypeOf(call), App(BlockVar(f), targs, as))
      case f: ValueSymbol =>
        Context.bind(Context.inferredTypeOf(call), App(Unbox(ValueVar(f)), targs, as))
    }
  }

  def transform(p: source.BlockParam)(using Context): core.BlockParam = BlockParam(p.symbol)
  def transform(p: source.ValueParam)(using Context): core.ValueParam = ValueParam(p.symbol)

  def transform(tree: source.MatchPattern)(using Context): (Pattern, List[core.ValueParam]) = tree match {
    case source.IgnorePattern()    => (core.IgnorePattern(), Nil)
    case source.LiteralPattern(l)  => (core.LiteralPattern(transformLit(l)), Nil)
    case p @ source.AnyPattern(id) => (core.AnyPattern(), List(ValueParam(p.symbol)))
    case p @ source.TagPattern(id, ps) =>
      val (patterns, params) = ps.map(transform).unzip
      (core.TagPattern(p.definition, patterns), params.flatten)
  }

  def transform(exprs: List[source.Term])(using Context): List[Expr] =
    exprs.map(transformAsExpr)

  def freshWildcardFor(e: source.Tree)(using Context): Wildcard = {
    val x = Wildcard(Context.module)
    Context.inferredTypeOption(e) match {
      case Some(t) => Context.assignType(x, t)
      case _           => Context.abort("Internal Error: Missing type of source expression.")
    }
    x
  }

  def insertBindings(stmt: => Stmt)(using Context): Stmt = {
    val (body, bindings) = Context.withBindings { stmt }
    Context.reifyBindings(body, bindings)
  }

  // Helpers to constructed typed trees
  def ValueParam(id: ValueSymbol)(using Context): core.ValueParam =
    core.ValueParam(id, Context.valueTypeOf(id))

  def BlockParam(id: BlockSymbol)(using Context): core.BlockParam =
    core.BlockParam(id, Context.blockTypeOf(id))

  def Val(id: ValueSymbol, binding: Stmt, body: Stmt)(using Context): core.Val =
    core.Val(id, Context.valueTypeOf(id), binding, body)

  def Let(id: ValueSymbol, binding: Expr, body: Stmt)(using Context): core.Let =
    core.Let(id, Context.valueTypeOf(id), binding, body)

  def optimize(s: Stmt)(using Context): Stmt = {

    // a very small and easy post processing step...
    // reduces run-return pairs
    object eliminateReturnRun extends core.Tree.Rewrite {
      override def expr = {
        case core.Run(core.Ret(p)) => rewrite(p)
      }
    }

    // rewrite (Val (Ret e) s) to (Let e s)
    object directStyleVal extends core.Tree.Rewrite {
      override def stmt = {
        case core.Val(id, tpe, core.Ret(expr), body) =>
          core.Let(id, tpe, rewrite(expr), rewrite(body))
      }
    }
    val opt = eliminateReturnRun.rewrite(s)
    directStyleVal.rewrite(opt)
  }

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
      isIO || isMutableState
  }
}

private[core] enum Binding {
  case Val(name: Tmp, tpe: symbols.ValueType, binding: Stmt)
  case Let(name: Tmp, tpe: symbols.ValueType, binding: Expr)
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
  private[core] def bind(tpe: symbols.ValueType, s: Stmt): Pure = {

    // create a fresh symbol and assign the type
    val x = Tmp(module)
    assignType(x, tpe)

    val binding = Binding.Val(x, tpe, s)
    bindings += binding

    ValueVar(x)
  }

  private[core] def bind(tpe: symbols.ValueType, s: Expr): Pure = {

    // create a fresh symbol and assign the type
    val x = Tmp(module)
    assignType(x, tpe)

    val binding = Binding.Let(x, tpe, s)
    bindings += binding

    ValueVar(x)
  }

  private[core] def withBindings[R](block: => R): (R, ListBuffer[Binding]) = Context in {
    val before = bindings
    val b = ListBuffer.empty[Binding]
    bindings = b
    val result = block
    bindings = before
    (result, b)
  }

  // TODO, when reifying bindings, insert let bindings and use RUN when statement is pure or IO!
  private[core] def reifyBindings(body: Stmt, bindings: ListBuffer[Binding]): Stmt = {
    bindings.foldRight(body) {
      // optimization: remove unnecessary binds
      case (Binding.Val(x, tpe, b), Ret(ValueVar(y))) if x == y => b
      case (Binding.Val(x, tpe, b), body) => Val(x, tpe, b, body)
      case (Binding.Let(x, tpe, b), body) => Let(x, tpe, b, body)
    }
  }
}
