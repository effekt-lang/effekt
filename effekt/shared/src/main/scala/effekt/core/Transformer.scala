package effekt
package core

import scala.collection.mutable.ListBuffer
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.symbols.builtins.*
import effekt.context.assertions.*
import effekt.source.MatchPattern

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

    // The type of the acc needs to be a function!
    val transformed = (defs.foldRight(() => Return(UnitLit()) : Stmt) {
      case (d, r) => () => transform(d, () => r())
    })()

    val optimized = optimize(transformed)

    val externals = Context.gatheredExternals
    val declarations = Context.gatheredDeclarations

    // We use the imports on the symbol (since they include the prelude)
    ModuleDecl(path, mod.imports.map { _.path }, declarations, externals, optimized, exports)
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
      Context.emitDeclaration(Data(d.symbol, ctors.map { c => c.symbol }))
      rest()

    case d @ source.RecordDef(id, _, _) =>
      val rec = d.symbol
      Context.emitDeclaration(core.Record(rec, rec.constructor.fields))
      rest()

    case v @ source.ValDef(id, _, binding) if pureOrIO(binding) =>
      Let(v.symbol, Run(transform(binding), Context.inferredTypeOf(binding)), rest())

    case v @ source.ValDef(id, _, binding) =>
      Val(v.symbol, transform(binding), rest())

    case v @ source.DefDef(id, annot, binding) =>
      val sym = v.symbol
      insertBindings { Def(sym, Context.blockTypeOf(sym), transformAsBlock(binding), rest()) }

    case v @ source.VarDef(id, _, reg, binding) =>
      val sym = v.symbol
      val tpe = TState.extractType(Context.blockTypeOf(sym))
      insertBindings {
        val b = Context.bind(tpe, transform(binding))
        State(sym, b, sym.region, rest())
      }

    case d @ source.InterfaceDef(id, tparams, ops, isEffect) =>
      Context.emitDeclaration(core.Interface(d.symbol, ops.map { e => e.symbol }))
      rest()

    case f @ source.ExternDef(pure, id, tps, vps, bps, ret, body) =>
      val sym = f.symbol
      Context.emitExternal(Extern.Def(f.symbol, Context.functionTypeOf(sym), (vps map transform) ++ (bps map transform), body))
      rest()

    case e @ source.ExternInclude(path, contents, _) =>
      Context.emitExternal(Extern.Include(contents))
      rest()

    // For now we forget about all of the following definitions in core:
    case d: source.ExternResource => rest()
    case d: source.ExternType => rest()
    case d: source.ExternInterface => rest()
    case d: source.TypeDef => rest()
    case d: source.EffectDef => rest()
  }

  def transform(tree: source.Stmt)(using Context): Stmt = tree match {
    case source.DefStmt(d, rest) =>
      transform(d, () => transform(rest))

    // { e; stmt } --> { let _ = e; stmt }
    case source.ExprStmt(e, rest) if pureOrIO(e) =>
      val (expr, bs) = Context.withBindings { transformAsExpr(e) }
      val let = Let(freshWildcardFor(e), expr, transform(rest))
      if (bs.isEmpty) { let }
      else { Context.reifyBindings(let, bs) }

    // { e; stmt } --> { val _ = e; stmt }
    case source.ExprStmt(e, rest) =>
      Val(freshWildcardFor(e), insertBindings { Return(transformAsPure(e)) }, transform(rest))

    case source.Return(e) =>
      insertBindings { Return(transformAsPure(e)) }

    case source.BlockStmt(b) =>
      transform(b)
  }

  def transformLit(tree: source.Literal)(using Context): Literal[_] = tree match {
    case source.Literal(value: Unit, _)    => UnitLit()
    case source.Literal(value: Int, _)     => IntLit(value)
    case source.Literal(value: Boolean, _) => BooleanLit(value)
    case source.Literal(value: Double, _)  => DoubleLit(value)
    case source.Literal(value: String, _)  => StringLit(value)
    case source.Literal(value, _)          => Context.panic(s"Unknown literal value: ${value}")
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

      New(Implementation(sig, sig.ops.map(clauses.apply).map {
        case op @ source.OpClause(id, tparams, vparams, ret, body, resume) =>
          val vps = vparams map transform
          // currently the operations don't take block params
          val opBlock: BlockLit = BlockLit(vps, transform(body))
          core.Operation(op.definition, opBlock)
      }))

    case source.Unbox(b) =>
      Unbox(transformAsPure(b))

    case source.BlockLiteral(tps, vps, bps, body) =>
      BlockLit((vps map transform) ++ (bps map transform), transform(body))

    case _ =>
      transformUnbox(tree)
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

    case l: source.Literal => transformLit(l)

    case s @ source.Select(receiver, selector) =>
      Select(transformAsPure(receiver), s.definition)

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
      val exprTpe = Context.inferredTypeOf(tree)
      Context.bind(exprTpe, If(c, transform(thn), transform(els)))

    case source.While(cond, body) =>
      val exprTpe = Context.inferredTypeOf(tree)
      Context.bind(exprTpe, While(insertBindings { Return(transformAsPure(cond)) }, transform(body)))

    case source.Match(sc, cs) =>
      // (1) Bind scrutinee and all clauses so we do not have to deal with sharing on demand.
      val scrutinee: ValueVar = Context.bind(Context.inferredTypeOf(sc), transformAsPure(sc))
      val clauses = cs.map(c => preprocess(scrutinee.id, c))
      val compiledMatch = Context.at(tree) { compileMatch(clauses) }
      Context.bind(Context.inferredTypeOf(tree), compiledMatch)

    case source.TryHandle(prog, handlers) =>
      val caps = handlers.map { h =>
        val cap = h.capability.get.symbol
        core.BlockParam(cap, cap.tpe)
      }
      val body = BlockLit(caps, transform(prog))

      // to obtain a canonical ordering of operation clauses, we use the definition ordering
      def transformHandler: source.Handler => core.Implementation = {
        case h @ source.Handler(cap, source.Implementation(eff, cls)) =>
          val clauses = cls.map { cl => (cl.definition, cl) }.toMap

          Implementation(h.definition, h.definition.ops.map(clauses.apply).map {
            case op @ source.OpClause(id, tps, vps, ret, body, resume) =>
              val ps = vps map transform

              // introduce a block parameter for resume
              val resumeParam = BlockParam(resume.symbol.asInstanceOf[BlockSymbol])

              val opBlock: BlockLit = BlockLit(ps :+ resumeParam, transform(body))
              core.Operation(op.definition, opBlock)
          })
      }

      val answerType = Context.inferredTypeOf(tree)

      Context.bind(answerType, Try(body, answerType, handlers map transformHandler))

    case r @ source.Region(name, body) =>
      val sym = r.symbol
      val tpe = sym match {
        case b: BlockParam => b.tpe
        case b: SelfParam => builtins.TRegion
        case _ => Context.panic("Continuations cannot be regions")
      }
      val cap = core.BlockParam(sym, tpe)
      val answerType = Context.inferredTypeOf(tree)
      Context.bind(answerType, Region(BlockLit(List(cap), transform(body)), answerType))

    case source.Hole(stmts) =>
      Context.bind(Context.inferredTypeOf(tree), Hole)

    case a @ source.Assign(id, expr) =>
      val e = transformAsPure(expr)
      val sym = a.definition
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

      val (funTpe, capture) = Context.inferredTypeOf(expr) match {
        case BoxedType(s: FunctionType, c: CaptureSet) => (s, c)
        case _ => Context.panic("Should be a boxed function type with a known capture set.")
      }
      val e = transformAsPure(expr)
      val typeArgs = Context.typeArguments(c)
      val valueArgs = vargs.map(transformAsPure)
      val blockArgs = bargs.map(transformAsBlock)

      if (pureOrIO(capture) && bargs.forall { pureOrIO }) {
        Run(App(Unbox(e), typeArgs, valueArgs ++ blockArgs), funTpe.result)
      } else {
        Context.bind(Context.inferredTypeOf(tree), App(Unbox(e), typeArgs, valueArgs ++ blockArgs))
      }

    case c @ source.Call(fun: source.IdTarget, _, vargs, bargs) =>
      // assumption: typer removed all ambiguous references, so there is exactly one
      makeFunctionCall(c, fun.definition, vargs, bargs)

    case c @ source.Call(s: source.ExprTarget, targs, vargs, bargs) =>
      Context.panic("Should not happen. Unbox should have been inferred.")

    case source.Do(effect, id, targs, vargs) =>
      Context.panic("Should have been translated away (to explicit selection `@CAP.op()`) by capability passing.")
  }

  def makeFunctionCall(call: source.CallLike, sym: TermSymbol, vargs: List[source.Term], bargs: List[source.Term])(using Context): Expr = {
    // the type arguments, inferred by typer
    val targs = Context.typeArguments(call)

    val vargsT = vargs.map(transformAsPure)
    val bargsT = bargs.map(transformAsBlock)
    val as = vargsT ++ bargsT

    sym match {
      case f: ExternFunction if isPure(f.capture) =>
        // if (bargs.nonEmpty) Context.abort("Pure builtin functions cannot take block arguments.")
        PureApp(BlockVar(f), targs, vargsT)
      case f: ExternFunction if pureOrIO(f.capture) =>
        DirectApp(BlockVar(f), targs, as)
      case r: Constructor =>
        if (bargs.nonEmpty) Context.abort("Constructors cannot take block arguments.")
        PureApp(BlockVar(r), targs, vargsT)
      case f: Operation =>
        Context.panic("Should have been translated to a method call!")
      case f: Field =>
        Context.panic("Should have been translated to a select!")
      case f: BlockSymbol if pureOrIO(f) && bargs.forall { pureOrIO } =>
        Run(App(BlockVar(f), targs, as), Context.functionTypeOf(f).result)
      case f: BlockSymbol =>
        Context.bind(Context.inferredTypeOf(call), App(BlockVar(f), targs, as))
      case f: ValueSymbol =>
        Context.bind(Context.inferredTypeOf(call), App(Unbox(ValueVar(f)), targs, as))
    }
  }

  def transform(p: source.BlockParam)(using Context): core.BlockParam = BlockParam(p.symbol)
  def transform(p: source.ValueParam)(using Context): core.ValueParam = ValueParam(p.symbol)

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
  private case class Clause(patterns: Map[ValueSymbol, source.MatchPattern], label: BlockSymbol, args: List[ValueSymbol])

  // Uses the bind effect to bind the right hand sides of clauses!
  private def preprocess(sc: ValueSymbol, clause: source.MatchClause)(using Context): Clause = {
    def boundVars(p: source.MatchPattern): List[ValueParam] = p match {
      case p @ source.AnyPattern(id) => List(p.symbol)
      case source.TagPattern(id, patterns) => patterns.flatMap(boundVars)
      case _ => Nil
    }
    val params = boundVars(clause.pattern).map { p => (p, Context.valueTypeOf(p)) }
    val body = transform(clause.body)
    val blockLit = BlockLit(params.map { case (p, tpe) => core.ValueParam(p, tpe) }, body)
    val returnType = Context.inferredTypeOf(clause.body)
    val blockTpe = symbols.FunctionType(Nil, Nil, params.map { case (_, tpe) => tpe }, Nil, returnType, Effects.Pure)

    // TODO Do we also need to annotate the capture???
    val joinpoint = Context.bind(blockTpe, blockLit)
    Clause(Map(sc -> clause.pattern), joinpoint.id, params.map { case (p, _) => p })
  }

  /**
   * The match compiler works with
   * - a sequence of clauses that represent alternatives (disjunction)
   * - each sequence contains a list of patterns that all have to match (conjunction).
   */
  private def compileMatch(clauses: Seq[Clause])(using Context): core.Stmt = {

    // matching on void will result in this case
    if (clauses.isEmpty) return core.Hole

    val normalizedClauses = clauses.map(normalize)

    def jumpToBranch(target: BlockSymbol, args: List[ValueSymbol]) =
      core.App(core.BlockVar(target), Nil, args.map(a => core.ValueVar(a)))

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

    def freshFields(c: Constructor) = c.fields.map { f =>
      val tmp = Tmp.apply(Context.module)
      Context.assignType(tmp, f.returnType)
      tmp
    }
    val varsFor: Map[Constructor, List[Tmp]] = variants.map { v => v -> freshFields(v) }.toMap

    normalizedClauses.foreach {
      case c @ Clause(patterns, target, args) => patterns.get(splitVar) match {
        case Some(p @ source.TagPattern(id, ps)) =>
          val constructor = p.definition
          addClause(constructor, Clause(patterns - splitVar ++ varsFor(constructor).zip(ps), target, args))
        case Some(_) => Context.panic("Should not happen")
        case None =>
          // Clauses that don't match on that var are duplicated.
          // So we want to choose our branching heuristic to minimize this
          addDefault(c)
          // THIS ONE IS NOT LINEAR
          variants.foreach { v => addClause(v, c) }
      }
    }

    // TODO order variants by declaration of constructor.
    val branches = variants.toList.map { v =>
      // TODO only add defaultVar as `self`, if it is free in the body.
      val body = compileMatch(clausesFor.getOrElse(v, Vector.empty))
      val params = varsFor(v).map { tmp => ValueParam(tmp) }
      val blockLit: BlockLit = BlockLit(params, body)
      (v, blockLit)
    }

    val default = if defaults.isEmpty then None else Some(compileMatch(defaults))

    core.Match(ValueVar(splitVar), branches, default)
  }

  /**
   * Substitutes IdPatterns and removes wildcards (and literal patterns -- which are already ruled out by Typer);
   * only TagPatterns are left.
   */
  private def normalize(clause: Clause)(using Context): Clause = {
    val Clause(patterns, target, args) = clause
    val substitution = patterns.collect { case (v, source.AnyPattern(id)) => id.symbol -> v }
    val tagPatterns = patterns.collect { case (v, p: source.TagPattern) => v -> p }

    // HERE WE COULD GATHER AN EXPLICIT SUBSTITUTION ON THE RHS
    Clause(tagPatterns, target, args.map(v => substitution.getOrElse(v, v)))
  }


  // Helpers
  // -------

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
        case core.Run(core.Return(p), _) => rewrite(p)
      }
    }

    // rewrite (Val (Return e) s) to (Let e s)
    object directStyleVal extends core.Tree.Rewrite {
      override def stmt = {
        case core.Val(id, tpe, core.Return(expr), body) =>
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
      def isResource = c.isInstanceOf[Resource]
      def isControl = c == builtins.ControlCapability.capture
      !isControl && (isIO || isMutableState || isResource)
  }
}

private[core] enum Binding {
  case Val(name: Tmp, tpe: symbols.ValueType, binding: Stmt)
  case Let(name: Tmp, tpe: symbols.ValueType, binding: Expr)
  case Def(name: TmpBlock, tpe: symbols.BlockType, binding: Block)
}

trait TransformerOps extends ContextOps { Context: Context =>


  /**
   * A _mutable_ ListBuffer that stores all bindings to be inserted at the current scope
   */
  private var bindings: ListBuffer[Binding] = ListBuffer()
  private var declarations: ListBuffer[core.Decl] = ListBuffer()
  private var externals: ListBuffer[core.Extern] = ListBuffer()

  private[core] def initTransformerState() = {
    bindings = ListBuffer()
    declarations = ListBuffer()
    externals = ListBuffer()
  }

  /**
   * Introduces a binding for the given statement.
   *
   * @param tpe the type of the bound statement
   * @param s the statement to be bound
   */
  private[core] def bind(tpe: symbols.ValueType, s: Stmt): ValueVar = {

    // create a fresh symbol and assign the type
    val x = Tmp(module)
    assignType(x, tpe)

    val binding = Binding.Val(x, tpe, s)
    bindings += binding

    ValueVar(x)
  }

  private[core] def bind(tpe: symbols.ValueType, s: Expr): ValueVar = {

    // create a fresh symbol and assign the type
    val x = Tmp(module)
    assignType(x, tpe)

    val binding = Binding.Let(x, tpe, s)
    bindings += binding

    ValueVar(x)
  }

  private[core] def bind(tpe: symbols.BlockType, b: Block): BlockVar = {

    // create a fresh symbol and assign the type
    val x = TmpBlock(module)
    assignType(x, tpe)

    val binding = Binding.Def(x, tpe, b)
    bindings += binding

    BlockVar(x)
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
      case (Binding.Val(x, tpe, b), Return(ValueVar(y))) if x == y => b
      case (Binding.Val(x, tpe, b), body) => Val(x, tpe, b, body)
      case (Binding.Let(x, tpe, Run(s, _)), Return(ValueVar(y))) if x == y => s
      case (Binding.Let(x, tpe, b: Pure), Return(ValueVar(y))) if x == y => Return(b)
      case (Binding.Let(x, tpe, b), body) => Let(x, tpe, b, body)
      case (Binding.Def(x, tpe, b), body) => Def(x, tpe, b, body)
    }
  }

  private[core] def emitDeclaration(decl: core.Decl): Unit = declarations += decl

  private[core] def emitExternal(decl: core.Extern): Unit = externals += decl

  private[core] def gatheredDeclarations = declarations.toList
  private[core] def gatheredExternals = externals.toList
}
