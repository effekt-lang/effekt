package effekt
package regions

import effekt.source._
import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.{ Binder, BlockSymbol, Effectful, Symbol, UserFunction, ValueSymbol }
import effekt.context.assertions._

object RegionChecker extends Phase[Typechecked, Typechecked] {

  val phaseName = "region-checker"

  def run(input: Typechecked)(implicit C: Context): Option[Typechecked] =
    val Typechecked(source, tree, mod) = input
    C.using(module = mod, focus = tree) {
      Context.initRegionstate()
      Context.unifyAndSubstitute()

      try {
        // this should go into a precheck method
        tree.defs.foreach {
          case f: FunDef =>
            Context.annotateRegions(f.symbol, C.staticRegion)
          case f: ExternFun =>
            val reg =
              if (f.purity == ExternFlag.IO) {
                Region(symbols.builtins.IOCapability)
              } else if (f.purity == ExternFlag.Control) {
                Region(symbols.builtins.ControlCapablity)
              } else {
                Region.empty
              }
            Context.annotateRegions(f.symbol, reg)
          case d: DataDef =>
            d.ctors.foreach { c =>
              Context.annotateRegions(c.symbol, Region.empty)
            }
          case d: RecordDef =>
            val sym = d.symbol
            Context.annotateRegions(sym, Region.empty)
            sym.fields.foreach { f =>
              Context.annotateRegions(f, Region.empty)
            }
          case _ => ()
        }
        check(tree)
        Some(input)
      } finally {
        Context.commitConstraints()
      }
    }


  // A traversal with the side effect to annotate all functions with their region
  // it returns a _concrete_ region set, all region variables have to be resolved
  // at that point.
  def checkTree(implicit C: Context): PartialFunction[Tree, RegionSet] = {

    case FunDef(id, tparams, params, ret, body) =>
      val sym = id.symbol

      // Since this function might be (mutally) recursive, annotate it with the current region
      // before checking.
      //
      // This is a conservative approximation that can be refined, later, potentially
      // by using region variables and collecting constraints.
      Context.annotateRegions(sym, C.staticRegion)

      // regions of parameters introduced by this function
      val boundRegions: RegionSet = bindRegions(params)

      val selfRegion = Region(sym)
      val bodyRegion = Context.inDynamicRegion(selfRegion) { check(body) }
      val reg = bodyRegion -- boundRegions -- selfRegion

      // check that the self region (used by resume and variables) does not escape the scope
      val tpe = Context.blockTypeOf(sym)
      val escapes = Context.freeRegionVariables(tpe.ret) intersect selfRegion
      if (escapes.nonEmpty) {
        Context.explain(sym, body)
        Context.abort(s"A value that is introduced in '${id.name}' leaves its scope.")
      }

      // safe inferred region on the function symbol
      Context.annotateRegions(id.symbol, reg)
      reg

    case l @ Lambda(id, params, body) =>
      val sym = l.symbol
      // annotated by typer
      val expected = Context.regionOf(sym)
      val boundRegions: RegionSet = bindRegions(params)

      val selfRegion = Region(sym)
      val bodyRegion = Context.inDynamicRegion(selfRegion) { check(body) }

      val inferredReg = bodyRegion -- boundRegions -- selfRegion

      // check that expected >: inferredReg
      val reg = expected.withRegion { allowed =>
        val forbidden = inferredReg -- allowed

        if (!forbidden.isEmpty) {
          forbidden.regions.foreach { sym =>
            Context.explain(sym, body)
          }

          Context.abort(s"Region not allowed here: ${inferredReg}")
        }
        allowed
      }.getOrElse { inferredReg }

      // check that the self region does not escape as part of the lambdas type
      val tpe = Context.blockTypeOf(sym)
      val escapes = Context.freeRegionVariables(tpe.ret) intersect selfRegion
      if (escapes.nonEmpty) {
        Context.explain(sym, body)
        Context.abort(s"A value that is introduced in this lambda leaves its scope.")
      }

      // safe inferred region on the function symbol
      Context.annotateRegions(sym, reg)

      // if expected was a variable, instantiate it.
      if (!expected.isInstantiated) {
        expected.asRegionVar.instantiate(reg)
        unifyAndExplain(body)
      }
      reg

    case b @ BlockArg(params, body) =>
      // Here we just make up a symbol on the spot (symbols.BlockArg)
      // typically, we use the blockparam to look up types etc., this one
      // is only used to represent the scope / region
      val selfRegion = Region(symbols.BlockArg(b))
      val boundRegions: RegionSet = bindRegions(params)

      // Refining the dynamic region here is necessary to fix #50 -- the region inferred for the continuation is not precise enough since
      // a higher-order function can introduce handlers !
      val bodyRegion = Context.inDynamicRegion(selfRegion) { check(body) }
      bodyRegion -- boundRegions

    // TODO What about capabilities introduced by resume????
    case TryHandle(body, handlers) =>

      // regions for all the capabilities
      val caps = handlers.flatMap { h => h.capability }
      val boundRegions = bindRegions(caps)
      val bodyRegion = Context.inRegion(boundRegions) { check(body) }

      var reg = bodyRegion -- boundRegions

      // check that boundRegions do not escape as part of an inferred type
      val t @ (tpe, _) = C.inferredTypeAndEffectOf(body)

      val escapes = Context.freeRegionVariables(t) intersect boundRegions
      if (escapes.nonEmpty) {
        escapes.regions.foreach { sym =>
          Context.explain(s"The return type mentions capability ${sym}", sym, body)
        }
        Context.abort(s"The value returned from this handler has type ${tpe}. \nAs part of this type, the following capabilities leave their defining scope ${escapes}.")
      }

      handlers.foreach {
        case Handler(id, cap, clauses) => clauses.foreach {
          case OpClause(id, params, body, resumeId) =>
            val resumeSym = resumeId.symbol
            val resumeReg = Context.dynamicRegion
            Context.annotateRegions(resumeSym, resumeReg)
            reg ++= check(body)
        }
      }
      reg

    // capability call
    case MemberTarget(cap, op) =>
      Context.regionOf(cap.symbol).asRegionSet

    case tgt @ IdTarget(id) => id.symbol match {
      case b: BlockSymbol =>
        Context.regionOf(b).asRegionSet
      case t: ValueSymbol =>
        val symbols.FunType(tpe, reg) = Context.valueTypeOf(t).dealias
        reg.asRegionSet
    }

    case ExprTarget(e) =>
      val reg = check(e)
      val symbols.FunType(tpe, funReg) = Context.inferredTypeOf(e)
      reg ++ funReg.asRegionSet

    case VarDef(id, _, binding) =>
      Context.annotateRegions(id.symbol, Context.dynamicRegion)
      val reg = check(binding)
      // associate the mutable variable binding with the current scope
      reg

    case Var(id) if id.symbol.isInstanceOf[symbols.VarBinder] =>
      Context.regionOf(id.symbol).asRegionSet

    case Assign(id, expr) =>
      val res = check(expr) ++ Context.regionOf(id.symbol).asRegionSet
      res

    // TODO eventually we want to change the representation of block types to admit region polymorphism
    // everywhere where blocktypes are allowed. For now, we only support region polymorphism on known functions.
    // This restriction is fine, since we do only allow second-order blocks anyway.

    // calls to known functions

    // TODO check: resumptions can take block arguments (with bidirectional effects), but are not "known functions"
    case c @ Call(id: IdTarget, _, args) if id.definition.isInstanceOf[symbols.Fun] =>
      var reg = check(id)
      val fun = id.definition.asFun
      val tpe = Context.inferredTypeOf(c)

      // there can be more args than params (for capability args)
      val params = fun.params.padTo(args.size, null)

      (params zip args).foreach {
        case (param, arg: ValueArgs) => reg ++= check(args)
        case (List(param: symbols.BlockParam), arg: BlockArg) =>
          val argReg = check(arg)
          reg ++= argReg

          // here we substitute the inferred region for the block parameter in the return type.
          substitute(param, argReg, tpe)

        // we are using the capability, so we should only run in their region
        case (param, arg: CapabilityArg) => reg ++= Region(arg.definition)
        case _ => Context.panic("Unreachable case")
      }

      // check constraints again after substitution
      unifyAndExplain(c)

      reg

    // calls to unknown functions (block arguments, lambdas, etc.)
    case c @ Call(target, _, args) =>
      val tpe = Context.inferredTypeOf(c)
      args.foldLeft(check(target)) { case (reg, arg) => reg ++ check(arg) }

    case c: CapabilityArg => Region(c.definition)
  }

  def unifyAndExplain(body: Tree)(implicit C: Context): Unit =
    Context.tryUnifyAndSubstitute().foreach {
      // error case...
      case RegionEq(RegionSet(exp), RegionSet(got), tree) =>
        val notAllowed = got -- exp
        notAllowed.regions.foreach { sym =>
          Context.explain(s"Not allowed: ${sym}", sym, tree)
        }
        Context.at(tree) {
          Context.abort(s"Region mismatch: expected $exp but got $got")
        }

      case RegionEq(_, _, _) => Context.panic("All region variables should've been resolved by now.")
    }

  def bindRegions(params: List[ParamSection])(implicit C: Context): RegionSet = {
    var regs: RegionSet = Region.empty
    params.foreach {
      case b: BlockParam =>
        val sym = b.symbol
        val reg = Region(sym)
        Context.annotateRegions(sym, reg)
        regs ++= reg
      case b: CapabilityParam =>
        val sym = b.symbol
        val reg = Region(sym)
        Context.annotateRegions(sym, reg)
        regs ++= reg
      case v: ValueParams => ()
    }
    regs
  }

  def check(obj: Any)(implicit C: Context): RegionSet = obj match {
    case _: Symbol | _: String => Region.empty
    case t: Tree =>
      C.at(t) {
        val reg = if (checkTree.isDefinedAt(t)) {
          checkTree(C)(t)
        } else {
          t.productIterator.foldLeft(Region.empty) { case (r, t) => r ++ check(t) }
        }
        C.annotateRegion(t, reg)
        reg
      }
    case p: Product =>
      p.productIterator.foldLeft(Region.empty) { case (r, t) => r ++ check(t) }
    case t: Iterable[t] =>
      t.foldLeft(Region.empty) { case (r, t) => r ++ check(t) }
    case leaf =>
      Region.empty
  }

  def substitute(x: Symbol, r: RegionSet, o: Any)(implicit C: Context): Unit = o match {
    case _: Symbol | _: String => ()
    case y: RegionVar =>
      val reg = y.asRegionSet.substitute(x, r)
      y.instantiate(reg)
    case t: Iterable[t] =>
      t.foreach { t => substitute(x, r, t) }
    case p: Product =>
      p.productIterator.foreach { t => substitute(x, r, t) }
    case _ =>
      ()
  }
}

trait RegionCheckerOps extends ContextOps { self: Context =>

  // the current lexical region
  private[regions] var staticRegion: RegionSet = Region.empty

  // the current dynamical region (as approximated by the owner handler / lambda / function symbol )
  // only used for continuations!
  private[regions] var dynamicRegion: RegionSet = Region.empty

  private[regions] var constraints: List[RegionEq] = Nil

  private[regions] def initRegionstate(): Unit = {
    staticRegion = Region.empty
    constraints = annotation(Annotations.Unifier, module).constraints.toList
  }

  private[regions] def commitConstraints(): Unit = {
    val unifier = annotation(Annotations.Unifier, module)
    annotate(Annotations.Unifier, module, unifier.copy(constraints = constraints.toSet))
  }

  private[regions] def inRegion[T](r: RegionSet)(block: => T): T = {
    val staticBefore = staticRegion
    val dynamicBefore = dynamicRegion
    staticRegion = r
    dynamicRegion = r
    val res = block
    staticRegion = staticBefore
    dynamicRegion = dynamicBefore
    res
  }

  private[regions] def inDynamicRegion[T](r: RegionSet)(block: => T): T = {
    val dynamicBefore = dynamicRegion
    dynamicRegion = r
    val res = block
    dynamicRegion = dynamicBefore
    res
  }

  private[regions] def annotateRegion(t: Tree, r: RegionSet): Unit =
    annotate(Annotations.InferredRegion, t, r)

  /**
   * Returns the failed constraint or None if successful
   */
  private[regions] def tryUnifyAndSubstitute(): Option[RegionEq] = unifyAndSubstitute(constraints) match {
    case Left(r) => Some(r)
    case Right(cs) =>
      constraints = cs
      None
  }

  private[regions] def unifyAndSubstitute(): Unit = tryUnifyAndSubstitute().foreach {
    case RegionEq(RegionSet(exp), RegionSet(got), tree) => at(tree) {
      abort(s"Region mismatch: expected $exp but got $got")
    }
    case RegionEq(_, _, _) => panic("All region variables should've been resolved by now.")
  }

  /**
   * If unification is successful returns a new list of remaining constraints: Right(List[RegionEq])
   * If unification fails, returns the conflicting constraint.
   */
  private def unifyAndSubstitute(cs: List[RegionEq]): Either[RegionEq, List[RegionEq]] = (cs.distinct: @unchecked) match {

    // if both are instantiated -> compare their sets
    case (r @ RegionEq(RegionSet(x), RegionSet(y), _)) :: rest =>
      if (x != y) {
        Left(r)
      } else {
        unifyAndSubstitute(rest)
      }

    // if one is a variable -> instantiate
    case RegionEq(x: RegionVar, RegionSet(r), _) :: rest =>
      x.instantiate(r);
      unifyAndSubstitute(rest)

    // if one is a variable -> instantiate
    case RegionEq(RegionSet(r), x: RegionVar, _) :: rest =>
      x.instantiate(r);
      unifyAndSubstitute(rest)

    // if both are variables -> keep constraint
    case (r @ RegionEq(x: RegionVar, y: RegionVar, _)) :: rest =>
      unifyAndSubstitute(rest).map(r :: _)

    case Nil => Right(Nil)
  }

  /**
   * A generic traversal to collects all free region variables
   */
  private[regions] def freeRegionVariables(o: Any): RegionSet = o match {
    case _: Symbol | _: String => Region.empty // don't follow symbols
    case symbols.FunType(tpe, reg) =>
      freeRegionVariables(tpe) ++ reg.asRegionSet
    case e: symbols.Effects => freeRegionVariables(e.toList)
    case t: Iterable[t] =>
      t.foldLeft(Region.empty) { case (r, t) => r ++ freeRegionVariables(t) }
    case p: Product =>
      p.productIterator.foldLeft(Region.empty) { case (r, t) => r ++ freeRegionVariables(t) }
    case _ =>
      Region.empty
  }
}

/**
 * When a region error occurs, explanations are gathered in form of a trace
 */
case class TraceItem(msg: String, tree: Tree, subtrace: List[TraceItem] = Nil) {

  def report(implicit C: Context): Unit = {
    C.info(tree, msg)
    subtrace.foreach { t => t.report }
  }

  def render(implicit C: Context): String = {
    val pos = C.positions.getStart(tree).map { p => s"(line ${p.line}) " }.getOrElse("")
    val sub = if (subtrace.isEmpty) "" else {
      "\n" + subtrace.map(_.render).mkString("\n").linesIterator.map("  " + _).mkString("\n")
    }
    s"- ${pos}${msg}${sub}"
  }
}

trait RegionReporter { self: Context =>

  type Trace = List[TraceItem]

  /**
   * The seen set is used to avoid nontermination
   *
   * TODO is it problematic to refer to the trees that are not transformed by capability passing?
   */
  def explainEscape(reg: Symbol, seen: Set[Symbol]): PartialFunction[Tree, Trace] = {
    case f @ FunDef(id, tparams, params, ret, body) if uses(f, reg) =>
      TraceItem(s"Function '${id.name}' closes over '$reg'", body, explainEscape(reg, body, seen + f.symbol)) :: Nil

    case l @ Lambda(id, params, body) if uses(l, reg) =>
      TraceItem(s"The lambda closes over '$reg'", l, explainEscape(reg, body, seen + l.symbol)) :: Nil

    case m @ MemberTarget(cap, op) if symbolOf(cap) == reg =>
      TraceItem(s"The problematic effect is used here", m) :: Nil

    case tgt @ IdTarget(id) if uses(tgt, reg) =>
      val rest = tgt.definition match {
        // TODO here we could follow to the definition.
        // however, like below, it is a _source tree_, before capability passing.
        case fun: UserFunction if !seen.contains(fun) => Nil // explainEscape(reg, fun.decl, seen)
        case _ => Nil
      }
      TraceItem(s"Function ${id.name} is called, which closes over '${reg}'", tgt, rest) :: Nil

    case DefStmt(d, rest) =>
      explainEscape(reg, rest, seen)

    // here we could follow value and variable binders
    // however, the declaration is a _source_ tree, before capability passing.
    //    case x @ Var(id) if x.definition.isInstanceOf[Binder] && mentionsInType(x, reg) =>
    //      val tree = x.definition.asBinder.decl
    //      TraceItem(s"Reference to a variable that captures '${reg}'", x, explainEscape(reg, tree, seen)) :: Nil

    case v @ Return(e) =>
      if (mentionsInType(e, reg)) {
        TraceItem(s"A value is returned that mentions '${reg}' in its inferred type", e, explainEscape(reg, e, seen)) :: Nil
      } else {
        explainEscape(reg, e, seen)
      }
  }

  def explain(escapingRegion: Symbol, tree: Tree): Unit =
    explainEscape(escapingRegion, tree, Set.empty).foreach { _.report }

  def explain(msg: String, escapingRegion: Symbol, tree: Tree): Unit =
    TraceItem(msg, tree, explainEscape(escapingRegion, tree, Set.empty)).report

  def explainEscape(escapingRegion: Symbol, obj: Any, seen: Set[Symbol]): Trace = obj match {
    case _: Symbol | _: String => Nil
    case t: Tree =>
      at(t) {
        if (explainEscape(escapingRegion, seen).isDefinedAt(t)) {
          explainEscape(escapingRegion, seen)(t)
        } else if (inferredRegionOption(t).forall(_.contains(escapingRegion))) {
          t.productIterator.foldLeft(Nil: Trace) { case (r, t) => r ++ explainEscape(escapingRegion, t, seen) }
        } else {
          Nil
        }
      }
    case p: Product =>
      p.productIterator.foldLeft(Nil: Trace) { case (r, t) => r ++ explainEscape(escapingRegion, t, seen) }
    case t: Iterable[t] =>
      t.foldLeft(Nil: Trace) { case (r, t) => r ++ explainEscape(escapingRegion, t, seen) }
    case leaf =>
      Nil
  }

  private def mentionsInType(t: Tree, reg: Symbol): Boolean =
    freeRegionVariables(inferredTypeAndEffectOf(t)).contains(reg)

  private def uses(t: Tree, reg: Symbol): Boolean =
    inferredRegionOption(t).exists(_.contains(reg))
}
