package effekt
package core

import effekt.context.Context
import effekt.PhaseResult.CoreTransformed
import effekt.core.optimizer.BindSubexpressions
import effekt.util.messages.INTERNAL_ERROR

import scala.collection.mutable

/**
 * Arity raising: type-directed unboxing of records, after monomorphization.
 *
 * A value of a flat record type R with constructor C(T₁, …, Tₙ) is represented by its
 * components, and arrays and references of records become records of arrays and references:
 *
 *   ⟦R⟧    = ⟦T₁⟧ ++ … ++ ⟦Tₙ⟧      if R is flat
 *   ⟦A[T]⟧ = ⟦T⟧ map (U => A[U])     if A ∈ {Array, Ref} and A[T] is not frozen
 *   ⟦T⟧    = [T]                     otherwise
 *
 * The representation is global: parameters, results (multiple returns), fields of all data
 * types, function types, prompts, continuations and local state are translated pointwise.
 * Only externs see the original representation; at extern calls records are packed (make)
 * and opened (match). The normalizer removes the remaining matches on known constructors.
 *
 * A record type R is *flat* if
 *   - it has exactly one constructor with at least one field,
 *   - it is not recursive,
 *   - some match scrutinizes a value of type R (otherwise flattening can only add packing),
 *   - it does not occur in a function type at an extern boundary (which would require eta).
 *
 * A container type A[T] is *frozen* if it occurs at an extern boundary that is not
 * [[componentwise]]. Containers nested in frozen types are frozen as well.
 */
object ArityRaising extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "arity-raising"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case input @ CoreTransformed(_, _, _, core) =>
      if Context.config.optimize() then Some(input.copy(core = transform(core)))
      else Some(input)
  }

  def transform(m: ModuleDecl): ModuleDecl = Raise(BindSubexpressions.transform(m)).module

  /** All value types occurring in a type, including the type itself. */
  def occurrences(tpe: ValueType): List[ValueType] = tpe :: (tpe match {
    case ValueType.Var(_) => Nil
    case ValueType.Data(_, targs) => targs.flatMap(occurrences)
    case ValueType.Boxed(b, _) => occurrences(b)
  })

  def occurrences(tpe: BlockType): List[ValueType] = tpe match {
    case BlockType.Function(_, _, vparams, bparams, result) =>
      (vparams ++ result).flatMap(occurrences) ++ bparams.flatMap(occurrences)
    case BlockType.Interface(_, targs) => targs.flatMap(occurrences)
    case BlockType.Prompt(answer) => answer.flatMap(occurrences)
    case BlockType.Resume(result, answer) => (result ++ answer).flatMap(occurrences)
  }

  /** Value types occurring inside function types. */
  def underFunctions(tpe: ValueType): List[ValueType] =
    occurrences(tpe).collect { case ValueType.Boxed(b, _) => occurrences(b) }.flatten

  def mentions(tpe: ValueType, t: Id): Boolean = occurrences(tpe).contains(ValueType.Var(t))

  // Generic traversal of everything reachable from a tree (including types)
  private def visitAll(obj: Any)(f: Any => Unit): Unit = {
    f(obj)
    obj match {
      case _: symbols.Symbol => ()
      case t: Iterable[?] => t.foreach(visitAll(_)(f))
      case p: Product => p.productIterator.foreach(visitAll(_)(f))
      case _ => ()
    }
  }

  private case class CallSite(f: Extern.Def, targs: List[ValueType], control: Boolean)

  private case class Env(values: Map[Id, List[Expr]], refs: Map[Id, List[Id]]) {
    def bind(id: Id, params: List[ValueParam]): Env =
      copy(values = values + (id -> params.map(p => ValueVar(p.id, p.tpe))))
    def bind(ids: List[Id], params: List[List[ValueParam]]): Env =
      ids.zip(params).foldLeft(this) { case (env, (id, ps)) => env.bind(id, ps) }
    def bindRef(id: Id, refs: List[Id]): Env = copy(refs = this.refs + (id -> refs))
  }

  private class Raise(m: ModuleDecl) {

    val externs: Map[Id, Extern.Def] = m.externs.collect { case f: Extern.Def => f.id -> f }.toMap

    // The only name-based step: the extern types of `array::Array` and `ref::Ref`.
    val containers: Set[Id] = m.externs.collect {
      case Extern.Data(id, List(_), _) if id.name.name == "Array" || id.name.name == "Ref" => id
    }.toSet

    def isContainer(tpe: ValueType): Boolean = tpe match {
      case ValueType.Data(a, List(_)) => containers.contains(a)
      case _ => false
    }

    /**
     * An extern f[T] is componentwise if it has no block parameters, and T only occurs bare
     * or directly under a container, at least once under a container. By parametricity, such
     * an extern commutes with unzipping A[(T₁, …, Tₙ)] into (A[T₁], …, A[Tₙ]); we thus call
     * it once per component. Externs that mention T only bare (like `genericEquals[T](x: T, y: T)`)
     * may inspect their arguments and are not componentwise.
     */
    def componentwise(f: Extern.Def): Boolean = f match {
      case Extern.Def(_, _, List(t), _, vparams, Nil, ret, _, _) =>
        val tpes = ret :: vparams.map(_.tpe)
        def underContainer(tpe: ValueType) = tpe match {
          case ValueType.Data(a, List(ValueType.Var(`t`))) => containers.contains(a)
          case _ => false
        }
        tpes.forall(tpe => tpe == ValueType.Var(t) || underContainer(tpe) || !mentions(tpe, t)) &&
          tpes.exists(underContainer)
      case _ => false
    }

    def signature(f: Extern.Def, targs: List[ValueType]): BlockType.Function =
      Type.instantiate(
        BlockType.Function(f.tparams, f.cparams, f.vparams.map(_.tpe), f.bparams.map(_.tpe), List(f.ret)),
        targs, f.cparams.map(c => Set(c)))

    /** The types at the boundary of an extern call; block parameters count as (boxed) functions. */
    def boundary(site: CallSite): List[ValueType] = signature(site.f, site.targs) match {
      case BlockType.Function(_, _, vparams, bparams, result) => vparams ++ result ++ bparams.map(b => ValueType.Boxed(b, Set.empty))
    }

    // Analysis
    // --------
    val callSites = mutable.ListBuffer.empty[CallSite]
    val interfaceArguments = mutable.ListBuffer.empty[ValueType]
    val matched = mutable.Set.empty[ValueType]
    // references used as blocks escape and keep the original representation
    val blockVars = mutable.Set.empty[Id]

    visitAll(m.definitions) {
      case Expr.PureApp(BlockVar(f, _, _), targs, _) if externs.contains(f) =>
        callSites += CallSite(externs(f), targs, false)
      case Stmt.ImpureApp(_, BlockVar(f, _, _), targs, _, _, _) if externs.contains(f) =>
        callSites += CallSite(externs(f), targs, false)
      case Stmt.App(BlockVar(f, _, _), targs, _, _) if externs.contains(f) =>
        callSites += CallSite(externs(f), targs, true)
      case Stmt.Match(scrutinee, _, _, _) => matched += scrutinee.tpe
      case BlockType.Interface(_, targs) => interfaceArguments ++= targs
      case BlockVar(id, _, _) => blockVars += id
      case _ => ()
    }

    val frozen: Set[ValueType] =
      (callSites.filterNot(site => !site.control && componentwise(site.f)).flatMap(boundary) ++ interfaceArguments)
        .flatMap(occurrences).filter(isContainer).toSet

    /** The element type, if the call can be translated componentwise. */
    def componentwiseElement(f: Extern.Def, targs: List[ValueType]): Option[ValueType] = targs match {
      case List(u) if componentwise(f) =>
        val instances = (f.ret :: f.vparams.map(_.tpe)).collect {
          case ValueType.Data(a, List(ValueType.Var(_))) if containers.contains(a) => ValueType.Data(a, List(u))
        }
        if instances.exists(frozen.contains) then None else Some(u)
      case _ => None
    }

    val opaque: List[ValueType] =
      callSites.toList.filter(site => site.control || componentwiseElement(site.f, site.targs).isEmpty).flatMap(boundary) ++
        interfaceArguments

    val records: Map[Id, Constructor] = m.declarations.collect {
      case Declaration.Data(id, Nil, List(c @ Constructor(_, Nil, fields))) if fields.nonEmpty => id -> c
    }.toMap

    val dependencies: Map[Id, Set[Id]] = m.declarations.collect {
      case Declaration.Data(id, _, constructors) =>
        id -> constructors.flatMap(_.fields).flatMap(f => occurrences(f.tpe)).collect { case ValueType.Data(d, _) => d }.toSet
    }.toMap

    def recursive(id: Id): Boolean = {
      var seen: Set[Id] = Set.empty
      def reach(from: Id): Unit = dependencies.getOrElse(from, Set.empty).foreach { to =>
        if !seen.contains(to) then { seen += to; reach(to) }
      }
      reach(id)
      seen.contains(id)
    }

    val excluded: Set[Id] = opaque.flatMap(underFunctions).collect { case ValueType.Data(id, _) => id }.toSet

    val flat: Set[Id] = records.keySet.filter { id =>
      matched.contains(ValueType.Data(id, Nil)) && !excluded.contains(id) && !recursive(id)
    }

    def isFlat(tpe: ValueType): Boolean = tpe match {
      case ValueType.Data(id, Nil) => flat.contains(id)
      case _ => false
    }

    // Representation
    // --------------
    def repr(tpe: ValueType): List[ValueType] = tpe match {
      case ValueType.Data(id, Nil) if flat.contains(id) => records(id).fields.flatMap(f => repr(f.tpe))
      case ValueType.Data(a, List(t)) if containers.contains(a) && !frozen.contains(tpe) =>
        repr(t).map(u => ValueType.Data(a, List(u)))
      case ValueType.Boxed(b, capt) => List(ValueType.Boxed(repr(b), capt))
      case other => List(other)
    }

    def repr(tpe: BlockType): BlockType = tpe match {
      case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
        BlockType.Function(tparams, cparams, vparams.flatMap(repr), bparams.map(repr), result.flatMap(repr))
      case BlockType.Interface(_, _) => tpe
      case BlockType.Prompt(answer) => BlockType.Prompt(answer.flatMap(repr))
      case BlockType.Resume(result, answer) => BlockType.Resume(result.flatMap(repr), answer.flatMap(repr))
    }

    def binders(id: Id, tpe: ValueType): List[ValueParam] = repr(tpe) match {
      case List(t) => List(ValueParam(id, t))
      case tpes => tpes.zipWithIndex.map { case (t, i) => ValueParam(Id(id.name.rename(n => s"${n}_${i}")), t) }
    }

    def fields(f: Field): List[Field] = binders(f.id, f.tpe).map(p => Field(p.id, p.tpe))

    def single(es: List[Expr]): Expr = es match {
      case List(e) => e
      case _ => INTERNAL_ERROR(s"Expected a single component, but got ${es.size}")
    }

    /** The original representation of a value, given its components. */
    def pack(tpe: ValueType, es: List[Expr]): Expr =
      if isFlat(tpe) then Make(tpe.asInstanceOf[ValueType.Data], records(tpe.asInstanceOf[ValueType.Data].name).id, Nil, es)
      else single(es)

    /** The components of a value in the original representation. */
    def open(e: Expr, tpe: ValueType): Bind[List[Expr]] =
      if isFlat(tpe) then {
        val ValueType.Data(id, _) = tpe: @unchecked
        val params = records(id).fields.flatMap(f => binders(Id(f.id), f.tpe))
        val results = params.map(p => ValueParam(Id(p.id), p.tpe))
        val clause: BlockLit = BlockLit(Nil, Nil, params, Nil, Return(params.map(p => ValueVar(p.id, p.tpe))))
        Bind(results.map(p => ValueVar(p.id, p.tpe)),
          List(Binding.Val(results.map(_.id), Match(e, results.map(_.tpe), List(records(id).id -> clause), None))))
      } else Bind.pure(List(e))

    def lets(params: List[ValueParam], es: List[Expr], body: Stmt): Stmt = {
      util.assert(params.size == es.size, "Wrong number of components")
      params.zip(es).foldRight(body) { case ((p, e), rest) => Let(p.id, e, rest) }
    }

    // Terms
    // -----
    def expr(e: Expr)(using env: Env): Bind[List[Expr]] = e match {
      case ValueVar(id, tpe) => env.values.get(id) match {
        case Some(es) => Bind.pure(es)
        // toplevel values keep the original representation
        case None => open(e, tpe)
      }
      case l: Literal => Bind.pure(List(l))
      case PureApp(f, targs, vargs) => pureCall(f, targs, vargs)
      case Make(data, tag, targs, vargs) => exprs(vargs).map { es =>
        if isFlat(data) then es else List(Make(data, tag, targs, es))
      }
      case Box(b, capt) => Bind.pure(List(Box(block(b), capt)))
    }

    def exprs(es: List[Expr])(using Env): Bind[List[Expr]] = Bind.traverse(es)(expr).map(_.flatten)

    // Extern calls
    // ------------
    def packed(f: Extern.Def, targs: List[ValueType], vargs: List[Expr])(using Env): Bind[List[Expr]] =
      Bind.traverse(vargs)(expr).map { args =>
        args.zip(signature(f, targs).vparams).map { case (es, tpe) => pack(tpe, es) }
      }

    def result(f: Extern.Def, targs: List[ValueType]): ValueType = Type.single(signature(f, targs).result)

    /** Arguments of the j-th componentwise call. */
    def componentArguments(f: Extern.Def, args: List[List[Expr]], n: Int): List[List[Expr]] =
      List.tabulate(n) { j =>
        f.vparams.zip(args).map { case (p, es) =>
          if mentions(p.tpe, f.tparams.head) then es(j) else pack(p.tpe, es)
        }
      }

    def pureCall(callee: BlockVar, targs: List[ValueType], vargs: List[Expr])(using Env): Bind[List[Expr]] = {
      val f = externs(callee.id)
      componentwiseElement(f, targs) match {
        case Some(u) => Bind.traverse(vargs)(expr).map { args =>
          val calls = componentArguments(f, args, repr(u).size).zip(repr(u)).map { case (as, t) => PureApp(callee, List(t), as) }
          if mentions(f.ret, f.tparams.head) then calls else calls.take(1)
        }
        case None => packed(f, targs, vargs).flatMap { as => open(PureApp(callee, targs, as), result(f, targs)) }
      }
    }

    def impureCall(callee: BlockVar, targs: List[ValueType], vargs: List[Expr], bargs: List[Block])(using Env): Bind[List[Expr]] = {
      val f = externs(callee.id)
      componentwiseElement(f, targs) match {
        case Some(u) => Bind.traverse(vargs)(expr).flatMap { args =>
          val calls = componentArguments(f, args, repr(u).size).zip(repr(u)).map { case (as, t) => Bind.bind(callee, List(t), as, Nil) }
          Bind.traverse(calls)(identity).map { results =>
            if mentions(f.ret, f.tparams.head) then results else List(results.last)
          }
        }
        case None => packed(f, targs, vargs).flatMap { as =>
          Bind.bind(callee, targs, as, bargs.map(block)).flatMap { v => open(v, result(f, targs)) }
        }
      }
    }

    // Statements
    // ----------
    def stmt(s: Stmt)(using env: Env): Stmt = s match {
      case Stmt.Def(id, b, body) => Stmt.Def(id, block(b), stmt(body))

      case Stmt.Let(id, e, body) => expr(e).run { es =>
        val params = binders(id, e.tpe)
        lets(params, es, stmt(body)(using env.bind(id, params)))
      }

      case s @ Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => impureCall(callee, targs, vargs, bargs).run { es =>
        val params = binders(id, Type.bindingType(s))
        lets(params, es, stmt(body)(using env.bind(id, params)))
      }

      case Stmt.Return(es) => exprs(es).run(Return(_))

      case Stmt.Val(ids, binding, body) =>
        val params = ids.zip(binding.tpe).map(binders)
        Stmt.Val(params.flatten.map(_.id), stmt(binding), stmt(body)(using env.bind(ids, params)))

      case Stmt.App(callee: BlockVar, targs, vargs, bargs) if externs.contains(callee.id) =>
        val f = externs(callee.id)
        packed(f, targs, vargs).run { as =>
          val tmp = Id("tmp")
          val tpe = result(f, targs)
          Stmt.Val(List(tmp), Stmt.App(callee, targs, as, bargs.map(block)), open(ValueVar(tmp, tpe), tpe).run(Return(_)))
        }

      case Stmt.App(callee, targs, vargs, bargs) => exprs(vargs).run { vs =>
        Stmt.App(block(callee), targs, vs, bargs.map(block))
      }

      case Stmt.Invoke(callee, method, tpe, targs, vargs, bargs) => exprs(vargs).run { vs =>
        Stmt.Invoke(block(callee), method, repr(tpe), targs, vs, bargs.map(block))
      }

      case Stmt.If(cond, thn, els) => expr(cond).run { cs => Stmt.If(single(cs), stmt(thn), stmt(els)) }

      // the scrutinee is already open: bind its components to the clause parameters
      case Stmt.Match(scrutinee, _, clauses, default) if isFlat(scrutinee.tpe) => expr(scrutinee).run { cs =>
        clauses match {
          case List((_, BlockLit(_, _, vparams, _, body))) =>
            val params = vparams.map(p => binders(p.id, p.tpe))
            lets(params.flatten, cs, stmt(body)(using env.bind(vparams.map(_.id), params)))
          case _ => stmt(default.getOrElse { INTERNAL_ERROR("Match on a record without clauses") })
        }
      }

      case Stmt.Match(scrutinee, tpes, clauses, default) => expr(scrutinee).run { cs =>
        Stmt.Match(single(cs), tpes.flatMap(repr), clauses.map { case (c, b) => (c, blockLit(b)) }, default.map(stmt))
      }

      case Stmt.Region(b) => Stmt.Region(blockLit(b))

      case Stmt.Alloc(id, init, region, body) => expr(init).run { es =>
        if blockVars.contains(id) then Stmt.Alloc(id, pack(init.tpe, es), region, stmt(body))
        else {
          val refs = references(id, es.size)
          refs.zip(es).foldRight(stmt(body)(using env.bindRef(id, refs))) { case ((r, e), rest) => Stmt.Alloc(r, e, region, rest) }
        }
      }

      // all components share the capture of the original variable
      case Stmt.Var(ref, init, capture, body) => expr(init).run { es =>
        if blockVars.contains(ref) then Stmt.Var(ref, pack(init.tpe, es), capture, stmt(body))
        else {
          val refs = references(ref, es.size)
          refs.zip(es).foldRight(stmt(body)(using env.bindRef(ref, refs))) { case ((r, e), rest) => Stmt.Var(r, e, capture, rest) }
        }
      }

      case Stmt.Get(id, tpe, ref, capt, body) =>
        val params = binders(id, tpe)
        val rest = stmt(body)(using env.bind(id, params))
        env.refs.get(ref) match {
          case Some(refs) => params.zip(refs).foldRight(rest) { case ((p, r), rest) => Stmt.Get(p.id, p.tpe, r, capt, rest) }
          // references that escape keep the original representation
          case None =>
            val tmp = Id(id)
            Stmt.Get(tmp, tpe, ref, capt, open(ValueVar(tmp, tpe), tpe).run { es => lets(params, es, rest) })
        }

      case Stmt.Put(ref, capt, value, body) => expr(value).run { es =>
        env.refs.get(ref) match {
          case Some(refs) => refs.zip(es).foldRight(stmt(body)) { case ((r, e), rest) => Stmt.Put(r, capt, e, rest) }
          case None => Stmt.Put(ref, capt, pack(value.tpe, es), stmt(body))
        }
      }

      case Stmt.Reset(b) => Stmt.Reset(blockLit(b))
      case Stmt.Shift(prompt, k, body) => Stmt.Shift(blockVar(prompt), blockParam(k), stmt(body))
      case Stmt.Resume(k, body) => Stmt.Resume(blockVar(k), stmt(body))
      case Stmt.Hole(tpes, span) => Stmt.Hole(tpes.flatMap(repr), span)
    }

    def references(id: Id, n: Int): List[Id] =
      if n == 1 then List(id) else List.tabulate(n) { i => Id(id.name.rename(name => s"${name}_${i}")) }

    // Blocks
    // ------
    def block(b: Block)(using Env): Block = b match {
      case b: BlockVar => blockVar(b)
      case b: BlockLit => blockLit(b)
      case Unbox(e) => expr(e) match {
        case Bind(List(e1), Nil) => Unbox(e1)
        case _ => INTERNAL_ERROR("Expected an atomic expression in unbox")
      }
      case New(Implementation(interface, operations)) => New(Implementation(interface, operations.map(operation)))
    }

    def blockVar(b: BlockVar): BlockVar =
      if externs.contains(b.id) then b else BlockVar(b.id, repr(b.annotatedTpe), b.annotatedCapt)

    def blockParam(p: BlockParam): BlockParam = BlockParam(p.id, repr(p.tpe), p.capt)

    def blockLit(b: BlockLit)(using env: Env): BlockLit = b match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        val params = vparams.map(p => binders(p.id, p.tpe))
        BlockLit(tparams, cparams, params.flatten, bparams.map(blockParam), stmt(body)(using env.bind(vparams.map(_.id), params)))
    }

    def operation(op: Operation)(using env: Env): Operation = op match {
      case Operation(name, tparams, cparams, vparams, bparams, body) =>
        val params = vparams.map(p => binders(p.id, p.tpe))
        Operation(name, tparams, cparams, params.flatten, bparams.map(blockParam), stmt(body)(using env.bind(vparams.map(_.id), params)))
    }

    // Module
    // ------
    def declaration(d: Declaration): Declaration = d match {
      case Declaration.Data(id, tparams, constructors) =>
        Declaration.Data(id, tparams, constructors.map { c => Constructor(c.id, c.tparams, c.fields.flatMap(fields)) })
      case Declaration.Interface(id, tparams, properties) =>
        Declaration.Interface(id, tparams, properties.map { p => Property(p.id, repr(p.tpe)) })
    }

    // Toplevel values with a single component are translated; others keep the original representation.
    val toplevel: Env = Env(m.definitions.collect {
      case Toplevel.Val(id, binding) if repr(Type.single(binding.tpe)).size == 1 =>
        id -> List(ValueVar(id, repr(Type.single(binding.tpe)).head))
    }.toMap, Map.empty)

    def definition(d: Toplevel): Toplevel = d match {
      case Toplevel.Def(id, b) => Toplevel.Def(id, block(b)(using toplevel))
      case Toplevel.Val(id, binding) if toplevel.values.contains(id) => Toplevel.Val(id, stmt(binding)(using toplevel))
      case Toplevel.Val(id, binding) =>
        val tpe = Type.single(binding.tpe)
        val params = repr(tpe).map(t => ValueParam(Id(id), t))
        Toplevel.Val(id, Stmt.Val(params.map(_.id), stmt(binding)(using toplevel),
          Return(List(pack(tpe, params.map(p => ValueVar(p.id, p.tpe)))))))
    }

    def module: ModuleDecl = m match {
      case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
        ModuleDecl(path, includes, declarations.map(declaration), externs, definitions.map(definition), exports)
    }
  }
}
