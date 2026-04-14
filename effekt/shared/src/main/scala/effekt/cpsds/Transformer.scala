package effekt
package cpsds

import core.{ BlockType, Captures, Id, ValueType }
import core.optimizer.Usage

// Only maps Ids to Ids for dealiasing
case class TransformationContext(
  renamings: Map[Id, Id] = Map.empty
) {
  def lookup(id: Id): Id = renamings.getOrElse(id, id)
  def alias(from: Id, to: Id): TransformationContext = copy(renamings = renamings + (from -> lookup(to)))
}


// ---------- Continuation / MetaContinuation ----------

enum Continuation {
  case Dynamic(id: Id)
  case Static(hint: Id, k: (Expr, MetaContinuation) => Stmt)

  def apply(arg: Expr, ks: MetaContinuation): Stmt = this match {
    case Dynamic(id) => Stmt.App(id, List(arg, ks.reify), false)
    case Static(_, k) => k(arg, ks)
  }

  def reify(tpe: ValueType, body: Expr => Stmt): Stmt =
    if tpe == core.Type.TBottom then body(Expr.Abort)
    else this match {
      case Dynamic(id) => body(Expr.Variable(id))
      case Static(hint, k) =>
        val name = Id("k")
        val param = Id(hint)
        val ks = Id("ks")
        Stmt.Def(name, List(param, ks),
          k(Expr.Variable(param), MetaContinuation.Dynamic(ks)),
          body(Expr.Variable(name)))
    }
}
object Continuation {
  def Static(hint: Id)(k: (Expr, MetaContinuation) => Stmt): Continuation.Static =
    Continuation.Static(hint, k)
}

enum MetaContinuation {
  case Dynamic(id: Id)
  case Toplevel

  def reify: MetaCont = this match {
    case Dynamic(id) => Expr.Variable(id)
    case Toplevel    => Expr.Toplevel
  }
}

def withJoinpoint(k: Continuation)(body: Continuation => Stmt): Stmt = k match {
  case k: Continuation.Dynamic => body(k)
  case k: Continuation.Static =>
    k.reify(core.Type.TTop, {
      case Expr.Variable(id) => body(Continuation.Dynamic(id))
      case _ => sys.error("Expected variable after reify")
    })
}


// ---------- Expression / Block transforms (Bind monad) ----------

def transform(expr: core.Expr)(using C: TransformationContext): Bind[Expr] = expr match {
  case core.Expr.ValueVar(id, _) =>
    Bind.pure(Expr.Variable(C.lookup(id)))

  case core.Expr.Literal(value, tpe) =>
    Bind.pure(Expr.Literal(value, tpe))

  case core.Expr.PureApp(callee, _, vargs) =>
    for {
      f <- transform(callee)
      vs <- Bind.traverse(vargs)(transform)
      res <- f match {
        case Expr.Variable(fId) => Bind.run(fId, vs, Purity.Pure)
        case _ => sys.error("PureApp callee should resolve to a variable")
      }
    } yield res

  case core.Expr.Make(data, tag, _, vargs) =>
    for {
      vs <- Bind.traverse(vargs)(transform)
      res <- Bind.let(Expr.Make(data, tag, vs))
    } yield res

  case core.Expr.Box(b, _) =>
    transform(b)
}

def transform(block: core.Block)(using C: TransformationContext): Bind[Expr] = block match {
  case core.Block.BlockVar(id, _, _) =>
    Bind.pure(Expr.Variable(C.lookup(id)))

  case core.Block.BlockLit(_, _, vparams, bparams, body) =>
    val ks1 = Id("ks")
    val k1 = Id("k")
    val params = vparams.map(_.id) ++ bparams.map(_.id) ++ List(ks1, k1)
    Bind.define(params, transform(body, MetaContinuation.Dynamic(ks1), Continuation.Dynamic(k1)))

  case core.Block.Unbox(pure) =>
    transform(pure)

  case core.Block.New(impl) =>
    Bind.makeNew(impl.interface, transformOperations(impl))
}

/**
 * Transform a block and extract the Id.
 * Returns a Bind so that Defs/News are accumulated as bindings.
 */
def transformBlockId(block: core.Block)(using C: TransformationContext): Bind[Id] =
  transform(block).map {
    case Expr.Variable(id) => id
    case other => sys.error(s"Expected variable, got $other")
  }


// ---------- Helpers ----------

def transformOperations(impl: core.Implementation)(using C: TransformationContext): List[Operation] =
  impl.operations.map { case core.Operation(name, _, _, vparams, bparams, opBody) =>
    val ks1 = Id("ks")
    val k1 = Id("k")
    Operation(name, vparams.map(_.id) ++ bparams.map(_.id) ++ List(ks1, k1),
      transform(opBody, MetaContinuation.Dynamic(ks1), Continuation.Dynamic(k1)))
  }

def transformClause(clause: core.Block.BlockLit, ks: MetaContinuation, k: Continuation)(using C: TransformationContext): Clause =
  clause match {
    case core.Block.BlockLit(_, _, vparams, bparams, body) =>
      Clause(vparams.map(_.id) ++ bparams.map(_.id), transform(body, ks, k))
  }


// ---------- Statement transform ----------

def transform(stmt: core.Stmt, ks: MetaContinuation, k: Continuation)(using C: TransformationContext): Stmt = stmt match {

  // --- Return ---
  case core.Stmt.Return(expr) =>
    transform(expr).run { v => k(v, ks) }

  // --- Val (sequencing) ---
  case core.Stmt.Val(id, rhs, body) =>
    transform(rhs, ks, Continuation.Static(id) { (value, ks) =>
      value match {
        case Expr.Variable(x) =>
          given ctx: TransformationContext = C.alias(id, x)
          transform(body, ks, k)
        case other =>
          Stmt.Let(id, other, transform(body, ks, k))
      }
    })

  // --- Let (pure binding) ---
  case core.Stmt.Let(id, binding, body) => binding match {
    // Dealiasing
    case core.Expr.ValueVar(y, _) =>
      given ctx: TransformationContext = C.alias(id, C.lookup(y))
      transform(body, ks, k)

    case core.Expr.Literal(value, tpe) =>
      Stmt.Let(id, Expr.Literal(value, tpe), transform(body, ks, k))

    case core.Expr.PureApp(callee, _, vargs) =>
      (for {
        f <- transformBlockId(callee)
        vs <- Bind.traverse(vargs)(transform)
      } yield (f, vs)).run { case (fId, vs) =>
        Stmt.Run(id, fId, vs, Purity.Pure, transform(body, ks, k))
      }

    case core.Expr.Make(data, tag, _, vargs) =>
      Bind.traverse(vargs)(transform).run { vs =>
        Stmt.Let(id, Expr.Make(data, tag, vs), transform(body, ks, k))
      }

    case core.Expr.Box(b, _) =>
      transform(b).run {
        case Expr.Variable(x) =>
          given ctx: TransformationContext = C.alias(id, x)

          transform(body, ks, k)
        case other =>
          Stmt.Let(id, other, transform(body, ks, k))
      }
  }

  // --- ImpureApp ---
  case core.Stmt.ImpureApp(id, callee, _, vargs, bargs, body) =>
    (for {
      f <- transformBlockId(callee)
      vs <- Bind.traverse(vargs)(transform)
      bs <- Bind.traverse(bargs)(transform)
    } yield (f, vs ++ bs)).run { case (fId, args) =>
      Stmt.Run(id, fId, args, Purity.Impure, transform(body, ks, k))
    }

  // --- Def (block definition) ---
  // Dealiasing: def f = g
  case core.Stmt.Def(id, core.Block.BlockVar(x, _, _), body) =>
    given ctx: TransformationContext = C.alias(id, C.lookup(x))
    transform(body, ks, k)

  case core.Stmt.Def(id, core.Block.BlockLit(_, _, vparams, bparams, body), rest) =>
    val ks1 = Id("ks")
    val k1 = Id("k")
    val params = vparams.map(_.id) ++ bparams.map(_.id) ++ List(ks1, k1)
    Stmt.Def(id, params,
      transform(body, MetaContinuation.Dynamic(ks1), Continuation.Dynamic(k1)),
      transform(rest, ks, k))

  case core.Stmt.Def(id, core.Block.New(impl), body) =>
    val ops = transformOperations(impl)
    Stmt.New(id, impl.interface, ops, transform(body, ks, k))

  case core.Stmt.Def(id, core.Block.Unbox(pure), body) =>
    transform(pure).run {
      case Expr.Variable(x) =>
        given ctx: TransformationContext = C.alias(id, x)

        transform(body, ks, k)
      case other =>
        Stmt.Let(id, other, transform(body, ks, k))
    }

  // --- App ---
  case core.Stmt.App(callee, _, vargs, bargs) =>
    (for {
      calleeId <- transformBlockId(callee)
      vs <- Bind.traverse(vargs)(transform)
      bs <- Bind.traverse(bargs)(transform)
    } yield (calleeId, vs ++ bs)).run { case (calleeId, args) =>
      k.reify(stmt.tpe, cont =>
        Stmt.App(calleeId, args ++ List(ks.reify, cont), canBeDirect(stmt.capt)))
    }

  // --- Invoke ---
  case core.Stmt.Invoke(callee, method, _, _, vargs, bargs) =>
    (for {
      calleeId <- transformBlockId(callee)
      vs <- Bind.traverse(vargs)(transform)
      bs <- Bind.traverse(bargs)(transform)
    } yield (calleeId, vs ++ bs)).run { case (calleeId, args) =>
      k.reify(stmt.tpe, cont =>
        Stmt.Invoke(calleeId, method, args ++ List(ks.reify, cont)))
    }

  // --- If ---
  case core.Stmt.If(cond, thn, els) =>
    transform(cond).run { c =>
      withJoinpoint(k) { k2 =>
        Stmt.If(c, transform(thn, ks, k2), transform(els, ks, k2))
      }
    }

  // --- Match ---
  case core.Stmt.Match(scrutinee, _, List((id, rhs)), None) =>
    transform(scrutinee).run { sc =>
      Stmt.Match(sc, List((id, transformClause(rhs, ks, k))), None)
    }

  case core.Stmt.Match(scrutinee, _, clauses, default) =>
    transform(scrutinee).run { sc =>
      withJoinpoint(k) { k2 =>
        Stmt.Match(sc,
          clauses.map { case (id, rhs) => (id, transformClause(rhs, ks, k2)) },
          default.map(transform(_, ks, k2)))
      }
    }

  // --- Region ---
  case core.Stmt.Region(core.Block.BlockLit(_, _, _, List(region), body)) =>
    Stmt.Region(region.id, ks.reify,
      transform(body, ks,
        Continuation.Static(Id("tmp")) { (x, ks) =>
          Stmt.Dealloc(region.id, k(x, ks))
        }))

  case core.Stmt.Region(_) => sys.error("Region should have a single-param BlockLit")

  // --- Alloc ---
  case core.Stmt.Alloc(id, init, region, body) =>
    transform(init).run { v =>
      Stmt.Alloc(id, v, region, transform(body, ks, k))
    }

  // --- Var ---
  case core.Stmt.Var(ref, init, capture, body) =>
    transform(init).run { v =>
      Stmt.Var(ref, v, ks.reify,
        transform(body, ks,
          Continuation.Static(Id("tmp")) { (x, ks) =>
            Stmt.Dealloc(ref, k(x, ks))
          }))
    }

  // --- Get ---
  case core.Stmt.Get(id, _, ref, _, body) =>
    Stmt.Get(ref, id, transform(body, ks, k))

  // --- Put ---
  case core.Stmt.Put(ref, _, value, body) =>
    transform(value).run { v =>
      Stmt.Put(ref, v, transform(body, ks, k))
    }

  // --- Reset ---
  case core.Stmt.Reset(core.Block.BlockLit(_, _, _, List(prompt), body)) =>
    val ks2 = Id("ks")
    val k2 = Id("k")
    k.reify(stmt.tpe, cont =>
      Stmt.Reset(prompt.id, ks2, k2,
        transform(body, MetaContinuation.Dynamic(ks2), Continuation.Dynamic(k2)),
        ks.reify, cont))

  case core.Stmt.Reset(_) => sys.error("Reset should have single-param BlockLit")

  // --- Shift ---
  case core.Stmt.Shift(prompt, resume, body) =>
    val ks2 = Id("ks")
    val k2 = Id("k")
    k.reify(stmt.tpe, cont =>
      Stmt.Shift(prompt.id, resume.id, ks2, k2,
        transform(body, MetaContinuation.Dynamic(ks2), Continuation.Dynamic(k2)),
        ks.reify, cont))

  // --- Resume ---
  case core.Stmt.Resume(cont, body) =>
    val ks2 = Id("ks")
    val k2 = Id("k")
    k.reify(stmt.tpe, cont2 =>
      Stmt.Resume(cont.id, ks2, k2,
        transform(body, MetaContinuation.Dynamic(ks2), Continuation.Dynamic(k2)),
        ks.reify, cont2))

  // --- Hole ---
  case core.Stmt.Hole(_, span) => Stmt.Hole(span)
}


// ---------- Module-level ----------

def transform(module: core.ModuleDecl): ModuleDecl = module match {
  case core.ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
    val entrypoints = exports.toSet ++ definitions.collect {
      case core.Toplevel.Val(id, _) => id
    }
    given TransformationContext = TransformationContext()
    ModuleDecl(path, includes, declarations, externs.flatMap(transformExtern),
      definitions.map(transformToplevel), exports)
}

private def canBeDirect(captures: Captures)(using C: TransformationContext): Boolean =
  //def nonRecursive = C.reachability.get(id).exists(u => u != Usage.Recursive)
  def noControl = (captures -- Set(symbols.builtins.IOCapability.capture, symbols.builtins.GlobalCapability.capture)).isEmpty
  noControl

def transformToplevel(definition: core.Toplevel)(using C: TransformationContext): ToplevelDefinition = definition match {
  case core.Toplevel.Def(id, core.Block.BlockLit(_, _, vparams, bparams, body)) =>
    val ks = Id("ks")
    val k = Id("k")
    ToplevelDefinition.Def(id, vparams.map(_.id) ++ bparams.map(_.id) ++ List(ks, k),
      transform(body, MetaContinuation.Dynamic(ks), Continuation.Dynamic(k)))

  case core.Toplevel.Def(id, block) =>
    transform(block) match {
      case Bind(Expr.Variable(x), Nil) =>
        ToplevelDefinition.Let(id, Expr.Variable(x))
      case Bind(value, bindings) =>
        val ks = Id("ks")
        val k = Id("k")
        ToplevelDefinition.Def(id, List(ks, k),
          Binding(bindings, Stmt.App(k, List(value, Expr.Variable(ks)), false)))
    }

  case core.Toplevel.Val(id, binding) =>
    val ks = Id("ks")
    val k = Id("k")
    ToplevelDefinition.Val(id, ks, k,
      transform(binding, MetaContinuation.Dynamic(ks), Continuation.Dynamic(k)))
}

def transformExtern(extern: core.Extern)(using C: TransformationContext): Option[Extern] = extern match {
  case core.Extern.Def(id, _, _, vparams, bparams, _, annotatedCapture, body) =>
    val isAsync = annotatedCapture.contains(symbols.builtins.AsyncCapability.capture)
    Some(Extern.Def(id, vparams.map(_.id) ++ bparams.map(_.id), isAsync, transformExternBody(body)))
  case core.Extern.Include(featureFlag, contents) =>
    Some(Extern.Include(featureFlag, contents))
  case core.Extern.Data(_, _) => None
}

def transformExternBody(body: core.ExternBody)(using C: TransformationContext): ExternBody = body match {
  case core.ExternBody.StringExternBody(featureFlag, Template(strings, args)) =>
    // Extern template args should be atomic (ValueVar or BlockVar after elaboration).
    // We can use the Bind monad but expect no actual bindings.
    val transformedArgs = args.map { arg =>
      val Bind(value, bindings) = transform(arg)
      assert(bindings.isEmpty, s"Extern body args should be atomic, got bindings for $arg")
      value
    }
    ExternBody.StringExternBody(featureFlag, Template(strings, transformedArgs))
  case core.ExternBody.Unsupported(err) => ExternBody.Unsupported(err)
}
