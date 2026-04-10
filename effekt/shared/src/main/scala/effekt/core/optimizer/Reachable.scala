package effekt
package core
package optimizer

/**
 * A simple reachability analysis.
 */
class Reachable(
  var reachable: Map[Id, Usage],
  var stack: List[Id],
  var seen: Set[Id]
) {

  // TODO we could use [[Binding]] here.
  type Definitions = Map[Id, Block | Expr | Stmt | Extern.Def | Extern.Data | Declaration | (Property, Declaration.Interface)]

  private def update(id: Id, u: Usage): Unit = reachable = reachable.updated(id, u)
  private def usage(id: Id): Usage = reachable.getOrElse(id, Usage.Never)

  def processDefinition(id: Id, d: Block | Expr | Stmt | Extern.Def | Extern.Data | Declaration | (Property, Declaration.Interface))(using defs: Definitions): Unit = {
    if stack.contains(id) then { update(id, Usage.Recursive); return }

    seen = seen + id

    d match {
      case block: Block =>

        val before = stack
        stack = id :: stack

        process(block)
        stack = before

      case expr: Expr => process(expr)
      case binding: Stmt => process(binding)
      case extern: Extern.Def => process(extern)
      case extern: Extern.Data => process(extern)
      case decl: Declaration => process(decl)
      case (p: Property, d: Declaration.Interface) =>
        process(p.tpe); process(d.id)
    }
  }

  def process(id: Id)(using defs: Definitions): Unit =
    if (stack.contains(id)) {
      update(id, Usage.Recursive)
      return;
    }

    update(id, usage(id) + Usage.Once)

    if (!seen.contains(id)) {
      seen = seen + id
      defs.get(id).foreach { d => processDefinition(id, d) }
    }


  def process(b: Block)(using defs: Definitions): Unit =
    b match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
        process(annotatedTpe); process(id)
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
        vparams.foreach(process); bparams.foreach(process); process(body)
      case Block.Unbox(expr) => process(expr)
      case Block.New(impl) => process(impl)
    }

  def process(e: Extern.Def)(using defs: Definitions): Unit =
    e match {
      case Extern.Def(_, tps, cps, vps, bps, ret, capts, body) =>
        vps.foreach(process)
        bps.foreach(process)
        process(ret)
        body match {
          case ExternBody.StringExternBody(_, Template(_, args)) =>
            args.foreach(process)
          case effekt.core.ExternBody.Unsupported(_) => ()
        }
    }

  def process(p: ValueParam)(using defs: Definitions): Unit = process(p.tpe)
  def process(p: BlockParam)(using defs: Definitions): Unit = process(p.tpe)

  def process(e: Extern.Data)(using defs: Definitions): Unit = ()
  def process(d: Declaration)(using defs: Definitions): Unit =
    d match {
      case Declaration.Data(id, tparams, constructors) =>
        constructors.foreach {
          case Constructor(id, tparams, fields) =>
            fields.foreach {
              case Field(id, tpe) => process(tpe)
            }
        }
      case Declaration.Interface(id, tparams, properties) =>
        properties.foreach {
          case Property(id, tpe) => process(tpe)
        }
    }

  def process(t: ValueType)(using defs: Definitions): Unit =
    t match {
      case ValueType.Var(name) => ()
      case ValueType.Data(name, targs) => process(name); targs.foreach(process)
      case ValueType.Boxed(tpe, capt) => process(tpe)
    }
  def process(t: BlockType)(using defs: Definitions): Unit =
    t match {
      case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
        vparams.foreach(process)
        bparams.foreach(process)
        process(result)
      case BlockType.Interface(name, targs) => process(name); targs.foreach(process)
    }

  def process(s: Stmt)(using defs: Definitions): Unit = s match {
    case Stmt.Def(id, block, body) =>
      // Do NOT process `block` here, since this would mean the definition is used
      process(body)(using defs + (id -> block))
    case Stmt.Let(id, binding, body) =>
      // We would need to process the binding if it was impure,
      // to keep it for its side effects; however, the binding is guaranteed to be pure
      process(body)(using defs + (id -> binding))
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      process(callee)
      targs.foreach(process)
      vargs.foreach(process)
      bargs.foreach(process)
      // TODO what to do here?
      // process(body)(using defs + (id -> binding))
      process(body)
    case Stmt.Return(expr) => process(expr)
    case Stmt.Val(id, binding, body) => process(binding); process(body)
    case Stmt.App(callee, targs, vargs, bargs) =>
      process(callee)
      targs.foreach(process)
      vargs.foreach(process)
      bargs.foreach(process)
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      process(callee)
      process(callee.tpe)
      process(method)
      process(methodTpe)
      targs.foreach(process)
      vargs.foreach(process)
      bargs.foreach(process)
    case Stmt.If(cond, thn, els) => process(cond); process(thn); process(els)
    case Stmt.Match(scrutinee, tpe, clauses, default) =>
      process(scrutinee)
      process(tpe)
      clauses.foreach { case (id, value) => process(value) }
      default.foreach(process)
    case Stmt.Alloc(id, init, region, body) =>
      process(init)
      process(region)
      process(body)
    case Stmt.Var(ref, init, capture, body) =>
      process(init)
      process(body)
    case Stmt.Get(ref, tpe, id, capt, body) =>
      process(ref); process(tpe); process(body)
    case Stmt.Put(ref, capt, value, body) =>
      process(ref); process(value); process(body)
    case Stmt.Reset(body) => process(body)
    case Stmt.Shift(prompt, k, body) => process(prompt); process(k); process(body)
    case Stmt.Resume(k, body) => process(k); process(body)
    case Stmt.Region(body) => process(body)
    case Stmt.Hole(tpe, span) => process(tpe)
  }

  def process(e: Expr)(using defs: Definitions): Unit = e match {
    case Expr.ValueVar(id, annotatedType) => process(id); process(annotatedType)
    case Expr.Literal(value, annotatedType) => process(annotatedType)
    case Expr.PureApp(b, targs, vargs) => process(b); targs.foreach(process); vargs.foreach(process)
    case Expr.Make(data, tag, targs, vargs) =>
      process(data); process(tag); targs.foreach(process); vargs.foreach(process)
    case Expr.Box(b, annotatedCapture) => process(b)
  }

  def process(i: Implementation)(using defs: Definitions): Unit = {
    process(i.interface)
    i.operations.foreach {
      case Operation(name, tparams, cparams, vparams, bparams, body) =>
        vparams.foreach(process)
        bparams.foreach(process)
        process(body)
    }
  }
}

object Reachable {
  def apply(entrypoints: Set[Id], m: ModuleDecl): Map[Id, Usage] = {
    val definitions: Map[Id, Block | Expr | Stmt | Extern.Def | Extern.Data | Declaration | (Property, Declaration.Interface)] = (m.definitions.map {
      case Toplevel.Def(id, block) => id -> block
      case Toplevel.Val(id, binding) => id -> binding
    } ++ m.declarations.flatMap { d =>
      (d.id -> d) :: (d match {
        case Declaration.Data(id, tparams, constructors) => Nil
        case d@Declaration.Interface(id, tparams, properties) =>
          properties.map { p => p.id -> (p, d) }
      })
    }
      ++ m.externs.collect {
      case d @ Extern.Def(id, _, _, _, _, _, _, _) => id -> d
      case d @ Extern.Data(id, tps, body) => d.id -> d
    }).toMap
    val initialUsage = entrypoints.map { id => id -> Usage.Recursive }.toMap
    val analysis = new Reachable(initialUsage, Nil, Set.empty)

    entrypoints.foreach(d => analysis.process(d)(using definitions))

    analysis.reachable
  }
}

enum Usage {
  case Never
  case Once
  case Many
  case Recursive

  def +(other: Usage): Usage = (this, other) match {
    case (Usage.Never, other) => other
    case (other, Usage.Never) => other
    case (other, Usage.Recursive) => Usage.Recursive
    case (Usage.Recursive, other) => Usage.Recursive
    case (Usage.Once, Usage.Once) => Usage.Many
    case (Usage.Many, Usage.Many) => Usage.Many
    case (Usage.Many, Usage.Once) => Usage.Many
    case (Usage.Once, Usage.Many) => Usage.Many
  }

  def *(other: Usage): Usage = (this, other) match {
    case (Usage.Never, other) => Usage.Never
    case (other, Usage.Never) => Usage.Never
    case (Usage.Once, other) => other
    case (other, Usage.Once) => other
    case (Usage.Recursive, other) => Usage.Recursive
    case (other, Usage.Recursive) => Usage.Recursive
    case _ => Usage.Many
  }

  // -1
  def decrement: Usage = this match {
    case Usage.Never => Usage.Never
    case Usage.Once => Usage.Never
    case Usage.Many => Usage.Many
    case Usage.Recursive => Usage.Recursive
  }
}
