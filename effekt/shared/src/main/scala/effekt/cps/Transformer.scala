package effekt
package cps

import core.Id
import symbols.builtins.TState

case class TransformationContext(values: Map[Id, Pure], blocks: Map[Id, Block]) {
  def lookupValue(id: Id): Pure = values.getOrElse(id, ValueVar(id))
  def lookupBlock(id: Id): Block = blocks.getOrElse(id, BlockVar(id))
  def bind(id: Id, value: Pure): TransformationContext = copy(values = values + (id -> value))
  def bind(id: Id, block: Block): TransformationContext = copy(blocks = blocks + (id -> block))
}

def binding[R](id: Id, value: Pure)(body: TransformationContext ?=> R)(using C: TransformationContext): R =
  body(using C.bind(id, value))

def binding[R](id: Id, block: Block)(body: TransformationContext ?=> R)(using C: TransformationContext): R =
  body(using C.bind(id, block))


object Transformer {

  def transform(module: core.ModuleDecl): ModuleDecl = module match {
    case core.ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      given TransformationContext(Map.empty, Map.empty)
      ModuleDecl(path, includes, declarations, externs.map(transform), definitions.map(transformToplevel), exports)
  }

  def transformToplevel(definition: core.Definition)(using TransformationContext): ToplevelDefinition = definition match {
    case core.Definition.Def(id, block) => ToplevelDefinition.Def(id, transform(block))
    case core.Definition.Let(id, tpe, core.Run(stmt)) =>
      val ks = Id("ks")
      val k = Id("k")
      ToplevelDefinition.Val(id, ks, k, transform(stmt, ks, Continuation.Dynamic(k)))
    case core.Definition.Let(id, tpe, binding: core.DirectApp) => sys error "Not supported"
    case core.Definition.Let(id, tpe, p: core.Pure) => ToplevelDefinition.Let(id, transform(p))
  }

  def transform(extern: core.Extern)(using TransformationContext): Extern = extern match {
    case core.Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) =>
      Extern.Def(id, vparams.map(_.id), bparams.map(_.id), annotatedCapture.contains(symbols.builtins.AsyncCapability.capture), transform(body))
    case core.Extern.Include(featureFlag, contents) => Extern.Include(featureFlag, contents)
  }

  def transform(externBody: core.ExternBody)(using TransformationContext): ExternBody = externBody match {
    case core.ExternBody.StringExternBody(featureFlag, Template(strings, args)) =>
      ExternBody.StringExternBody(featureFlag, Template(strings, args.map(transform)))
    case core.ExternBody.Unsupported(err) => ExternBody.Unsupported(err)
  }

  def transform(stmt: core.Stmt, ks: Id, k: Continuation)(using TransformationContext): Stmt = stmt match {
    case core.Stmt.Scope(definitions, body) =>

      def go(definitions: List[core.Definition], acc: List[Def], ks: Id, k: Continuation)(using C: TransformationContext): Stmt =
        definitions match {
          case Nil => bindDefs(acc, transform(body, ks, k))

          // dealiasing
          case core.Definition.Def(id, core.BlockVar(x, _, _)) :: xs =>
            binding(id, C.lookupBlock(x)) { go(xs, acc, ks, k) }

          case core.Definition.Def(id, block) :: xs =>
            go(xs, acc :+ Def(id, transform(block)), ks, k)

          case core.Definition.Let(id, tpe, core.Run(stmt)) :: xs =>
            bindDefs(acc, transform(stmt, ks, Continuation.Static(id) { (value, ks) =>
              binding(id, value) { go(xs, Nil, ks, k) }
            }))

          case core.Definition.Let(id, tpe, core.DirectApp(b, targs, vargs, bargs)) :: xs =>
            transform(b) match {
              case Block.BlockVar(f) =>
                bindDefs(acc, LetExpr(id, DirectApp(f, vargs.map(transform), bargs.map(transform)),
                  go(xs, Nil, ks, k)))
              case _ => sys error "Should not happen"
            }

          // dealias
          case core.Definition.Let(id, tpe, core.Pure.ValueVar(x, _)) :: xs =>
            binding(id, C.lookupValue(x)) { go(xs, acc, ks, k) }

          case core.Definition.Let(id, tpe, pure: core.Pure) :: xs =>
            bindDefs(acc, LetExpr(id, transform(pure), go(xs, Nil, ks, k)))
        }

      def bindDefs(acc: List[Def], body: Stmt): Stmt = if acc.isEmpty then body else Scope(acc, body)

      go(definitions, Nil, ks, k)

    case core.Stmt.Return(value) =>
      k(transform(value), ks)

    case core.Stmt.Val(id, annotatedTpe, rhs, body) =>
      transform(rhs, ks, Continuation.Static(id) { (value, ks) =>
        binding(id, value) { transform(body, ks, k) }
      })

    case core.Stmt.App(core.Block.Member(ref: core.Block.BlockVar, TState.get, tpe), Nil, Nil, Nil) =>
      val x = Id("x")
      cps.Get(ref.id, x, k(ValueVar(x), ks))

    case core.Stmt.App(core.Block.Member(ref: core.Block.BlockVar, TState.put, tpe), Nil, List(value), Nil) =>
      cps.Put(ref.id, transform(value), k(cps.Pure.Literal(()), ks))

    case core.Stmt.App(callee, targs, vargs, bargs) => callee match {
      case core.Block.Member(block, field, tpe) =>
        Invoke(transform(block), field, vargs.map(transform), bargs.map(transform), MetaCont(ks), k.reify)

      case _ =>
        App(transform(callee), vargs.map(transform), bargs.map(transform), MetaCont(ks), k.reify)
    }

    case core.Stmt.If(cond, thn, els) =>
      withJoinpoint(k) { k2 =>
        If(transform(cond), transform(thn, ks, k2), transform(els, ks, k2))
      }

    case core.Stmt.Match(scrutinee, clauses, default) =>
      withJoinpoint(k) { k =>
        Match(
          transform(scrutinee),
          clauses.map { case (id, rhs) => (id, transformClause(rhs, ks, k)) },
          default.map(transform(_, ks, k)))
      }

    case core.Stmt.Reset(core.Block.BlockLit(_, _, _, prompt :: Nil, body)) =>
      val ks2 = Id("ks")
      val k2 = Id("k")
      Reset(Block.BlockLit(Nil, List(prompt.id), ks2, k2, transform(body, ks2, Continuation.Dynamic(k2))),
        MetaCont(ks), k.reify)

    case core.Stmt.Reset(body) => sys error "Shouldn't happen"

    // Only unidirectional, yet
    // core.Block.BlockLit(tparams, cparams, vparams, List(resume), body)
    case core.Stmt.Shift(prompt, core.Block.BlockLit(tparams, cparams, vparams, List(resume), body)) =>
      val ks2 = Id("ks")
      val k2 = Id("k")

      val translatedBody: BlockLit = BlockLit(vparams.map { p => p.id }, List(resume.id), ks2, k2,
        transform(body, ks2, Continuation.Dynamic(k2)))

      Shift(prompt.id, translatedBody, MetaCont(ks), k.reify)

    case core.Stmt.Shift(prompt, body) => sys error "Shouldn't happen"

    //
    //
    //      // Effectful method
    //      case (core.Operation(name, tparams, cparams, vparams, bparams, Some(resume), body), Some(prompt)) =>
    //        val ks = Id("ks")
    //        val k = Id("k")
    //
    //        val ks2 = Id("ks")
    //        val k2 = Id("k")
    //
    //        val resumeValue = Id("resumeValue")
    //        val resumeComp = Id("resumeComp")
    //
    //        val (ps, cont) = resume.tpe match {
    //          // bi-directional
    //          // TODO this deep eta-expansion is really ugly but should go away, once we change how block parameters are
    //          //   handled in source and core
    //          case core.BlockType.Function(tparams, cparams, vparams, List(tpe: core.BlockType.Function), result) =>
    //            val ps = tpe.bparams.map {
    //              case tpe : core.BlockType.Interface => Id(tpe.name.toString)
    //              case tpe : core.BlockType.Function => Id(s"f")
    //            }
    //
    //            val ks3 = Id("ks")
    //            val k3 = Id("k")
    //
    //            val ks4 = Id("ks")
    //            val k4 = Id("k")
    //            val c = Id("c")
    //
    //            (ps, BlockLit(Nil, List(c), ks3, k3,
    //              App(BlockVar(resumeComp), Nil, List(BlockLit(Nil, Nil, ks4, k4,
    //                App(BlockVar(c), Nil, ps.map(BlockVar.apply), MetaCont(ks4), Cont.ContVar(k4)))),
    //                  MetaCont(ks3), Cont.ContVar(k3))))
    //
    //          // uni-directional
    //          case _ =>
    //            (Nil, BlockVar(resumeValue))
    //        }
    //
    //        Operation(name, vparams.map(_.id), ps, ks, k,
    //          Shift(prompt,
    //            Block.BlockLit(Nil, List(resumeValue, resumeComp), ks2, k2,
    //              binding(resume.id, cont) {
    //                transform(body, ks2, Continuation.Dynamic(k2))
    //              }),
    //            MetaCont(ks),
    //            Cont.ContVar(k)))

    case core.Stmt.Resume(cont, body) =>
      val ks2 = Id("ks")
      val k2 = Id("k")
      val thunk: BlockLit = Block.BlockLit(Nil, Nil, ks2, k2, transform(body, ks2, Continuation.Dynamic(k2)))
      App(Block.BlockVar(cont.id), Nil, List(thunk), MetaCont(ks), k.reify)

    case core.Stmt.Hole() => Hole()

    case core.Stmt.Region(core.Block.BlockLit(_, _, _, List(region), body)) =>
      cps.Region(region.id, MetaCont(ks),
        transform(body, ks,
          Continuation.Static(Id("tmp")) { (x, ks) =>
            Dealloc(region.id, k(x, ks))
          }))

    case core.Stmt.Region(_) => sys error "Shouldn't happen"

    case core.Stmt.Alloc(id, init, region, body) =>
      cps.Alloc(id, transform(init), region, transform(body, ks, k))

    case core.Stmt.Var(id, init, capture, body) =>
      cps.Var(id, transform(init), MetaCont(ks),
        transform(body, ks,
          Continuation.Static(Id("tmp")) { (x, ks) =>
            Dealloc(id, k(x, ks))
          }))

    case core.Stmt.Get(ref, annotatedCapt, annotatedTpe) =>
      val x = Id("x")
      cps.Get(ref, x, k(ValueVar(x), ks))

    case core.Stmt.Put(ref, annotatedCapt, value) =>
      cps.Put(ref, transform(value), k(cps.Pure.Literal(()), ks))
  }

  def transformClause(clause: core.Block.BlockLit, ks: Id, k: Continuation)(using C: TransformationContext): Clause =
    clause match {
      case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
        Clause(vparams.map(_.id), transform(body, ks, k))
    }

  def transform(impl: core.Implementation)(using C: TransformationContext): Implementation =
    Implementation(impl.interface, impl.operations.map(transform))

  def transform(op: core.Operation)(using C: TransformationContext): Operation =
    op match {
      case core.Operation(name, tparams, cparams, vparams, bparams, _, body) =>
        val ks = Id("ks")
        val k = Id("k")
        Operation(name, vparams.map(_.id), bparams.map(_.id), ks, k,
          transform(body, ks, Continuation.Dynamic(k)))
    }

  def transform(pure: core.Pure)(using C: TransformationContext): Pure = pure match {
    case core.Pure.ValueVar(id, annotatedType) => C.lookupValue(id)
    case core.Pure.Literal(value, annotatedType) => Literal(value)
    case core.Pure.PureApp(b, targs, vargs) => transform(b) match {
      case Block.BlockVar(id) => PureApp(id, vargs.map(transform))
      case _ => sys error "Should not happen"
    }
    case core.Pure.Make(data, tag, vargs) => Make(data, tag, vargs.map(transform))
    case core.Pure.Select(target, field, annotatedType) => Select(transform(target), field)
    case core.Pure.Box(b, annotatedCapture) => Box(transform(b))
  }

  def transformBlockLit(b: core.BlockLit)(using TransformationContext): BlockLit = b match {
    case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      val ks = Id("ks")
      val k = Id("k")
      BlockLit(vparams.map { p => p.id }, bparams.map { p => p.id }, ks, k, transform(body, ks, Continuation.Dynamic(k)))
  }

  def transform(block: core.Block)(using C: TransformationContext): Block = block match {
    case core.Block.BlockVar(id, annotatedTpe, annotatedCapt) => C.lookupBlock(id)
    case b @ core.Block.BlockLit(tparams, cparams, vparams, bparams, body) => transformBlockLit(b)
    case core.Block.Member(block, field, annotatedTpe) => sys error "shouldn't happen"
    case core.Block.Unbox(pure) => Unbox(transform(pure))
    case core.Block.New(impl) => New(transform(impl))
  }
}

def withJoinpoint(k: Continuation)(body: Continuation => Stmt): Stmt = k match {
  case k : Continuation.Dynamic => body(k)
  case k : Continuation.Static =>
    val name = core.Id("k")
    val reified = k.reify.asInstanceOf[Cont.ContLam]

    // do not create a joinpoint for continuations roughly of the size of a return cont...
    if reified.body.size <= 5 then
      body(k)
    else
      LetCont(name, reified.asInstanceOf, body(Continuation.Dynamic(name)))
}

enum Continuation {
  case Dynamic(id: Id) // in ml this is an arbitrary term, why?
  case Static(hint: Id, k: (Pure, Id) => Stmt)

  def apply(arg: Pure, ks: Id): Stmt = this match {
    case Continuation.Dynamic(id) => Jump(id, arg, MetaCont(ks))
    case Continuation.Static(hint, k) => k(arg, ks)
  }
  def reify: Cont =
    this match {
      case c : Continuation.Dynamic => cps.Cont.ContVar(c.id)
      case Continuation.Static(hint, k) =>
        val ks = Id("ks")
        cps.Cont.ContLam(hint, ks, k(Pure.ValueVar(hint), ks))
    }
}
object Continuation {
  def Static(hint: Id)(k: (Pure, Id) => Stmt): Continuation.Static = Continuation.Static(hint, k)

}
