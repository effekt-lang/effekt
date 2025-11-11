package effekt.core

import effekt.{Template, core, symbols}
import effekt.context.Context
import effekt.core.Type.{PromptSymbol, ResumeSymbol}

/**
 * Freshens bound names in a given term for tests.
 * Please use this _only_ for tests. Otherwise, prefer [[effekt.core.Renamer]].
 *
 * @param names used to look up a reference by name to resolve to the same symbols.
 *              This is only used by tests to deterministically rename terms and check for
 *              alpha-equivalence.
 * @param prefix if the prefix is empty, the original name will be used as a prefix
 *
 * @param C the context is used to copy annotations from old symbols to fresh symbols
 */
class TestRenamer(names: Names = Names(Map.empty)) extends core.Tree.Rewrite {

  // list of scopes that map bound symbols to their renamed variants.
  private var scopes: List[Map[Id, Id]] = List.empty

  // Here we track ALL renamings
  var renamed: Map[Id, Id] = Map.empty

  private var suffix: Int = 0

  def freshIdFor(id: Id): Id =
    suffix = suffix + 1
    Id(id.name.name, suffix)

  def withBindings[R](ids: List[Id])(f: => R): R =
    val before = scopes
    try {
      val newScope = ids.map { x => x -> freshIdFor(x) }.toMap
      scopes = newScope :: scopes
      renamed = renamed ++ newScope
      f
    } finally { scopes = before }

  /** Alias for withBindings(List(id)){...} */
  def withBinding[R](id: Id)(f: => R): R = withBindings(List(id))(f)

  // free variables cannot be left untouched, because top-level items may be mutually recursive.
  // This means that a bound occurrence may precede its binding.
  override def id: PartialFunction[core.Id, core.Id] = {
    case id => {
      if (isBuiltin(id)) {
        id
      } else {
        scopes.collectFirst {
          case bnds if bnds.contains(id) => bnds(id)
        }.getOrElse {
          scopes match {
            case Nil =>
              id
            case head :: tail => {
              val freshId = freshIdFor(id)
              scopes = (head + (id -> freshId)) :: tail
              freshId
            }
          }
        }
      }
    }
  }

  override def stmt: PartialFunction[Stmt, Stmt] = {
    case core.Def(id, block, body) =>
      // can be recursive
      withBinding(id) { core.Def(rewrite(id), rewrite(block), rewrite(body)) }

    case core.Let(id, tpe, binding, body) =>
      val resolvedBinding = rewrite(binding)
      withBinding(id) { core.Let(rewrite(id), rewrite(tpe), resolvedBinding, rewrite(body)) }

    case core.ImpureApp(id, callee, targs, vargs, bargs, body) =>
      val resolvedCallee = rewrite(callee)
      val resolvedTargs = targs map rewrite
      val resolvedVargs = vargs map rewrite
      val resolvedBargs = bargs map rewrite
      withBinding(id) { core.ImpureApp(rewrite(id), resolvedCallee, resolvedTargs, resolvedVargs, resolvedBargs, rewrite(body)) }

    case core.Val(id, tpe, binding, body) =>
      val resolvedBinding = rewrite(binding)
      withBinding(id) { core.Val(rewrite(id), rewrite(tpe), resolvedBinding, rewrite(body)) }

    case core.Alloc(id, init, reg, body) =>
      val resolvedInit = rewrite(init)
      val resolvedReg = rewrite(reg)
      withBinding(id) { core.Alloc(rewrite(id), resolvedInit, resolvedReg, rewrite(body)) }

    case core.Var(ref, init, capt, body) =>
      val resolvedInit = rewrite(init)
      val resolvedCapt = rewrite(capt)
      withBinding(ref) { core.Var(rewrite(ref), resolvedInit, resolvedCapt, rewrite(body)) }

    case core.Get(id, tpe, ref, capt, body) =>
      val resolvedRef = rewrite(ref)
      val resolvedCapt = rewrite(capt)
      withBinding(id) { core.Get(rewrite(id), rewrite(tpe), resolvedRef, resolvedCapt, rewrite(body)) }

  }

  override def block: PartialFunction[Block, Block] = {
    case Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      withBindings(tparams ++ cparams ++ vparams.map(_.id) ++ bparams.map(_.id)) {
        Block.BlockLit(tparams map rewrite, cparams map rewrite, vparams map rewrite, bparams map rewrite,
          rewrite(body))
      }
  }

  override def rewrite(o: Operation): Operation = o match {
    case Operation(name, tparams, cparams, vparams, bparams, body) =>
      withBindings(tparams ++ cparams ++ vparams.map(_.id) ++ bparams.map(_.id)) {
        Operation(name,
          tparams map rewrite,
          cparams map rewrite,
          vparams map rewrite,
          bparams map rewrite,
          rewrite(body))
      }
  }

  override def rewrite(toplevel: Toplevel): Toplevel = toplevel match {
    case Toplevel.Def(id, block) =>
      withBinding(id) {
        Toplevel.Def(rewrite(id), rewrite(block))
      }
    case Toplevel.Val(id, tpe, binding) =>
      val resolvedBinding = rewrite(binding)
      withBinding(id) {
        Toplevel.Val(rewrite(id), rewrite(tpe), resolvedBinding)
      }
  }

  override def rewrite(d: Declaration): Declaration = d match {
    case Declaration.Data(id: Id, tparams: List[Id], constructors: List[Constructor]) =>
      withBinding(id) {
      withBindings(tparams) {
        Declaration.Data(rewrite(id), tparams map rewrite, constructors map rewrite)
      }}
    case Declaration.Interface(id: Id, tparams: List[Id], properties: List[Property]) =>
      withBinding(id) {
      withBindings(tparams) {
        Declaration.Interface(rewrite(id), tparams map rewrite, properties map rewrite)
      }}
  }

  override def rewrite(e: ExternBody) = e match {
    case ExternBody.StringExternBody(featureFlag, contents) =>
      ExternBody.StringExternBody(featureFlag, rewriteTemplate(contents))
    case ExternBody.Unsupported(err) => ???
  }

  def rewriteTemplate(t: Template[Expr]) = t match {
    case Template(strings, args) => Template(strings, args map rewrite)
  }

  override def rewrite(e: Extern) = e match {
    case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) => {
      withBinding(id) {
        withBindings(tparams ++ cparams ++ vparams.map(_.id) ++ bparams.map(_.id)) {
          Extern.Def(
            rewrite(id),
            tparams map rewrite,
            cparams map rewrite,
            vparams map rewrite,
            bparams map rewrite,
            rewrite(ret),
            rewrite(annotatedCapture),
            rewrite(body)
          )
        }
      }
    }
    case Extern.Include(featureFlag, contents) => {
        Extern.Include(featureFlag, contents)
    }
  }

  override def rewrite(c: Constructor) = c match {
    case Constructor(id, tparams, fields) =>
      withBinding(id) {
      withBindings(tparams) {
        Constructor(rewrite(id), tparams map rewrite, fields map rewrite)
      }}
  }

  override def rewrite(p: Property) = p match {
    case Property(id: Id, tpe: BlockType) =>
      withBinding(id) {
        Property(rewrite(id), rewrite(tpe))
      }
  }

  override def rewrite(f: Field) = f match {
    case Field(id, tpe) =>
      withBinding(id) {
        Field(rewrite(id), rewrite(tpe))
      }
  }

  override def rewrite(b: BlockType): BlockType = b match {
    case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      withBindings(tparams ++ cparams) {
        BlockType.Function(tparams map rewrite, cparams map rewrite, vparams map rewrite, bparams map rewrite, rewrite(result))
      }
    case BlockType.Interface(name, targs) =>
      BlockType.Interface(rewrite(name), targs map rewrite)
  }

  override def rewrite(b: BlockType.Interface) = b match {
    case BlockType.Interface(name, targs) =>
      BlockType.Interface(rewrite(name), targs map rewrite)
  }

  override def rewrite(t: ValueType): ValueType = t match {
    case ValueType.Var(name) => ValueType.Var(rewrite(name))
    case ValueType.Data(name, targs) => {
      val newName = rewrite(name)
      ValueType.Data(rewrite(name), targs map rewrite)
    }
    case ValueType.Boxed(tpe, capt) => ValueType.Boxed(rewrite(tpe), rewrite(capt))
  }

  override def rewrite(t: ValueType.Data): ValueType.Data = t match {
    case ValueType.Data(name: Id, targs: List[ValueType]) => ValueType.Data(rewrite(name), targs map rewrite)
  }

  override def rewrite(b: BlockParam): BlockParam = b match {
    case BlockParam(id, tpe, capt) => withBinding(id) {
      BlockParam(rewrite(id), rewrite(tpe), rewrite(capt))
    }
  }

  override def rewrite(v: ValueParam): ValueParam = v match {
    case ValueParam(id, tpe) => withBinding(id) {
      ValueParam(rewrite(id), rewrite(tpe))
    }
  }

  def apply(m: core.ModuleDecl): core.ModuleDecl =
    suffix = 0
    m match {
      case core.ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
        core.ModuleDecl(path, includes, declarations map rewrite, externs map rewrite, definitions map rewrite, exports map rewrite)
    }

  def apply(s: Stmt): Stmt = {
    suffix = 0
    rewrite(s)
  }
}