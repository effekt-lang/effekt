
package effekt
package core


// Establishes a normal form in which every subexpression
// is explicitly named and aliasing is removed.
//
// let x = Cons(1, Cons(2, Cons(3, Nil())))
//
// ->
// let x1 = Nil()
// let x2 = Cons(3, x1)
// let x3 = Cons(2, x2)
// let x  = Cons(1, x3)
object BindSubexpressions {

  type Env = Map[Id, Id]
  def alias(from: Id, to: Id, env: Env): Env =
    env + (from -> env.getOrElse(to, to))

  def transform(m: ModuleDecl): ModuleDecl = m match {
    case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
      val (newDefs, env) = transformDefs(definitions)(using Map.empty)
      ModuleDecl(path, includes, declarations, externs, newDefs, exports)
  }

  def transformDefs(definitions: List[Definition])(using env: Env): (List[Definition], Env) =
    var definitionsSoFar = List.empty[Definition]
    var envSoFar = env

    definitions.foreach {
      case Definition.Def(id, block) =>
        transform(block)(using envSoFar) match {
          case Bind(Block.BlockVar(x, _, _), defs) =>
            definitionsSoFar ++= defs
            envSoFar = alias(id, x, envSoFar)

          case Bind(other, defs) =>
            definitionsSoFar = definitionsSoFar ++ (defs :+ Definition.Def(id, other))
        }
      case Definition.Let(id, tpe, expr) =>
        transform(expr)(using envSoFar) match {
          case Bind(Pure.ValueVar(x, _), defs) =>
            definitionsSoFar ++= defs
            envSoFar = alias(id, x, envSoFar)
          case Bind(other, defs) =>
            definitionsSoFar = definitionsSoFar ++ (defs :+ Definition.Let(id, transform(tpe)(using envSoFar), other))
        }
    }
    (definitionsSoFar, envSoFar)

  def transform(s: Stmt)(using env: Env): Stmt = s match {
    case Stmt.Scope(definitions, body) =>
      val (newDefs, newEnv) = transformDefs(definitions)
      normal.scope(newDefs, transform(body)(using newEnv))

    case Stmt.App(callee, targs, vargs, bargs) => delimit {
      for {
        c <- transform(callee)
        vs <- transformExprs(vargs)
        bs <- transformBlocks(bargs)
      } yield Stmt.App(c, targs.map(transform), vs, bs)
    }

    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => delimit {
      for {
        c <- transform(callee)
        vs <- transformExprs(vargs)
        bs <- transformBlocks(bargs)
      } yield Stmt.Invoke(c, method, transform(methodTpe), targs.map(transform), vs, bs)
    }

    case Stmt.Return(expr) => transform(expr).run { res => Stmt.Return(res) }
    case Stmt.Alloc(id, init, region, body) => transform(init).run { v => Stmt.Alloc(id, v, transform(region), transform(body)) }
    case Stmt.Var(id, init, capture, body) => transform(init).run { v => Stmt.Var(id, v, transform(capture), transform(body)) }
    case Stmt.Get(id, capt, tpe) => Stmt.Get(id, transform(capt), transform(tpe))
    case Stmt.Put(id, capt, value) => transform(value).run { v => Stmt.Put(id, transform(capt), v) }

    case Stmt.If(cond, thn, els) => transform(cond).run { c =>
      Stmt.If(c, transform(thn), transform(els))
    }
    case Stmt.Match(scrutinee, clauses, default) => transform(scrutinee).run { sc =>
      Stmt.Match(sc, clauses.map { case (tag, rhs) => (tag, transform(rhs)) }, default.map(transform))
    }

    // Congruences
    case Stmt.Region(body) => Stmt.Region(transform(body))
    case Stmt.Val(id, tpe, binding, body) => Stmt.Val(id, transform(tpe), transform(binding), transform(body))
    case Stmt.Reset(body) => Stmt.Reset(transform(body))
    case Stmt.Shift(prompt, body) => Stmt.Shift(transform(prompt), transform(body))
    case Stmt.Resume(k, body) => Stmt.Resume(transform(k), transform(body))
    case Stmt.Hole() => Stmt.Hole()
  }

  def transform(b: Block)(using Env): Bind[Block] = b match {
    case b: Block.BlockVar => pure(transform(b))
    case b: Block.BlockLit => pure(transform(b))
    case Block.New(impl) => pure(Block.New(transform(impl)))
    case Block.Unbox(pure) => transform(pure) { v => bind(Block.Unbox(v)) }
  }

  def transform(b: BlockLit)(using Env): BlockLit = b match {
    case BlockLit(tparams, cparams, vparams, bparams, body) =>
      BlockLit(tparams, cparams, vparams.map(transform), bparams.map(transform), transform(body))
  }

  def transform(b: BlockVar)(using Env): BlockVar = b match {
    case BlockVar(id, annotatedTpe, annotatedCapt) =>
      BlockVar(transform(id), transform(annotatedTpe), transform(annotatedCapt))
  }

  def transform(impl: Implementation)(using Env): Implementation = impl match {
    case Implementation(interface, operations) => Implementation(transform(interface).asInstanceOf, operations.map {
      case Operation(name, tparams, cparams, vparams, bparams, body) =>
        Operation(name, tparams, cparams, vparams.map(transform), bparams.map(transform), transform(body))
    })
  }

  def transform(p: ValueParam)(using Env): ValueParam = p match {
    case ValueParam(id, tpe) => ValueParam(id, transform(tpe))
  }
  def transform(p: BlockParam)(using Env): BlockParam = p match {
    case BlockParam(id, tpe, capt) => BlockParam(id, transform(tpe), transform(capt))
  }

  def transform(id: Id)(using env: Env): Id = env.getOrElse(id, id)

  def transform(e: Expr)(using Env): Bind[ValueVar | Literal] = e match {
    case Pure.ValueVar(id, tpe) => pure(ValueVar(transform(id), transform(tpe)))
    case Pure.Literal(value, tpe) => pure(Pure.Literal(value, transform(tpe)))

    case Pure.Make(data, tag, vargs) => transformExprs(vargs) { vs =>
      bind(Pure.Make(data, tag, vs))
    }
    case DirectApp(block, targs, vargs, bargs) => for {
      b <- transform(block);
      vs <- transformExprs(vargs);
      bs <- transformBlocks(bargs);
      res <- bind(DirectApp(b, targs.map(transform), vs, bs))
    } yield res
    case Pure.PureApp(block, targs, vargs) => for {
      b <- transform(block);
      vs <- transformExprs(vargs);
      res <- bind(Pure.PureApp(b, targs.map(transform), vs))
    } yield res
    case Pure.Select(target, field, tpe) => transform(target) { v => bind(Pure.Select(v, field, transform(tpe))) }
    case Pure.Box(block, capt) => transform(block) { b => bind(Pure.Box(b, transform(capt))) }

    case Run(s) => bind(Run(transform(s)))
  }

  def transformExprs(es: List[Expr])(using Env): Bind[List[ValueVar | Literal]] = traverse(es)(transform)
  def transformBlocks(es: List[Block])(using Env): Bind[List[Block]] = traverse(es)(transform)

  // Types
  // -----
  // Types mention captures and captures might require renaming after dealiasing
  def transform(tpe: ValueType)(using Env): ValueType = tpe match {
    case ValueType.Var(name) => ValueType.Var(transform(name))
    case ValueType.Data(name, targs) => ValueType.Data(name, targs.map(transform))
    case ValueType.Boxed(tpe, capt) => ValueType.Boxed(transform(tpe), transform(capt))
  }
  def transform(tpe: BlockType)(using Env): BlockType = tpe match {
    case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      BlockType.Function(tparams, cparams, vparams.map(transform), bparams.map(transform), transform(result))
    case BlockType.Interface(name, targs) =>
      BlockType.Interface(name, targs.map(transform))
  }
  def transform(captures: Captures)(using Env): Captures = captures.map(transform)


  // Binding Monad
  // -------------
  case class Bind[+A](value: A, definitions: List[Definition]) {
    def run(f: A => Stmt): Stmt = normal.scope(definitions, f(value))
    def map[B](f: A => B): Bind[B] = Bind(f(value), definitions)
    def flatMap[B](f: A => Bind[B]): Bind[B] =
      val Bind(result, other) = f(value)
      Bind(result, definitions ++ other)
    def apply[B](f: A => Bind[B]): Bind[B] = flatMap(f)
  }
  def pure[A](value: A): Bind[A] = Bind(value, Nil)
  def bind[A](expr: Expr): Bind[ValueVar] =
    val id = Id("tmp")
    Bind(ValueVar(id, expr.tpe), List(Definition.Let(id, expr.tpe, expr)))

  def bind[A](block: Block): Bind[BlockVar] =
    val id = Id("tmp")
    Bind(BlockVar(id, block.tpe, block.capt), List(Definition.Def(id, block)))

  def delimit(b: Bind[Stmt]): Stmt = b.run(a => a)

  def traverse[S, T](l: List[S])(f: S => Bind[T]): Bind[List[T]] =
    l match {
      case Nil => pure(Nil)
      case head :: tail => for { x <- f(head); xs <- traverse(tail)(f) } yield x :: xs
    }

}
