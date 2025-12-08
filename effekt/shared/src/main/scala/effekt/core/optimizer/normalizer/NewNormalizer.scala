package effekt
package core
package optimizer
package normalizer

import effekt.core.ValueType.Boxed

// TODO
// - change story of how inlining is implemented. We need to also support toplevel functions that potentially
//   inline each other. Do we need to sort them topologically? How do we deal with (mutually) recursive definitions?
//
//
// plan: only introduce parameters for free things inside a block that are bound in the **stack**
// that is in
//
// only abstract over p, but not n:
//
//   def outer(n: Int) =
//     def foo(p) = shift(p) { ... n ... }
//     reset { p =>
//       ...
//     }
//
// Same actually for stack allocated mutable state, we should abstract over those (but only those)
// and keep the function in its original location.
// This means we only need to abstract over blocks, no values, no types.
//
// TODO Region desugaring
// region r {
//   reset { p =>
//    var x in r = 42
//    x = !x + 1
//    println(!x)
//   }
// }
//
// reset { r =>
//   reset { p =>
//    //var x in r = 42
//    shift(r) { k =>
//      var x = 42
//      resume(k) {
//        x = !x + 1
//        println(!x)
//      }
//    }
//   }
// }
//
// - Typeability preservation: {r: Region} becomes {r: Prompt[T]}
//    [[ def f() {r: Region} = s ]] = def f[T]() {r: Prompt[T]} = ...
// - Continuation capture is _not_ constant time in JS backend, so we expect a (drastic) slowdown when desugaring

/**
 * A new normalizer that is conservative (avoids code bloat)
 */
class NewNormalizer {

  import semantics.*

  // used for potentially recursive definitions
  def evaluateRecursive(id: Id, block: core.BlockLit, bound: List[Static[BlockParam]])(using env: Env, scope: Scope): Computation =
    block match {
      case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
        val freshened = Id(id)

        // we keep the params as they are for now...
        given localEnv: Env = env
          .bindValue(vparams.map(p => p.id -> p.id))
          .bindComputation(bparams.map(p => p.id -> Computation.Unknown(p.id)))
          // Assume that we capture nothing
          .bindComputation(id, Computation.Def(Closure(freshened, Nil)))

        val normalizedBlock = scope.local {
          Block(tparams, vparams, bparams, nested {
            evaluate(body, Frame.Return, Stack.Unknown)(using localEnv)
          })
        }

        val dynamicCapt = env.subst(normalizedBlock.dynamicCapture.toList)
        val closureParams = bound.filter { bp => dynamicCapt.map(_.id) contains bp.id.id }

        // Only normalize again if we actually we wrong in our assumption that we capture nothing
        // We might run into exponential complexity for nested recursive functions
        if (closureParams.isEmpty) {
          scope.defineRecursive(freshened, normalizedBlock.copy(bparams = normalizedBlock.bparams), block.tpe, block.capt)
          Computation.Def(Closure(freshened, Nil))
        } else {
          val captures = closureParams.map { p => Computation.Known(p.map(_.id)): Computation.Known }.toList
          given localEnv1: Env = env
            .bindValue(vparams.map(p => p.id -> p.id))
            .bindComputation(bparams.map(p => p.id -> Computation.Unknown(p.id)))
            .bindComputation(id, Computation.Def(Closure(freshened, captures)))

          val normalizedBlock1 = Block(tparams, vparams, bparams, nested {
            evaluate(body, Frame.Return, Stack.Unknown)(using localEnv1)
          })

          val tpe: BlockType.Function = block.tpe match {
            case _: BlockType.Interface => ???
            case ftpe: BlockType.Function => ftpe
          }
          scope.defineRecursive(
            freshened,
            normalizedBlock1.copy(bparams = normalizedBlock1.bparams ++ closureParams.map(_.id)),
            tpe.copy(cparams = tpe.cparams ++ closureParams.map { bp => bp.id.id }),
            block.capt
          )
          Computation.Def(Closure(freshened, captures))
        }

    }

  // the stack here is not the one this is run in, but the one the definition potentially escapes
  // Block, String, List[BlockParam], Env, Scope => Computation
  def evaluate(block: core.Block, hint: String, bound: List[Static[BlockParam]])(using env: Env, scope: Scope): Computation = block match {
    case core.Block.BlockVar(id, annotatedTpe, annotatedCapt) =>
      env.lookupComputation(id)
    case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) =>
      // we keep the params as they are for now...
      given localEnv: Env = env
        .bindValue(vparams.map(p => p.id -> p.id))
        .bindComputation(bparams.map(p => p.id -> Computation.Unknown(p.id)))

      val normalizedBlock = Block(tparams, vparams, bparams, nested {
        evaluate(body, Frame.Return, Stack.Unknown)
      })

      val dynamicCapt = env.subst(normalizedBlock.dynamicCapture.toList)
      val closureParams = bound.filter { bp => dynamicCapt contains bp.id.id }

      val f = Id(hint)
      scope.define(f, normalizedBlock.copy(bparams = normalizedBlock.bparams ++ closureParams.map(_.id)))
      Computation.Def(Closure(f, closureParams.map { bp => Computation.Known(bp.map(_.id)) }))

    case core.Block.Unbox(pure) =>
      val addr = evaluate(pure, bound)
      scope.lookupValue(addr) match {
        case Some(Value.Box(body, _)) => body
        case Some(_) | None => {
          val (tpe, capt) = pure.tpe match {
            case Boxed(tpe, capt) => (tpe, capt)
            case _ => sys error "should not happen"
          }
          // TODO translate static capture set capt to a dynamic capture set (e.g. {exc} -> {@p_17})
          val unboxAddr = scope.unbox(addr, tpe, capt)
          Computation.Unknown(unboxAddr)
        }
      }

    // TODO this does not work for recursive objects currently
    case core.Block.New(Implementation(interface, operations)) =>
      val ops = operations.map {
        case Operation(name, tparams, cparams, vparams, bparams, body) =>
          // Check whether the operation is already "just" an eta expansion and then use the identifier...
          //   no need to create a fresh block literal
          val eta: Option[Closure] =
            body match {
              case Stmt.App(BlockVar(id, _, _), targs, vargs, bargs) =>
                def sameTargs = targs == tparams.map(t => ValueType.Var(t))
                def sameVargs = vargs == vparams.map(p => ValueVar(p.id, p.tpe))
                def sameBargs = bargs == bparams.map(p => BlockVar(p.id, p.tpe, p.capt))
                def isEta = sameTargs && sameVargs && sameBargs

                env.lookupComputation(id) match {
                  // TODO what to do with closure environment
                  case Computation.Def(closure) if isEta => Some(closure)
                  case _ => None
                }
              case _ => None
            }

          val closure = eta.getOrElse {
            evaluate(core.Block.BlockLit(tparams, cparams, vparams, bparams, body), name.name.name, bound) match {
              case Computation.Def(closure) => closure
              case _ => sys error "Should not happen"
            }
          }
          (name, closure)
      }
      Computation.New(interface, ops)
  }

  def evaluate(expr: Expr, bound: List[Static[BlockParam]])(using env: Env, scope: Scope): Addr = expr match {
    case Expr.ValueVar(id, annotatedType) =>
      env.lookupValue(id)

    case core.Expr.Literal(value, annotatedType) => value match {
      case As.IntRep(x) => scope.allocate("x", Value.Integer(x))
      case As.StringRep(x) => scope.allocate("x", Value.String(x))
      case _ => scope.allocate("x", Value.Literal(value, annotatedType))
    }

    case core.Expr.PureApp(f, targs, vargs) =>
      val externDef = env.lookupComputation(f.id)
      val vargsEvaluated = vargs.map(evaluate(_, bound))
      val valuesOpt: Option[List[semantics.Value]] =
        vargsEvaluated.foldLeft(Option(List.empty[semantics.Value])) { (acc, addr) =>
          for {
            xs <- acc
            x <- scope.lookupValue(addr)
          } yield x :: xs
        }.map(_.reverse)
      (valuesOpt, externDef) match {
        case (Some(values), Computation.BuiltinExtern(id, name)) if supportedBuiltins(name).isDefinedAt(values) =>
          val impl = supportedBuiltins(name)
          val res = impl(values)
          scope.allocate("x", res)
        case _ => scope.allocate("x", Value.PureExtern(f, targs, vargs.map(evaluate(_, bound))))
      }

    case core.Expr.Make(data, tag, targs, vargs) =>
      scope.allocate("x", Value.Make(data, tag, targs, vargs.map(evaluate(_, bound))))

    case core.Expr.Box(b, annotatedCapture) =>
      /*
      var counter = 22;
      val p : Borrowed[Int] at counter = box new Borrowed[Int] {
        def dereference() = counter
       };
       counter = counter + 1;
       println(p.dereference)
      */
      // should capture `counter` but does not since the stack is Stack.Unknown
      // (effekt.JavaScriptTests.examples/pos/capture/borrows.effekt (js))
      // TLDR we need to pass an escaping stack to do a proper escape analysis. Stack.Unkown is insufficient
      val comp = evaluate(b, "x", bound)
      scope.allocate("x", Value.Box(comp, annotatedCapture))
  }

  // TODO make evaluate(stmt) return BasicBlock (won't work for shift or reset, though)
  def evaluate(stmt: Stmt, k: Frame, ks: Stack)(using env: Env, scope: Scope): NeutralStmt = stmt match {

    case Stmt.Return(expr) =>
      k.ret(ks, evaluate(expr, ks.bound))

    case Stmt.Val(id, annotatedTpe, binding, body) =>
      evaluate(binding, k.push(annotatedTpe) { scope => res => k => ks =>
        given Scope = scope
        bind(id, res) { evaluate(body, k, ks) }
      }, ks)

    case Stmt.ImpureApp(id, f, targs, vargs, bargs, body) =>
      assert(bargs.isEmpty)
      val addr = scope.run("x", f, targs, vargs.map(evaluate(_, ks.bound)), bargs.map(evaluate(_, "f", Stack.Unknown.bound)))
      evaluate(body, k, ks)(using env.bindValue(id, addr), scope)

    case Stmt.Let(id, annotatedTpe, binding, body) =>
      bind(id, evaluate(binding, ks.bound)) { evaluate(body, k, ks) }

    // can be recursive
    case Stmt.Def(id, block: core.BlockLit, body) =>
      bind(id, evaluateRecursive(id, block, ks.bound)) { evaluate(body, k, ks) }

    case Stmt.Def(id, block, body) =>
      bind(id, evaluate(block, id.name.name, ks.bound)) { evaluate(body, k, ks) }

    case Stmt.App(core.Block.BlockLit(tparams, cparams, vparams, bparams, body), targs, vargs, bargs) =>
      // TODO also bind type arguments in environment
      // TODO substitute cparams???
      val newEnv = env
        .bindValue(vparams.zip(vargs).map { case (p, a) => p.id -> evaluate(a, ks.bound) })
        .bindComputation(bparams.zip(bargs).map { case (p, a) => p.id -> evaluate(a, "f", ks.bound) })

      evaluate(body, k, ks)(using newEnv, scope)

    case Stmt.App(callee, targs, vargs, bargs) =>
      evaluate(callee, "f", ks.bound) match {
        case Computation.Unknown(id) =>
          reify(k, ks) { NeutralStmt.App(id, targs, vargs.map(evaluate(_, ks.bound)), bargs.map(evaluate(_, "f", ks.bound))) }
        case Computation.Def(Closure(label, environment)) =>
          val args = vargs.map(evaluate(_, ks.bound))
          /*
          try {
            prog {
              do Eff()
            }
          } with Eff { ... }
          ---
          val captures = stack.bound.filter { block.free }
          is incorrect as the result is always the empty capture set since Stack.Unkown.bound = Set()
           */
          val blockargs = bargs.map(evaluate(_, "f", ks.bound))
          // if stmt doesn't capture anything, it can not make any changes to the stack (ks) and we don't have to pretend it is unknown as an over-approximation
          // compute dynamic captures of the whole statement (App node)
          val dynCaptures = blockargs.flatMap(_.dynamicCapture) ++ environment
          if (dynCaptures.isEmpty) {
            reifyKnown(k, ks) {
              NeutralStmt.Jump(label, targs, args, blockargs)
            }
          } else {
            reify(k, ks) {
              NeutralStmt.Jump(label, targs, args, blockargs ++ environment)
            }
          }
        case _: (Computation.New | Computation.Known | Computation.Continuation | Computation.BuiltinExtern) => sys error "Should not happen"
      }

    // case Stmt.Invoke(New)

    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
      val bound = Stack.Unknown.bound
      evaluate(callee, "o", bound) match {
        case Computation.Unknown(id) =>
          reify(k, ks) { NeutralStmt.Invoke(id, method, methodTpe, targs, vargs.map(evaluate(_, ks.bound)), bargs.map(evaluate(_, "f", bound))) }
        case Computation.New(interface, operations) =>
          operations.collectFirst { case (id, Closure(label, environment)) if id == method =>
            reify(k, ks) { NeutralStmt.Jump(label, targs, vargs.map(evaluate(_, ks.bound)), bargs.map(evaluate(_, "f", bound)) ++ environment) }
          }.get
        case _: (Computation.Def | Computation.Known | Computation.Continuation | Computation.BuiltinExtern) => sys error s"Should not happen"
      }

    case Stmt.If(cond, thn, els) =>
      val sc = evaluate(cond, ks.bound)
      scope.lookupValue(sc) match {
        case Some(Value.Literal(true, _)) => evaluate(thn, k, ks)
        case Some(Value.Literal(false, _)) => evaluate(els, k, ks)
        case _ =>
          // joinpoint(k, ks, List(thn, els)) { (A, Frame, Stack) => (NeutralStmt, Variables) } { case thn1 :: els1 :: Nil => NeutralStmt.If(sc, thn1, els1) }
          joinpoint(k, ks) { (k, ks) =>
            NeutralStmt.If(sc, nested {
              evaluate(thn, k, ks)
            }, nested {
              evaluate(els, k, ks)
            })
          }
      }
    case Stmt.Match(scrutinee, clauses, default) =>
      val sc = evaluate(scrutinee, ks.bound)
      scope.lookupValue(sc) match {
        case Some(Value.Make(data, tag, targs, vargs)) =>
          // TODO substitute types (or bind them in the env)!
          clauses.collectFirst {
            case (tpe, core.Block.BlockLit(tparams, cparams, vparams, bparams, body)) if tpe == tag =>
              bind(vparams.map(_.id).zip(vargs)) { evaluate(body, k, ks) }
          }.getOrElse {
            evaluate(default.getOrElse { sys.error("Non-exhaustive pattern match.") }, k, ks)
          }
        //        case _ if (clauses.size + default.size) <= 1 =>
        //          NeutralStmt.Match(sc,
        //            clauses.map { case (id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
        //              given localEnv: Env = env.bindValue(vparams.map(p => p.id -> p.id))
        //              val block = Block(tparams, vparams, bparams, nested {
        //                evaluate(body, k, ks)
        //              })
        //              (id, block)
        //            },
        //            default.map { stmt => nested { evaluate(stmt, k, ks) } })
        case _ =>
          def neutralMatch(k: Frame, ks: Stack) =
            NeutralStmt.Match(sc,
              // This is ALMOST like evaluate(BlockLit), but keeps the current continuation
              clauses.map { case (id, core.Block.BlockLit(tparams, cparams, vparams, bparams, body)) =>
                given localEnv: Env = env.bindValue(vparams.map(p => p.id -> p.id))
                val block = Block(tparams, vparams, bparams, nested { scope ?=>
                  // here we now know that our scrutinee sc has the shape id(vparams, ...)

                  val datatype = scrutinee.tpe match {
                    case tpe @ ValueType.Data(name, targs) => tpe
                    case tpe => sys error s"Should not happen: pattern matching on a non-datatype: ${tpe}"
                  }
                  val eta = Value.Make(datatype, id, tparams.map(t => ValueType.Var(t)), vparams.map(p => p.id))
                  scope.bindings = scope.bindings.updated(sc, Binding.Let(eta))
                  evaluate(body, k, ks)
                })
                (id, block)
              },
              default.map { stmt => nested { evaluate(stmt, k, ks) } })
          // linear usage of the continuation: do not create a joinpoint.
          // This is a simple optimization for record access since r.x is always desugared into a match
          if (default.size + clauses.size > 1) {
            joinpoint(k, ks) { (k, ks) => neutralMatch(k, ks) }
          } else {
            neutralMatch(k, ks)
          }
      }

    case Stmt.Hole(span) => NeutralStmt.Hole(span)

    // State
    case Stmt.Region(BlockLit(Nil, List(capture), Nil, List(cap), body)) =>
      val reg = Id(cap.id)
      bind(cap.id, Computation.Known(Static.Region(reg))) {
        evaluate(body, Frame.Return, Stack.Region(BlockParam(reg, cap.tpe, cap.capt), Map.empty, k, ks))
      }
    case Stmt.Region(_) => ???
    case Stmt.Alloc(ref, init, region, body) =>
      val r = Id(ref)
      val reg = env.subst(region)
      val addr = evaluate(init, ks.bound)
      val bp = BlockParam(r, Type.TState(init.tpe), Set(reg))
      bind(ref, Computation.Known(Static.Reference(r))) {
        alloc(bp, reg, addr, ks) match {
          case Some(ks1) => evaluate(body, k, ks1)
          case None => NeutralStmt.Alloc(bp, addr, reg, nested {
            evaluate(body, k, ks)
          })
        }
      }

    case Stmt.Var(ref, init, capture, body) =>
      val r = Id(ref)
      val addr = evaluate(init, ks.bound)
      bind(ref, Computation.Known(Static.Reference(r))) {
        evaluate(body, Frame.Return, Stack.Var(BlockParam(r, Type.TState(init.tpe), Set(capture)), addr, k, ks))
      }
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) =>
      val ref1 = env.subst(ref)
      get(ref1, ks) match {
        case Some(addr) => bind(id, addr) { evaluate(body, k, ks) }
        case None => bind(id, scope.allocateGet(ref1, annotatedTpe, annotatedCapt)) { evaluate(body, k, ks) }
      }
    case Stmt.Put(ref, annotatedCapt, value, body) =>
      val addr = evaluate(value, ks.bound)
      val ref1 = env.subst(ref)
      put(ref1, addr, ks) match {
        case Some(stack) => evaluate(body, k, stack)
        case None =>
          NeutralStmt.Put(ref1, value.tpe, annotatedCapt, addr, nested { evaluate(body, k, ks) })
      }

    // Control Effects
    case Stmt.Shift(prompt, core.Block.BlockLit(Nil, cparam :: Nil, Nil, k2 :: Nil, body)) =>
      val p = env.subst(prompt.id)

      if (ks.bound.exists { other => other.id == p }) {
        val (cont, frame, stack) = shift(p, k, ks)
        given Env = env.bindComputation(k2.id -> Computation.Continuation(cont) :: Nil)
        evaluate(body, frame, stack)
      } else {
        val neutralBody = {
          given Env = env.bindComputation(k2.id -> Computation.Unknown(k2.id) :: Nil)
          nested {
            evaluate(body, Frame.Return, Stack.Unknown)
          }
        }
        assert(Set(cparam) == k2.capt, "At least for now these need to be the same")
        reify(k, ks) { NeutralStmt.Shift(p, cparam, k2, neutralBody) }
      }
    case Stmt.Shift(_, _) => ???

    case Stmt.Reset(core.Block.BlockLit(Nil, cparams, Nil, prompt :: Nil, body)) =>
      val p = Id(prompt.id)
      bind(prompt.id, Computation.Known(Static.Prompt(p))) {
        evaluate(body, Frame.Return, Stack.Reset(BlockParam(p, prompt.tpe, prompt.capt), k, ks))
      }

    case Stmt.Reset(_) => ???
    case Stmt.Resume(k2, body) =>
      env.lookupComputation(k2.id) match {
        case Computation.Unknown(r) =>
          reify(k, ks) {
            NeutralStmt.Resume(r, nested {
              evaluate(body, Frame.Return, Stack.Unknown)
            })
          }
        case Computation.Continuation(k3) =>
          val (k4, ks4) = resume(k3, k, ks)
          evaluate(body, k4, ks4)
        case _ => ???
      }
  }

  def run(mod: ModuleDecl): ModuleDecl = {
    //util.trace(mod)
    // TODO deal with async externs properly (see examples/benchmarks/input_output/dyck_one.effekt)
    val externTypes = mod.externs.collect { case d: Extern.Def =>
      d.id -> (BlockType.Function(d.tparams, d.cparams, d.vparams.map { _.tpe }, d.bparams.map { bp => bp.tpe }, d.ret), d.annotatedCapture)
    }

    val (builtinExterns, otherExterns) = mod.externs.collect { case d: Extern.Def => d }.partition {
      case Extern.Def(id, tps, cps, vps, bps, ret, capt, targetBody, Some(vmBody)) =>
        val builtinName = vmBody.contents.strings.head
        supportedBuiltins.contains(builtinName)
      case _ => false
    }

    val builtinNameToBlockVar: Map[String, BlockVar] = builtinExterns.collect {
      case Extern.Def(id, tps, cps, vps, bps, ret, capt, targetBody, Some(vmBody)) =>
        val builtinName = vmBody.contents.strings.head
        val bv: BlockVar = BlockVar(id, BlockType.Function(tps, cps, vps.map { _.tpe }, bps.map { bp => bp.tpe }, ret), capt)
        builtinName -> bv
    }.toMap

    val toplevelEnv = Env.empty
      // user-defined functions
      .bindComputation(mod.definitions.collect {
        case Toplevel.Def(id, b) => id -> (b match {
          case core.Block.BlockLit(tparams, cparams, vparams, bparams, body) => Computation.Def(Closure(id, Nil))
          case core.Block.BlockVar(idd, annotatedTpe, annotatedCapt) => Computation.Unknown(id)
          case core.Block.Unbox(pure) => Computation.Unknown(id)
          case core.Block.New(impl) => Computation.Unknown(id)
        })
      })
      // user-defined values
      .bindValue(mod.definitions.collect {
        case Toplevel.Val(id, _, _) => id -> id
      })
      // async extern functions
      .bindComputation(otherExterns.map(defn => defn.id -> Computation.Unknown(defn.id)))
      // pure extern functions
      .bindComputation(builtinExterns.flatMap(defn => defn.vmBody.map(vmBody => defn.id -> Computation.BuiltinExtern(defn.id, vmBody.contents.strings.head))))

    val typingContext = TypingContext(
      mod.definitions.collect {
        case Toplevel.Val(id, tpe, _) => id -> tpe
      }.toMap,
      mod.definitions.collect {
        case Toplevel.Def(id, b) => id -> (b.tpe, b.capt)
      }.toMap ++ externTypes,
      builtinNameToBlockVar
    )

    val newDefinitions = mod.definitions.map(d => run(d)(using toplevelEnv, typingContext))
    mod.copy(definitions = newDefinitions)
  }

  val showDebugInfo = true
  inline def debug(inline msg: => Any) = if (showDebugInfo) println(msg) else ()

  def run(defn: Toplevel)(using env: Env, G: TypingContext): Toplevel = defn match {
    case Toplevel.Def(id, core.Block.BlockLit(tparams, cparams, vparams, bparams, body)) =>
      debug(s"------- ${util.show(id)} -------")
      debug(util.show(body))

      val scope = Scope.empty
      val localEnv: Env = env
        .bindValue(vparams.map(p => p.id -> scope.allocate("p", Value.Var(p.id, p.tpe))))
        .bindComputation(bparams.map(p => p.id -> Computation.Unknown(p.id)))

      val result = evaluate(body, Frame.Return, Stack.Empty)(using localEnv, scope)

      debug(s"----------normalized-----------")
      val block = Block(tparams, vparams, bparams, reifyBindings(scope, result))
      debug(PrettyPrinter.show(block))

      debug(s"----------embedded-----------")
      val embedded = embedBlockLit(block)
      debug(util.show(embedded))

      Toplevel.Def(id, embedded)
    case other => other
  }

  case class TypingContext(values: Map[Addr, ValueType], blocks: Map[Label, (BlockType, Captures)], builtinBlockVars: Map[String, BlockVar]) {
    def bind(id: Id, tpe: ValueType): TypingContext = this.copy(values = values + (id -> tpe))
    def bind(id: Id, tpe: BlockType, capt: Captures): TypingContext = this.copy(blocks = blocks + (id -> (tpe, capt)))
    def bindValues(vparams: List[ValueParam]): TypingContext = this.copy(values = values ++ vparams.map(p => p.id -> p.tpe))
    def lookupValue(id: Id): ValueType = values.getOrElse(id, sys.error(s"Unknown value: ${util.show(id)}"))
    def bindComputations(bparams: List[BlockParam]): TypingContext = this.copy(blocks = blocks ++ bparams.map(p => p.id -> (p.tpe, p.capt)))
    def bindComputation(bparam: BlockParam): TypingContext = this.copy(blocks = blocks + (bparam.id -> (bparam.tpe, bparam.capt)))
  }

  def embedStmt(neutral: NeutralStmt)(using G: TypingContext): core.Stmt = neutral match {
    case NeutralStmt.Return(result) =>
      Stmt.Return(embedExpr(result))
    case NeutralStmt.Jump(label, targs, vargs, bargs) =>
      Stmt.App(embedBlockVar(label), targs, vargs.map(embedExpr), bargs.map(embedBlock))
    case NeutralStmt.App(label, targs, vargs, bargs) =>
      Stmt.App(embedBlockVar(label), targs, vargs.map(embedExpr), bargs.map(embedBlock))
    case NeutralStmt.Invoke(label, method, tpe, targs, vargs, bargs) =>
      Stmt.Invoke(embedBlockVar(label), method, tpe, targs, vargs.map(embedExpr), bargs.map(embedBlock))
    case NeutralStmt.If(cond, thn, els) =>
      Stmt.If(embedExpr(cond), embedStmt(thn), embedStmt(els))
    case NeutralStmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(embedExpr(scrutinee),
        clauses.map { case (id, block) => id -> embedBlockLit(block) },
        default.map(embedStmt))
    case NeutralStmt.Reset(prompt, body) =>
      val capture = prompt.capt match {
        case set if set.size == 1 => set.head
        case _ => sys error "Prompt needs to have a single capture"
      }
      Stmt.Reset(core.BlockLit(Nil, capture :: Nil, Nil, prompt :: Nil, embedStmt(body)(using G.bindComputation(prompt))))
    case NeutralStmt.Shift(prompt, capt, k, body) =>
      Stmt.Shift(embedBlockVar(prompt), core.BlockLit(Nil, capt :: Nil, Nil, k :: Nil, embedStmt(body)(using G.bindComputation(k))))
    case NeutralStmt.Resume(k, body) =>
      Stmt.Resume(embedBlockVar(k), embedStmt(body))
    case NeutralStmt.Var(blockParam, init, body) =>
      val capt = blockParam.capt match {
        case cs if cs.size == 1 => cs.head
        case _ => sys error "Variable needs to have a single capture"
      }
      Stmt.Var(blockParam.id, embedExpr(init), capt, embedStmt(body)(using G.bind(blockParam.id, blockParam.tpe, blockParam.capt)))
    case NeutralStmt.Put(ref, annotatedTpe, annotatedCapt, value, body) =>
      Stmt.Put(ref, annotatedCapt, embedExpr(value), embedStmt(body))
    case NeutralStmt.Region(id, body) =>
      Stmt.Region(BlockLit(Nil, List(id.id), Nil, List(id), embedStmt(body)(using G.bindComputation(id))))
    case NeutralStmt.Alloc(blockparam, init, region, body) =>
      Stmt.Alloc(blockparam.id, embedExpr(init), region, embedStmt(body)(using G.bind(blockparam.id, blockparam.tpe, blockparam.capt)))
    case NeutralStmt.Hole(span) =>
      Stmt.Hole(span)
  }

  def embedStmt(basicBlock: BasicBlock)(using G: TypingContext): core.Stmt = basicBlock match {
    case BasicBlock(bindings, stmt) =>
      bindings.foldRight((G: TypingContext) => embedStmt(stmt)(using G)) {
        case ((id, Binding.Let(value)), rest) => G =>
          val coreExpr = embedExpr(value)(using G)
          // TODO why do we even have this type in core, if we always infer it?
          Stmt.Let(id, coreExpr.tpe, coreExpr, rest(G.bind(id, coreExpr.tpe)))
        case ((id, Binding.Def(block)), rest) => G =>
          val coreBlock = embedBlock(block)(using G)
          Stmt.Def(id, coreBlock, rest(G.bind(id, coreBlock.tpe, coreBlock.capt)))
        case ((id, Binding.Rec(block, tpe, capt)), rest) => G =>
          val coreBlock = embedBlock(block)(using G.bind(id, tpe, capt))
          Stmt.Def(id, coreBlock, rest(G.bind(id, coreBlock.tpe, coreBlock.capt)))
        case ((id, Binding.Val(stmt)), rest) => G =>
          val coreStmt = embedStmt(stmt)(using G)
          Stmt.Val(id, coreStmt.tpe, coreStmt, rest(G.bind(id, coreStmt.tpe)))
        case ((id, Binding.Run(callee, targs, vargs, bargs)), rest) => G =>
          val vargs1 = vargs.map(arg => embedExpr(arg)(using G))
          val bargs1 = bargs.map(arg => embedBlock(arg)(using G))
          val tpe = Type.bindingType(callee, targs, vargs1, bargs1)
          core.ImpureApp(id, callee, targs, vargs1, bargs1, rest(G.bind(id, tpe)))
        case ((id, Binding.Unbox(addr, tpe, capt)), rest) => G =>
          val pureValue = embedExpr(addr)(using G)
          Stmt.Def(id, core.Block.Unbox(pureValue), rest(G.bind(id, tpe, capt)))
        case ((id, Binding.Get(ref, tpe, cap)), rest) => G =>
          Stmt.Get(id, tpe, ref, cap, rest(G.bind(id, tpe)) )
      }(G)
  }

  def embedExpr(value: Value)(using cx: TypingContext): core.Expr = value match {
    case Value.PureExtern(callee, targs, vargs) => Expr.PureApp(callee, targs, vargs.map(embedExpr))
    case Value.Literal(value, annotatedType) => Expr.Literal(value, annotatedType)
    case Value.Make(data, tag, targs, vargs) => Expr.Make(data, tag, targs, vargs.map(embedExpr))
    case Value.Box(body, annotatedCapture) => Expr.Box(embedBlock(body), annotatedCapture)
    case Value.Var(id, annotatedType) => Expr.ValueVar(id, annotatedType)
    case Value.Integer(value) => theories.integers.reify(value, cx.builtinBlockVars, embedNeutral)
    case Value.String(value) => theories.strings.reify(value, cx.builtinBlockVars, embedNeutral)
  }

  def embedNeutral(neutral: Neutral)(using G: TypingContext): core.Expr = neutral match {
    case Value.Var(id, annotatedType) => Expr.ValueVar(id, annotatedType)
    case Value.PureExtern(callee, targs, vargs) => Expr.PureApp(callee, targs, vargs.map(embedExpr))
  }

  def embedExpr(addr: Addr)(using G: TypingContext): core.Expr = Expr.ValueVar(addr, G.lookupValue(addr))

  def embedBlock(comp: Computation)(using G: TypingContext): core.Block = comp match {
    case Computation.Unknown(id) =>
      embedBlockVar(id)
    case Computation.Def(Closure(label, Nil)) =>
      embedBlockVar(label)
    case Computation.Def(closure) =>
      etaExpandToBlockLit(closure)
    case Computation.Known(id) =>
      embedBlockVar(id.id)
    case Computation.Continuation(k) => ???
    case Computation.BuiltinExtern(_, _) => ???
    case Computation.New(interface, operations) =>
      val ops = operations.map { etaExpandToOperation.tupled }
      core.Block.New(Implementation(interface, ops))
  }

  /**
   * Embed `Computation.Def` to a `core.BlockLit`
   * This eta-expands the block var that stands for the `Computation.Def` to a full block literal
   * so that we can supply the correct capture arguments from the environment.
   */
  def etaExpandToBlockLit(closure: Closure)(using G: TypingContext): core.BlockLit = {
    val Closure(label, environment) = closure
    val blockvar = embedBlockVar(label)
    G.blocks(label) match {
      // TODO why is `captures` unused?
      case (BlockType.Function(tparams, cparams, vparams, bparams, result), captures) =>
        val vps = vparams.map { p => core.ValueParam(Id("x"), p) }
        val vargs = vps.map { vp => core.Expr.ValueVar(vp.id, vp.tpe) }

        // this uses the invariant that we _append_ all environment captures to the bparams
        val (origCapts, synthCapts) = cparams.splitAt(bparams.length - environment.length)
        val (origBparams, synthBparams) = bparams.splitAt(bparams.length - environment.length)
        val origBps = origBparams.zip(origCapts).map { case (bp, c) => core.BlockParam(Id("f"), bp, Set(c)) }
        val origBargs = origBps.map { bp => core.BlockVar(bp.id, bp.tpe, bp.capt) }
        val synthBargs = environment.zip(synthBparams).zip(synthCapts).map {
          case ((Computation.Known(s), bp), c) => core.BlockVar(s.id, bp, Set(c))
        }
        val bargs = origBargs ++ synthBargs

        val targs = tparams.map { core.ValueType.Var.apply }

        core.Block.BlockLit(
          tparams,
          origCapts,
          vps,
          origBps,
          Stmt.App(
            blockvar,
            targs,
            vargs,
            bargs
          )
        )
      case _ => sys.error("Unexpected block type for a closure")
    }
  }

  /**
   * Embed an operation as part of a `Computation.New`.
   * This eta-expands the block var that stands for the operation body to a full operation
   * so that we can supply the correct capture arguments from the environment.
   */
  def etaExpandToOperation(id: Id, closure: Closure)(using G: TypingContext): core.Operation = {
   val Closure(label, environment) = closure
    G.blocks(label) match {
      case (BlockType.Function(tparams, cparams, vparams, bparams, result), captures) =>
        val tparams2 = tparams.map(t => Id(t))
        // TODO if we freshen cparams, then we also need to substitute them in the result AND the parameters
        val cparams2 = cparams //.map(c => Id(c))
        val vparams2 = vparams.map(t => ValueParam(Id("x"), t))
        val bparams2 = (bparams zip cparams).map { case (t, c) => BlockParam(Id("f"), t, Set(c)) }
        // In the following section, we create a new instance of the interface.
        // All operation bodies were lifted to block literals in an earlier stage.
        // While doing so, their block parameters (bparams) were concatenated with their capture parameters (cparams).
        // When we embed back to core, we need to "eta-expand" the operation body to supply the correct captures from the environment.
        // To see why this "eta-expansion" is necessary to achieve this, consider the following example:
        // ```scala
        // effect Eff(): Unit
        // def use = { do Eff() }
        // def main() = {
        //     val r = try {
        //         use()
        //     } with Eff {
        //         resume(())
        //     }
        // }
        // ```
        // the handler body normalizes to the following:
        // ```scala
        // reset {{p} =>
        //     jump use(){new Eff {def Eff = Eff @ [p]}}
        // }
        // ```
        // where
        // ```
        //  def Eff = (){p} { ... }
        // ```
        // In particular, the prompt `p` needs to be passed to the lifted operation body.
        // ```
        val (origBparams, synthBparams) = bparams2.splitAt(bparams2.length - environment.length)
        val bargs =
          // TODO: Fix captures
          origBparams.map { case bp => BlockVar(bp.id, bp.tpe, Set()) } ++
            synthBparams.zip(environment).map {
              // TODO: Fix captures
              case (bp, Computation.Known(s)) => BlockVar(s.id, bp.tpe, Set())
            }

        core.Operation(
          id,
          tparams2,
          cparams.take(cparams.length - environment.length),
          vparams2,
          origBparams,
          Stmt.App(
            embedBlockVar(label),
            tparams2.map(ValueType.Var.apply),
            vparams2.map(p => ValueVar(p.id, p.tpe)),
            bargs
          )
        )
      case _ => sys error "Unexpected block type"
    }
  }

  def embedBlock(block: Block)(using G: TypingContext): core.Block = block match {
    case Block(tparams, vparams, bparams, b) =>
      val cparams = bparams.map {
        case BlockParam(id, tpe, captures) =>
          assert(captures.size == 1)
          captures.head
      }
      core.Block.BlockLit(tparams, cparams, vparams, bparams,
        embedStmt(b)(using G.bindValues(vparams).bindComputations(bparams)))
  }

  def embedBlockLit(block: Block)(using G: TypingContext): core.BlockLit = embedBlock(block).asInstanceOf[core.BlockLit]

  def embedBlockVar(label: Label)(using G: TypingContext): core.BlockVar =
    val (tpe, capt) = G.blocks.getOrElse(label, sys error s"Unknown block: ${util.show(label)}. ${G.blocks.keys.map(util.show).mkString(", ")}")
    core.BlockVar(label, tpe, capt)
}
