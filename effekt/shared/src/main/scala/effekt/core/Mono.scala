package effekt
package core

import effekt.context.Context
import effekt.util.messages.ErrorMessageReifier
import effekt.core.Type.functionType
import effekt.core.optimizer.Deadcode
import effekt.core.optimizer.BindSubexpressions

object Mono extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "mono"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = input match {
    case CoreTransformed(source, tree, mod, core) =>
      val main = Context.ensureMainExists(mod)
      val reachable = Deadcode.remove(main, core)
      val bound = BindSubexpressions.transform(reachable)
      val preprocessed = preprocess(Deadcode.remove(main, bound))
      val constraints = collect(preprocessed)
      val solution = solve(constraints)
      Some(CoreTransformed(source, tree, mod, specialize(preprocessed, solution)))
  }

  type FlowVar = Id

  case class Flow(from: Vector[TypeArg], to: FlowVar)
  type Flows = List[Flow]

  enum TypeArg {
    case Data(tpe: Id, targs: List[TypeArg])
    case Var(owner: FlowVar, pos: Int)
    case Boxed(tpe: BlockType, capt: Captures)
  }

  type Ground = TypeArg.Data | TypeArg.Boxed
  type Solution = Map[FlowVar, Set[Vector[Ground]]]


  /**
    Rewrites this:

    {{{
        def higherorder { f : [A] (A) => A } = f[Int](42)

        def main() = {
            println(higherorder { [B] (x) => x })
        }
    }}}

    To this:

    {{{
        interface Poly {
        def apply[A](a: A): A
        }

        def higherorder { f : Poly } = f.apply[Int](42)
        def main() = {
        def id = new Poly {
            def apply[B](b: B) = b
        }
        println(higherorder { id })
        }
    }}}
  */
  object preprocess {

    private object DeBruijn {

      case class Index(level: Int, position: Int)

      enum Capture {
        case Bound(index: Index)
        case Named(id: Id)
      }

      enum ValueType {
        case Var(index: Index)
        case Data(name: Id, targs: List[ValueType])
        case Boxed(tpe: BlockType, captures: Set[Capture])
      }

      enum BlockType {
        case Function(
          tarity: Int,
          carity: Int,
          vparams: List[ValueType],
          bparams: List[BlockType],
          result: ValueType
        )
        case Interface(name: Id, targs: List[ValueType])
      }

      private case class Environment(
        typeBinders: List[List[Id]],
        captureBinders: List[List[Id]]
      ) {
        def enter(tparams: List[Id], cparams: List[Id]): Environment =
          Environment(tparams :: typeBinders, cparams :: captureBinders)

        def typeIndex(id: Id): Option[Index] = indexOf(id, typeBinders)
        def captureIndex(id: Id): Option[Index] = indexOf(id, captureBinders)

        private def indexOf(id: Id, binders: List[List[Id]]): Option[Index] =
          binders.zipWithIndex.collectFirst {
            case (level, depth) if level.contains(id) =>
              Index(depth, level.indexOf(id))
          }
      }

      def apply(tpe: core.BlockType.Function, outer: List[Id])(using Context): BlockType =
        blockType(tpe)(using Environment(List(outer), List(Nil)))

      // Closing free variables in first-occurrence order makes the result
      // invariant under renaming both the local and the enclosing binders.
      def freeTypeVariables(tpe: core.BlockType): List[Id] =
        freeTypeVariables(tpe, Set.empty).distinct

      def freeCaptureVariables(tpe: core.BlockType): Set[Id] =
        freeCaptureVariables(tpe, Set.empty)

      private def blockType(tpe: core.BlockType)(using env: Environment, context: Context): BlockType = tpe match {
        case core.BlockType.Function(tparams, cparams, vparams, bparams, result) =>
          given Environment = env.enter(tparams, cparams)
          BlockType.Function(
            tparams.size,
            cparams.size,
            vparams.map(valueType),
            bparams.map(blockType),
            valueType(result)
          )

        case core.BlockType.Interface(name, targs) =>
          BlockType.Interface(name, targs.map(valueType))
      }

      private def valueType(tpe: core.ValueType)(using env: Environment, context: Context): ValueType = tpe match {
        case core.ValueType.Var(id) =>
          env.typeIndex(id) match {
            case Some(index) => ValueType.Var(index)
            case None => Context.abort(pretty"Unbound type variable '${id}' while encoding a polymorphic block")
          }

        case core.ValueType.Data(name, targs) =>
          ValueType.Data(name, targs.map(valueType))

        case core.ValueType.Boxed(tpe, captures) =>
          ValueType.Boxed(blockType(tpe), captures.map { id =>
            env.captureIndex(id).map(Capture.Bound.apply).getOrElse(Capture.Named(id))
          })
      }

      private def freeTypeVariables(tpe: core.BlockType, bound: Set[Id]): List[Id] = tpe match {
        case core.BlockType.Function(tparams, _, vparams, bparams, result) =>
          val locallyBound = bound ++ tparams
          vparams.flatMap(freeTypeVariables(_, locallyBound)) ++
            bparams.flatMap(freeTypeVariables(_, locallyBound)) ++
            freeTypeVariables(result, locallyBound)

        case core.BlockType.Interface(_, targs) =>
          targs.flatMap(freeTypeVariables(_, bound))
      }

      private def freeTypeVariables(tpe: core.ValueType, bound: Set[Id]): List[Id] = tpe match {
        case core.ValueType.Var(id) if !bound.contains(id) => List(id)
        case core.ValueType.Var(_) => Nil
        case core.ValueType.Data(_, targs) => targs.flatMap(freeTypeVariables(_, bound))
        case core.ValueType.Boxed(tpe, _) => freeTypeVariables(tpe, bound)
      }

      private def freeCaptureVariables(tpe: core.BlockType, bound: Set[Id]): Set[Id] = tpe match {
        case core.BlockType.Function(_, cparams, vparams, bparams, result) =>
          val locallyBound = bound ++ cparams
          vparams.flatMap(freeCaptureVariables(_, locallyBound)).toSet ++
            bparams.flatMap(freeCaptureVariables(_, locallyBound)) ++
            freeCaptureVariables(result, locallyBound)

        case core.BlockType.Interface(_, targs) =>
          targs.flatMap(freeCaptureVariables(_, bound)).toSet
      }

      private def freeCaptureVariables(tpe: core.ValueType, bound: Set[Id]): Set[Id] = tpe match {
        case core.ValueType.Var(_) => Set.empty
        case core.ValueType.Data(_, targs) => targs.flatMap(freeCaptureVariables(_, bound)).toSet
        case core.ValueType.Boxed(tpe, captures) =>
          captures.diff(bound) ++ freeCaptureVariables(tpe, bound)
      }
    }

    private case class Encoding(interface: Id, method: Id)

    private case class Encoded(encoding: Encoding, outer: List[Id]) {
      def interface(arguments: List[ValueType]): BlockType.Interface = {
        assert(arguments.size == outer.size)
        BlockType.Interface(encoding.interface, arguments)
      }

      def openInterface: BlockType.Interface =
        interface(outer.map(ValueType.Var.apply))
    }

    private class State(using Context) {
      val interfaces = collection.mutable.ArrayBuffer.empty[Declaration.Interface]
      private var encodings: Map[DeBruijn.BlockType, Encoding] = Map.empty

      def encode(tpe: BlockType.Function, scope: Scope): Encoded = {
        val outer = DeBruijn.freeTypeVariables(tpe)
        val unbound = outer.toSet -- scope.typeParams.toSet

        if (unbound.nonEmpty) {
          Context.abort(pretty"Unbound type variables while encoding a polymorphic block: ${unbound}")
        }

        val freeCaptures = DeBruijn.freeCaptureVariables(tpe).intersect(scope.captureParams.toSet)
        if (freeCaptures.nonEmpty) {
          Context.abort(pretty"Cannot encode a polymorphic block with free capture parameters: ${freeCaptures}")
        }

        val key = DeBruijn(tpe, outer)
        val encoding = encodings.getOrElse(key, {
          val interface = Id("Poly")
          val method = Id("apply")
          val freshParams = outer.map(param => Id(param.name.name))
          val substitution = effekt.util.DB.from(
            outer.zip(freshParams.map(ValueType.Var.apply))
          )
          val methodType = Type.substitute(tpe, substitution, effekt.util.DB.empty)
          interfaces += Declaration.Interface(
            interface,
            freshParams,
            List(Property(method, methodType))
          )

          val fresh = Encoding(interface, method)
          encodings += key -> fresh
          fresh
        })

        Encoded(encoding, outer)
      }
    }

    private case class Scope(
      typeParams: List[Id] = Nil,
      captureParams: List[Id] = Nil,
      blocks: Map[Id, Encoded] = Map.empty
    ) {
      def bind(tparams: List[Id], cparams: List[Id]): Scope =
        copy(
          typeParams = typeParams ++ tparams,
          captureParams = captureParams ++ cparams
        )

      def bind(blocks: Map[Id, Encoded]): Scope =
        copy(blocks = this.blocks ++ blocks)
    }

    private class Elaborator(state: State)(using Context) extends Tree.RewriteWithContext[Scope] {

      override def rewrite(declaration: Declaration)(using scope: Scope): Declaration = declaration match {
        case Declaration.Interface(id, tparams, properties) =>
          val local = scope.bind(tparams, Nil)
          Declaration.Interface(id, tparams, properties.map(rewrite(_)(using local)))

        case Declaration.Data(id, tparams, constructors) =>
          val local = scope.bind(tparams, Nil)
          Declaration.Data(id, tparams, constructors.map {
            case Constructor(tag, existentialParams, fields) =>
              val constructorScope = local.bind(existentialParams, Nil)
              Constructor(tag, existentialParams, fields.map {
                case Field(field, tpe) => Field(field, rewrite(tpe)(using constructorScope))
              })
          })
      }

      override def rewrite(operation: Operation)(using scope: Scope): Operation = operation match {
        case Operation(name, tparams, cparams, vparams, bparams, body) =>
          val local = scope.bind(tparams, cparams)
          val (rewrittenParams, encoded) = rewriteParameters(bparams, local)
          Operation(
            name,
            tparams,
            cparams,
            vparams.map(rewrite(_)(using local)),
            rewrittenParams,
            rewrite(body)(using local.bind(encoded))
          )
      }

      override def rewrite(block: BlockLit)(using scope: Scope): BlockLit = block match {
        case BlockLit(tparams, cparams, vparams, bparams, body) =>
          val local = scope.bind(tparams, cparams)
          val (rewrittenParams, encoded) = rewriteParameters(bparams, local)
          BlockLit(
            tparams,
            cparams,
            vparams.map(rewrite(_)(using local)),
            rewrittenParams,
            rewrite(body)(using local.bind(encoded))
          )
      }

      private def rewriteParameters(
        params: List[BlockParam],
        scope: Scope
      ): (List[BlockParam], Map[Id, Encoded]) = {
        var encoded: Map[Id, Encoded] = Map.empty
        val rewritten = params.map {
          case BlockParam(id, tpe: BlockType.Function, captures) if tpe.tparams.nonEmpty =>
            val replacement = state.encode(tpe, scope)
            encoded += id -> replacement
            BlockParam(id, replacement.openInterface, captures)

          case BlockParam(id, tpe, captures) =>
            BlockParam(id, rewrite(tpe)(using scope), captures)
        }
        (rewritten, encoded)
      }

      override def rewrite(block: BlockVar)(using scope: Scope): BlockVar = block match {
        case BlockVar(id, _, captures) if scope.blocks.contains(id) =>
          BlockVar(id, scope.blocks(id).openInterface, captures)

        case BlockVar(id, tpe, captures) =>
          BlockVar(id, rewrite(tpe), captures)
      }

      override def rewrite(tpe: ValueType)(using scope: Scope): ValueType = tpe match {
        case ValueType.Var(id) => ValueType.Var(id)
        case ValueType.Data(name, targs) => ValueType.Data(name, targs.map(rewrite))
        case ValueType.Boxed(blockType, captures) =>
          ValueType.Boxed(rewrite(blockType), captures)
      }

      override def rewrite(tpe: BlockType)(using scope: Scope): BlockType = tpe match {
        case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
          val local = scope.bind(tparams, cparams)
          BlockType.Function(
            tparams,
            cparams,
            vparams.map(rewrite(_)(using local)),
            bparams.map {
              case polymorphic: BlockType.Function if polymorphic.tparams.nonEmpty =>
                state.encode(polymorphic, local).openInterface
              case other =>
                rewrite(other)(using local)
            },
            rewrite(result)(using local)
          )

        case BlockType.Interface(name, targs) =>
          BlockType.Interface(name, targs.map(rewrite))
      }

      override def rewrite(stmt: Stmt)(using scope: Scope): Stmt = stmt match {
        case App(callee, targs, vargs, bargs) =>
          val function = callee.functionType
          val rewrittenTargs = targs.map(rewrite)
          val rewrittenBargs = rewriteArguments(
            bargs,
            function,
            rewrittenTargs,
            scope
          )
          val rewrittenVargs = vargs.map(rewrite)

          callee match {
            case variable @ BlockVar(id, _, _) if scope.blocks.contains(id) =>
              val encoding = scope.blocks(id).encoding
              Invoke(
                rewrite(variable),
                encoding.method,
                rewrite(function),
                rewrittenTargs,
                rewrittenVargs,
                rewrittenBargs
              )

            case _ =>
              App(rewrite(callee), rewrittenTargs, rewrittenVargs, rewrittenBargs)
          }

        case Invoke(callee, method, methodType: BlockType.Function, targs, vargs, bargs) =>
          val rewrittenTargs = targs.map(rewrite)
          Invoke(
            rewrite(callee),
            method,
            rewrite(methodType),
            rewrittenTargs,
            vargs.map(rewrite),
            rewriteArguments(bargs, methodType, rewrittenTargs, scope)
          )

        case other =>
          super.rewrite(other)
      }

      private def rewriteArguments(
        arguments: List[Block],
        function: BlockType.Function,
        targs: List[ValueType],
        scope: Scope
      ): List[Block] = {
        assert(arguments.size == function.bparams.size)

        val signatureScope = scope.bind(function.tparams, function.cparams)
        val substitution = function.tparams.zip(targs).toMap

        arguments.zip(function.bparams).map {
          case (argument, polymorphic: BlockType.Function) if polymorphic.tparams.nonEmpty =>
            val encoded = state.encode(polymorphic, signatureScope)
            val interfaceArgs = encoded.outer.map { id =>
              substitution.getOrElse(id, ValueType.Var(id))
            }
            rewriteBlock(
              rewrite(argument)(using scope),
              encoded.interface(interfaceArgs),
              encoded.encoding
            )

          case (argument, _) =>
            rewrite(argument)(using scope)
        }
      }

      private def rewriteBlock(
        block: Block,
        target: BlockType.Interface,
        encoding: Encoding
      ): Block = block match {
        case BlockLit(tparams, cparams, vparams, bparams, body) =>
          New(Implementation(
            target,
            List(Operation(encoding.method, tparams, cparams, vparams, bparams, body))
          ))

        case block @ BlockVar(_, BlockType.Interface(name, _), _) if name == target.name =>
          block

        case block @ New(Implementation(interface, _)) if interface.name == target.name =>
          block

        case other =>
          Context.abort(pretty"Expected a polymorphic block literal, but found '${other}'")
      }
    }

    def apply(module: ModuleDecl)(using Context): ModuleDecl = module match {
      case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
        val state = new State
        val elaborator = new Elaborator(state)
        val initial = Scope()

        val rewrittenDeclarations = declarations.map(elaborator.rewrite(_)(using initial))
        val rewrittenDefinitions = definitions.map(elaborator.rewrite(_)(using initial))

        var index = 0
        while (index < state.interfaces.size) {
          state.interfaces(index) =
            elaborator.rewrite(state.interfaces(index))(using initial)
              .asInstanceOf[Declaration.Interface]
          index += 1
        }

        ModuleDecl(
          path,
          includes,
          rewrittenDeclarations ++ state.interfaces,
          externs,
          rewrittenDefinitions,
          exports
        )
    }
  }

  object collect {
    type TypeParams = Map[Id, TypeArg.Var]

    class Context {
      var typingContext: TypeParams = Map()

      def extendTypingContext(tparam: Id, index: Int, owner: FlowVar) =
        typingContext += (tparam -> TypeArg.Var(owner, index))
    }

    def apply(module: ModuleDecl): Flows = module match
      case ModuleDecl(_, _, declarations, externs, definitions, _) =>
        given Context = new Context
        findConstraints(definitions) ++
          externs.flatMap(findConstraints) ++
          declarations.flatMap(findConstraints)

    def findConstraints(definitions: List[Toplevel])(using Context): Flows =
      definitions flatMap findConstraints

    def findConstraints(definition: Toplevel)(using ctx: Context): Flows = definition match
      case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
        findConstraints(body)
      case Toplevel.Def(id, block) =>
        findConstraints(block)
      case Toplevel.Val(id, binding) =>
        findConstraints(binding)

    def findConstraints(declaration: Declaration)(using ctx: Context): Flows = declaration match
      // Maybe[T] { Just[](x: T) }
      case Data(id, tparams, constructors) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
        constructors.map { constr =>
          val arity = tparams.size
          val constructorArgs = (0 until arity).map(index =>
            TypeArg.Var(constr.id, index) // Just.0
          ).toVector // < Just.0 >
          Flow(constructorArgs, id) // < Just.0 > <: Maybe
        }.filter(_.from.nonEmpty)
      case Interface(id, tparams, properties) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
        properties.map { prop =>
          val arity = tparams.size
          val propArgs = (0 until arity).map(index =>
            TypeArg.Var(prop.id, index) // Just.0
          ).toVector // < Just.0 >
          Flow(propArgs, id) // < Just.0 > <: Maybe
        }.filter(_.from.nonEmpty) ++ (properties flatMap findConstraints)

    def findConstraints(extern: Extern)(using ctx: Context): Flows = extern match {
      case Extern.Def(id, qualifiedSignature, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
        val (_, constraints) = findConstraints(vparams.map(_.tpe))
        constraints
      case Extern.Data(id, tparams, body) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
        List()
      case Extern.Interface(id, tparams, body) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
        List()
      case Extern.Include(_, _) => List()
    }

    def findConstraints(property: Property)(using ctx: Context): Flows = property match {
      case Property(id, tpe@BlockType.Function(tparams, cparams, vparams, bparams, result)) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
        findConstraints(tpe, id)
      case Property(id, tpe@BlockType.Interface(name, targs)) => findConstraints(tpe)
    }

    def findConstraints(block: Block)(using ctx: Context): Flows = block match
      case BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt) => findConstraints(annotatedTpe)
      case BlockVar(id, annotatedTpe: BlockType.Function, annotatedCapt) => findConstraints(annotatedTpe, id)
      case BlockLit(tparams, cparams, vparams, bparams, body) => findConstraints(body)
      case Unbox(pure) => findConstraints(pure)
      case New(impl) => findConstraints(impl)

    def findConstraints(blockType: BlockType.Interface)(using ctx: Context): Flows = blockType match
      case BlockType.Interface(name, targs) =>
        val (newTargs, constraints) = findConstraints(targs)
        List(Flow(newTargs.toVector, name)) ++ constraints

    def findConstraints(blockType: BlockType.Function, owner: FlowVar)(using ctx: Context): Flows = blockType match
      case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, owner))
        List()

    def findConstraints(impl: Implementation)(using ctx: Context): Flows = impl match
      case Implementation(interface, operations) =>
        findConstraints(interface) ++
        (operations flatMap findConstraints)

    def findConstraints(operation: Operation)(using ctx: Context): Flows = operation match
      case Operation(name, tparams, cparams, vparams, bparams, body) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, name))
        findConstraints(body)

    def findConstraints(clause: (Id, BlockLit))(using ctx: Context): Flows = clause match
      case (id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
        findConstraints(body)

    def findConstraints(stmt: Stmt)(using ctx: Context): Flows = stmt match
      case Let(id, binding, body) => findConstraints(binding) ++ findConstraints(body)
      case Return(expr) => findConstraints(expr)
      case Val(id, binding, body) => findConstraints(binding) ++ findConstraints(body)
      case Var(ref, init, capture, body) => findConstraints(body)
      case ImpureApp(id, callee, targs, vargs, bargs, body) =>
        val (newTargs, constraints) = findConstraints(targs)
        List(Flow(newTargs.toVector, callee.id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ findConstraints(body) ++ constraints
      case App(callee: BlockVar, targs, vargs, bargs) =>
        val (newTargs, constraints) = findConstraints(targs)
        List(Flow(newTargs.toVector, callee.id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
      // TODO: Very specialized, but otherwise passing an id that matches in monomorphize is hard
      //       although I'm not certain any other case can even happen
      // TODO: part 2, also update the implementation in monomorphize if changing this
      case App(Unbox(ValueVar(id, annotatedType)), targs, vargs, bargs) =>
        val (newTargs, constraints) = findConstraints(targs)
        List(Flow(newTargs.toVector, id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
      case App(callee, targs, vargs, bargs) =>
        findConstraints(callee) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints)
      case Invoke(callee @ BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt), method, methodTpe, targs, vargs, bargs) =>
        val (newTargs, constraints) = findConstraints(annotatedTpe.targs ++ targs)
        List(Flow(newTargs.toVector, method)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
      case Invoke(Unbox(ValueVar(id, annotatedType)), method, methodTpe, targs, vargs, bargs) =>
        val (newTargs, constraints) = findConstraints(targs)
        List(Flow(newTargs.toVector, method)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
      case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
        findConstraints(callee) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints)
      case Reset(body) => findConstraints(body)
      case If(cond, thn, els) => findConstraints(cond) ++ findConstraints(thn) ++ findConstraints(els)
      case Def(id, BlockLit(tparams, cparams, vparams, bparams, bbody), body) =>
        tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
        findConstraints(bbody) ++ findConstraints(body)
      case Def(id, block, body) =>
        findConstraints(block) ++ findConstraints(body)
        // FIXME: Handle k as well
      case Shift(prompt, k, body) => findConstraints(prompt) ++ findConstraints(body)
      case Match(scrutinee, tpe, clauses, default) => clauses.flatMap(findConstraints) ++ findConstraints(default)
      case Resume(k, body) => findConstraints(k) ++ findConstraints(body)
      case Get(id, annotatedTpe, ref, annotatedCapt, body) => findConstraints(body)
      case Put(ref, annotatedCapt, value, body) => findConstraints(value) ++ findConstraints(body)
      case Alloc(id, init, region, body) => findConstraints(init) ++ findConstraints(body)
      case Region(body) => findConstraints(body)
      case Hole(tpe, span) => List.empty

    def findConstraints(opt: Option[Stmt])(using ctx: Context): Flows = opt match
      case None => List.empty
      case Some(stmt) => findConstraints(stmt)

    def findConstraints(expr: Expr)(using ctx: Context): Flows = expr match
      case PureApp(b, targs, vargs) =>
        val (newTargs, constraints) = findConstraints(targs)
        Flow(newTargs.toVector, b.id) :: constraints
      case ValueVar(id, annotatedType) => List.empty
      case Literal(value, annotatedType) => List.empty
      case Make(data, tag, targs, vargs) =>
        val (dataTargs, dataConstraints) = findConstraints(data.targs)
        val (newTargs, constraints) = findConstraints(data.targs ++ targs)
        List(Flow(dataTargs.toVector, data.name), Flow(newTargs.toVector, tag)) ++ // <Int> <: Just
        dataConstraints ++ constraints
      case Box(b, annotatedCapture) =>
        findConstraints(b)

    def findConstraints(vts: List[ValueType])(using ctx: Context): (List[TypeArg], Flows) = {
      val vtFindConstraints = vts map findConstraints
      val targs = vtFindConstraints.map(_._1)
      val constraints = vtFindConstraints.flatMap(_._2)
      (targs, constraints)
    }

    def findConstraints(vt: ValueType)(using ctx: Context): (TypeArg, Flows) = vt match {
      case ValueType.Boxed(tpe@BlockType.Function(tparams, cparams, vparams, bparams, result), capt) => {
        // TODO: Perhaps recurse into tpe
        // TODO: What do I do with a function type here? It does not have a name which does not work for my current findConstraints
        (TypeArg.Boxed(tpe, capt), List.empty)
      }
      case ValueType.Boxed(tpe@BlockType.Interface(name, targs), capt) => {
        val constraints = findConstraints(tpe)
        (TypeArg.Boxed(tpe, capt), constraints)
      }
      case ValueType.Data(name, targs) => {
        val (newTargs, constraints) = findConstraints(targs)
        val additionalConstraints = if (newTargs.nonEmpty) {
          List(Flow(newTargs.toVector, name))
        } else {
          List.empty
        }
        (TypeArg.Data(name, newTargs), constraints ++ additionalConstraints)
      }
      case ValueType.Var(name) => (ctx.typingContext(name), List.empty)
    }
  }

  object solve {
    def filterBounds(bounds: Map[FlowVar, Set[Vector[TypeArg]]]): Map[FlowVar, Set[Vector[Ground]]] = bounds.view.mapValues(filterNonGround).toMap

    def filterNonGround(bounds: Set[Vector[TypeArg]]): Set[Vector[Ground]] = bounds.flatMap(filterNonGround)

    def filterNonGround(bound: Vector[TypeArg]): Option[Vector[Ground]] = {
      var res: Vector[Ground] = Vector.empty
      bound.foreach({
        case TypeArg.Data(id, targs) => {
          val groundTargs = filterNonGround(targs.toVector)
          groundTargs match {
            case None => ()
            case Some(_) => res :+= TypeArg.Data(id, targs)
          }
        }
        case TypeArg.Boxed(tpe, capt) => res :+= TypeArg.Boxed(tpe, capt)
        case TypeArg.Var(owner, pos) => ()
      })
      if (res.size == bound.size) {
        Some(res)
      } else {
        None
      }
    }

    // One specific variant of a type variable
    type Variant = (FlowVar, Vector[TypeArg])
    type Variants = List[Variant]

    // Substitution of all combinations of variants of type variables
    type Substitution = Map[FlowVar, Vector[TypeArg]]
    type Substitutions = List[Substitution]

    def apply(constraints: Flows)(using Context): Solution = {
      val groupedConstraints = constraints.groupBy(c => c.to)
      var bounds = groupedConstraints.map((sym, constraints) => (sym -> constraints.map(c => c.from).toSet))

      while (true) {
        val previousBounds = bounds
        bounds.foreach((sym, tas) =>
          val bound = propagateBounds(sym, tas)
          bounds += (sym -> bound)
        )

        if (previousBounds == bounds) return filterBounds(bounds)
      }

      def propagateBounds(flowVar: FlowVar, filteredConstraints: Set[Vector[TypeArg]]): Set[Vector[TypeArg]] =
        var nbs: Set[List[TypeArg]] = Set.empty
        filteredConstraints.foreach(b =>

          def solveTypeArg(typeArg: TypeArg, substitution: Map[FlowVar, Vector[TypeArg]], taPos: Int, insideTypeConstructor: Boolean): TypeArg = typeArg match {
            case TypeArg.Data(tpe, targs) =>
              val solvedTargs = targs.zipWithIndex.map((ta, ind) => solveTypeArg(ta, substitution, ind, true))
              TypeArg.Data(tpe, solvedTargs)
            case TypeArg.Boxed(tpe, capt) => TypeArg.Boxed(tpe, capt)
            case TypeArg.Var(owner, pos) =>
              if (flowVar == owner && taPos == pos && insideTypeConstructor) Context.abort(pretty"Detected polymorphic recursion for '${flowVar}' at position '${taPos}'")
              substitution(owner)(pos)
          }

          // a => <Int, Char>, <Double, Bool>
          // b => <a.0, a.1>
          def collectBounds(typeArg: TypeArg): List[FlowVar] = typeArg match {
            case TypeArg.Var(owner, _) => List(owner)
            case TypeArg.Data(_, targs) => targs.flatMap(collectBounds)
            case _ => List()
          }
          val substitutions = b.flatMap(collectBounds).distinct.foldLeft(List(Map.empty): Substitutions) {
            case (substitutions, flowVar) =>
              val variants = bounds.getOrElse(flowVar, Set.empty).map((flowVar, _)).toList
              mapProductAppend(substitutions, variants)
          }

          substitutions.foreach(substitution => {
            val l = b.zipWithIndex.map((typeArg, ind) => {
              solveTypeArg(typeArg, substitution, ind, false)
            }).toList
            nbs += l
          })
        )
        nbs.map(l => l.toVector)

      // we will never get here
      filterBounds(bounds)
    }

    def productAppend[A](ls: List[List[A]], rs: List[A]): List[List[A]] =
      for { l <- ls; r <- rs } yield l :+ r

    // Cross product of existing substitutions and all variants for one type variable
    def mapProductAppend(ls: Substitutions, rs: Variants): List[Map[FlowVar, Vector[TypeArg]]] =
      for { l <- ls; r <- rs } yield l + r
  }

  object specialize {
    type FunctionNames = Map[(FlowVar, Vector[Ground]), Id]
    type TypeNames = collection.mutable.Map[(FlowVar, Vector[Ground]), ValueType.Data]

    case class State(solution: Solution, funNames: FunctionNames, tpeNames: TypeNames, polyExternDefs: List[Id]) {
      var replacementTparams: Map[Id, Ground] = Map.empty

      lazy val invertedTpeNames: Map[ValueType.Data, (FlowVar, Vector[Ground])] = tpeNames.map { case (k, v) => (v, k) }.toMap

      def instantiateTparams(tparams: List[Id], targs: List[Ground]) = {
        assert(targs.size == tparams.size, s"Wrong number of type arguments\n  targs: ${targs}\n  tparams: ${tparams}")
        replacementTparams ++= tparams.zip(targs).toMap
      }

      def isPolyExtern(id: Id) = polyExternDefs.contains(id)
    }

    def apply(module: ModuleDecl, solution: Solution)(using Context): ModuleDecl = module match
      case ModuleDecl(path, includes, declarations, externs, definitions, exports) =>
        val dctx = DeclarationContext(declarations, externs)
        var monoFunNames: FunctionNames = Map.empty
        val monoTpeNames: TypeNames = collection.mutable.Map.empty
        solution.foreach((id, targs) =>
          if (dctx.findExternDef(id).isDefined) {
            targs.foreach(vb => monoFunNames += ((id, vb) -> id))
          } else if (dctx.findData(id).isDefined) {
            val data = dctx.findData(id).get
            targs.foreach(vb => freshMonoTypeName(data.id, vb, monoTpeNames))
          } else {
            targs.foreach(vb => monoFunNames += ((id, vb) -> freshMonoName(id, vb)))
          }
        )

        declarations.foreach {
          case Data(id, List(), _) => monoTpeNames += ((id, Vector.empty) -> ValueType.Data(id, List.empty))
          case Interface(id, List(), _) => monoFunNames += ((id, Vector.empty) -> id)
          case _ => ()
        }

        val polyExternDefs: List[Id] = externs.collect {
          case Extern.Def(id, _, tparams, _, _, _, _, _, _) if tparams.nonEmpty => id
        }
        val monoContext = State(solution, monoFunNames, monoTpeNames, polyExternDefs)
        val monoDecls = declarations.flatMap(monomorphize(_)(using monoContext)(using dctx))
        val monoDefs = monomorphize(definitions)(using monoContext)(using Context, dctx)
        ModuleDecl(path, includes, monoDecls, externs, monoDefs, exports)

    def monomorphize(definitions: List[Toplevel])(using ctx: State)(using Context, DeclarationContext): List[Toplevel] =
      var newDefinitions: List[Toplevel] = List.empty
      definitions.foreach(definition => newDefinitions ++= monomorphize(definition))
      newDefinitions

    def monomorphize(toplevel: Toplevel)(using ctx: State)(using Context, DeclarationContext): List[Toplevel] = toplevel match
      case Toplevel.Def(id, BlockLit(List(), cparams, vparams, bparams, body)) =>
        List(Toplevel.Def(id, Renamer.rename(BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body)))))
      case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
        val monoTypes = ctx.solution(id).toList
        monoTypes.map(baseTypes =>
          ctx.instantiateTparams(tparams, baseTypes.toList)
          Toplevel.Def(ctx.funNames(id, baseTypes), Renamer.rename(BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))))
        )
      case Toplevel.Def(id, block) =>
        List(Toplevel.Def(id, monomorphize(block)))
      case Toplevel.Val(id, binding) =>
        List(Toplevel.Val(id, monomorphize(binding)))

    def monomorphize(decl: Declaration)(using ctx: State)(using DeclarationContext): List[Declaration] = decl match
      case Data(id, tparams, constructors) =>
        val monoTypes = ctx.solution.getOrElse(id, Set.empty).toList
        if (monoTypes.isEmpty) {
          List(Data(id, tparams, constructors.flatMap(monomorphize(_, Vector.empty))))
        } else {
          monoTypes.map(baseTypes =>
            ctx.instantiateTparams(tparams, baseTypes.toList)
            Declaration.Data(ctx.tpeNames(id, baseTypes).name, List.empty, constructors.flatMap(constr => monomorphize(constr, baseTypes)))
          )
        }
      case Interface(id, tparams, properties) =>
        val monoTypes = ctx.solution.getOrElse(id, Set.empty).toList
        if (monoTypes.isEmpty) {
          List(Declaration.Interface(id, tparams, properties.flatMap(monomorphize(_, Vector.empty))))
        } else {
          monoTypes.map(baseTypes =>
            ctx.instantiateTparams(tparams, baseTypes.toList)
            val monoProp = properties.flatMap(prop => monomorphize(prop, baseTypes))
            val interfaceName = ctx.funNames(id, baseTypes)
            if (interfaceName == id) {
              Declaration.Interface(interfaceName, tparams, monoProp)
            } else {
              Declaration.Interface(interfaceName, List.empty, monoProp)
            }
          )
        }

    def monomorphize(property: Property, variant: Vector[Ground])(using ctx: State)(using DeclarationContext): List[Property] = property match {
      case Property(id, tpe@BlockType.Function(tparams, cparams, vparams, bparams, result)) => {
        val baseTypes = ctx.solution.getOrElse(id, Set.empty).toList
        val relevantTypes = baseTypes.filter(tpes => tpes.startsWith(variant))
        relevantTypes.map(baseType => {
          val existentialBaseTypes = baseType.drop(variant.size)
          ctx.instantiateTparams(tparams, existentialBaseTypes.toList)
          Property(ctx.funNames((id, baseType)), monomorphize(tpe))
        })
      }
      case Property(id, tpe) => ???
    }

    def monomorphize(constructor: Constructor, variant: Vector[Ground])(using ctx: State)(using DeclarationContext): List[Constructor] = constructor match
      case Constructor(id, tparams, fields) =>
        // All solutions for this constructor
        val baseTypes = ctx.solution.getOrElse(id, Set.empty).toList
        // Filter solutions that do not belong to the variant currently being handled
        val relevantTypes = baseTypes.filter(tpes => tpes.startsWith(variant))
        // The solutions for constructors may have more types than the variant because of existentials
        // in which case we need to generate multiple constructors
        relevantTypes.map(baseType => {
          // Remove types not relevant for existentials (mono13)
          val existentialBaseTypes = baseType.drop(variant.size)
          ctx.instantiateTparams(tparams, existentialBaseTypes.toList)
          Constructor(ctx.funNames(id, baseType), List.empty, fields map monomorphize)
        })

    def monomorphize(block: Block)(using ctx: State)(using Context, DeclarationContext): Block = block match
      case b: BlockLit => monomorphize(b)
      case b: BlockVar => monomorphize(b)
      case New(impl) => New(monomorphize(impl))
      case Unbox(pure) => Unbox(monomorphize(pure))

    def monomorphize(impl: Implementation)(using ctx: State)(using Context, DeclarationContext): Implementation = impl match
      case Implementation(BlockType.Interface(name, targs), operations) =>
        val variant = (targs map toTypeArg).toVector
        Implementation(BlockType.Interface(replacementFun(name, targs), List.empty), operations.flatMap(op => monomorphize(op, variant)))

    def monomorphize(interface: BlockType.Interface)(using ctx: State): BlockType.Interface = interface match
      case BlockType.Interface(name, targs) =>
        val funName = replacementFun(name, targs)
        BlockType.Interface(funName, List.empty)

    def monomorphize(operation: Operation, variant: Vector[Ground])(using ctx: State)(using Context, DeclarationContext): List[Operation] = operation match
      case Operation(name, tparams, cparams, vparams, bparams, body) =>
        val baseTypes = ctx.solution.getOrElse(name, Set.empty).toList
        val relevantTypes = baseTypes.filter(tpes => tpes.startsWith(variant))
        relevantTypes.map(baseTypes =>
          val existentialBaseTypes = baseTypes.drop(variant.size)
          ctx.instantiateTparams(tparams, existentialBaseTypes.toList)
          Operation(ctx.funNames(name, baseTypes), List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))
        )


    def monomorphize(block: BlockLit)(using ctx: State)(using Context, DeclarationContext): BlockLit = block match
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))

    def monomorphize(block: BlockVar)(using ctx: State)(using DeclarationContext): BlockVar = block match
      case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(id, monomorphize(annotatedTpe), annotatedCapt)

    def monomorphize(field: Field)(using ctx: State)(using DeclarationContext): Field = field match
      case Field(id, tpe) => Field(id, monomorphize(tpe))

    // FIXME: Not a big fan of this function needing so many extra parameters
    def monomorphize(blockVar: BlockVar, replacementId: Id, targs: List[ValueType])(using ctx: State)(using DeclarationContext): BlockVar = blockVar match
      case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCapt) if ctx.isPolyExtern(id) =>
        ctx.instantiateTparams(tparams, targs map toTypeArg)
        val annotatedTpe = BlockType.Function(tparams, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
        BlockVar(id, annotatedTpe, annotatedCapt)
      case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCapt) =>
        ctx.instantiateTparams(tparams, targs map toTypeArg)
        val monoAnnotatedTpe = BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
        BlockVar(replacementId, monoAnnotatedTpe, annotatedCapt)
      case BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt) =>
        BlockVar(id, monomorphize(annotatedTpe), annotatedCapt)

    def monomorphize(stmt: Stmt)(using ctx: State)(using Context, DeclarationContext): Stmt = stmt match
      case Return(expr) =>
        Return(monomorphize(expr))
      case Val(id, binding, body) =>
        Val(id, monomorphize(binding), monomorphize(body))
      case Var(ref, init, capture, body) =>
        Var(ref, monomorphize(init), capture, monomorphize(body))
      case ImpureApp(id, callee, targs, vargs, bargs, body) =>
        ImpureApp(id, callee, targs map monomorphize, vargs map monomorphize, bargs map monomorphize, monomorphize(body))
      case App(callee: BlockVar, targs, vargs, bargs) if ctx.isPolyExtern(callee.id) =>
        ctx.instantiateTparams(callee.functionType.tparams, targs map toTypeArg)
        App(callee, targs map monomorphize, vargs map monomorphize, bargs map monomorphize)
      case App(callee: BlockVar, targs, vargs, bargs) =>
        val monoFnId = replacementFun(callee.id, targs)
        App(monomorphize(callee, monoFnId, targs), List.empty, vargs map monomorphize, bargs map monomorphize)
      // TODO: Highly specialized, see todo in findConstraints for info
      //       change at the same time as findConstraints
      case App(Unbox(ValueVar(id, annotatedTpe)), targs, vargs, bargs) =>
        App(Unbox(ValueVar(id, monomorphize(annotatedTpe))), List.empty, vargs map monomorphize, bargs map monomorphize)
      case App(callee, targs, vargs, bargs) =>
        App(monomorphize(callee), List.empty, vargs map monomorphize, bargs map monomorphize)
      case Let(id, binding, body) =>
        Let(id, monomorphize(binding), monomorphize(body))
      case If(cond, thn, els) =>
        If(monomorphize(cond), monomorphize(thn), monomorphize(els))
      case Invoke(Unbox(pure), method, methodTpe, targs, vargs, bargs) =>
        Invoke(Unbox(monomorphize(pure)), method, methodTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
      case Invoke(BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt), method, BlockType.Function(tparams, cparams, vparams, bparams, result), targs, vargs, bargs) =>
        val combinedTargs = annotatedTpe.targs ++ targs
        val replacementMethod = replacementFun(method, combinedTargs)
        val monoAnnotatedTpe = BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
        Invoke(BlockVar(id, monomorphize(annotatedTpe), annotatedCapt), replacementMethod, monoAnnotatedTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
      case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
        Invoke(monomorphize(callee), method, methodTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
      case Resume(k, body) =>
        Resume(monomorphize(k), monomorphize(body))
      case Reset(body) =>
        Reset(monomorphize(body))
      case Def(id, BlockLit(List(), cparams, vparams, bparams, bbody), body) =>
        Stmt.Def(id, BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(bbody)), monomorphize(body))
      case Def(id, BlockLit(tparams, cparams, vparams, bparams, bbody), body) =>
        val monoTypes = ctx.solution(id).toList
        // Monomorphizing inner functions may yield multiple definitions
        // which then need to be nested
        def nestDefs(defnTypes: List[Vector[Ground]]): Stmt = defnTypes match {
          case head :: next =>
            ctx.instantiateTparams(tparams, head.toList)
            Stmt.Def(ctx.funNames(id, head), BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(bbody)), nestDefs(next))
          case Nil => monomorphize(body)
        }
        nestDefs(monoTypes)
      case Def(id, block, body) =>
        val monoBlock = monomorphize(block)
        val monoBody = monomorphize(body)
        Def(id, monoBlock, monoBody)
      case Shift(prompt, k, body) =>
        Shift(monomorphize(prompt), monomorphize(k), monomorphize(body))
      case Match(scrutinee, matchTpe, clauses, default) =>
        // We need the type of the scrutinee, to be able to only monomorphize the cases to this variant
        val monoScrut = monomorphize(scrutinee)
        val variant = monoScrut.tpe match {
          // Get the type of this variant by inverting the monomorphized name of the scrutinee
          case t: ValueType.Data => ctx.invertedTpeNames.getOrElse(t, (t.name, Vector.empty))(1)
          // Ignore variant if we are matching on anything else (examples/pos/bidirectional/typeparametric.effekt)
          case _ => Vector.empty
        }

        Match(monoScrut, monomorphize(matchTpe), clauses.flatMap(clause => monomorphize(clause, variant)), monomorphize(default))
      case Get(id, annotatedTpe, ref, annotatedCapt, body) =>
        Get(id, monomorphize(annotatedTpe), ref, annotatedCapt, monomorphize(body))
      case Put(ref, annotatedCapt, value, body) =>
        Put(ref, annotatedCapt, monomorphize(value), monomorphize(body))
      case Alloc(id, init, region, body) =>
        Alloc(id, monomorphize(init), region, monomorphize(body))
      case Region(body) =>
        Region(monomorphize(body))
      case Hole(tpe, span) =>
        Hole(monomorphize(tpe), span)

    def monomorphize(clause: (Id, BlockLit), variant: Vector[Ground])(using ctx: State)(using Context, DeclarationContext): List[(Id, BlockLit)] = clause match
      case (id, BlockLit(tparams, cparams, vparams, bparams, body)) =>
        val baseTypes = ctx.solution.getOrElse(id, Set.empty).toList
        val relevantTypes = baseTypes.filter(tpes => tpes.startsWith(variant))
        relevantTypes.map(baseType =>
          val existentialBaseTypes = baseType.drop(variant.size)
          ctx.instantiateTparams(tparams, existentialBaseTypes.toList)
          val monoBlockLit: Block.BlockLit = BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))
          (ctx.funNames(id, baseType), monoBlockLit)
        ).toList

    def monomorphize(opt: Option[Stmt])(using ctx: State)(using Context, DeclarationContext): Option[Stmt] = opt match
      case None => None
      case Some(stmt) => Some(monomorphize(stmt))

    def monomorphize(expr: Expr)(using ctx: State)(using Context, DeclarationContext): Expr = expr match
      case Literal(value, annotatedType) =>
        Literal(value, monomorphize(annotatedType))
      case PureApp(b, targs, vargs) =>
        PureApp(b, targs map monomorphize, vargs map monomorphize)
      case Make(data, tag, targs, vargs) =>
        val combinedTargs = data.targs ++ targs
        val replacementTag = replacementFun(tag, combinedTargs)
        Make(replacementData(data.name, data.targs), replacementTag, List.empty, vargs map monomorphize)
      case Box(b, annotatedCapture) =>
        Box(monomorphize(b), annotatedCapture)
      case ValueVar(id, annotatedType) =>
        ValueVar(id, monomorphize(annotatedType))

    def monomorphize(valueParam: ValueParam)(using State, DeclarationContext): ValueParam = valueParam match
      case ValueParam(id, tpe) => ValueParam(id, monomorphize(tpe))

    def monomorphize(blockParam: BlockParam)(using State, DeclarationContext): BlockParam = blockParam match
      case BlockParam(id, tpe, capt) =>
        BlockParam(id, monomorphize(tpe), capt)

    def monomorphize(blockType: BlockType)(using ctx: State)(using DeclarationContext): BlockType = blockType match {
      case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
        BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
      case BlockType.Interface(name, targs) =>
        val funName = ctx.funNames.getOrElse((name, (targs map toTypeArg).toVector), name)
        // Special case here if we have 'Resume' or 'Prompt' we didn't change the name which we can detect here
        // then we don't change the targs for typechecking to work
        if (funName == name) {
          BlockType.Interface(funName, targs map monomorphize)
        } else {
          BlockType.Interface(funName, List.empty)
        }
    }

    def monomorphize(valueType: ValueType)(using ctx: State)(using dctx: DeclarationContext): ValueType = valueType match {
      case ValueType.Var(name) => ctx.replacementTparams(name) match {
        case TypeArg.Data(tpe, targs) => replacementData(tpe, targs.toVector)
        case TypeArg.Boxed(tpe, capt) => ValueType.Boxed(monomorphize(tpe), capt)
      }
      // We do not monomorphize targs here, because our name lookup for types is looking for
      // Option[Option[Int]] -> Option_Option_Int
      // and not
      // Option[Option_Int] -> Option_Option_Int
      case ValueType.Data(name, targs) => replacementData(name, targs)
      case ValueType.Boxed(tpe, capt) => ValueType.Boxed(monomorphize(tpe), capt)
    }

    def monomorphize(typeArg: TypeArg)(using State)(using dctx: DeclarationContext): ValueType = typeArg match {
      case TypeArg.Data(tpe, targs) =>
        dctx.findExternData(tpe) match {
          case Some(_) => {
            ValueType.Data(tpe, targs map monomorphize)
          }
          case None => {
            replacementData(tpe, targs.toVector)
          }
        }
      case TypeArg.Boxed(tpe, capt) => ValueType.Boxed(monomorphize(tpe), capt)
      case TypeArg.Var(owner, pos) =>
        // FIXME: Do we want to reflect this unreachability in the Data structure used for monomorphizing?
        //        we would need another version of TypeArg that only allows Ground arguments in Data
        throw new RuntimeException(s"All the vars should have been removed in the solving stage, still got '${typeArg}'")
    }

    def freshMonoTypeName(dataName: Id, tpes: Vector[Ground], monoTypeNames: TypeNames): ValueType.Data = {
      monoTypeNames.getOrElse((dataName, tpes), {
        val nameBuilder = StringBuilder(dataName.name.name)
        val valueTypes = tpes map {
          case TypeArg.Data(tpe, targs) => {
            // Safe `get`, because we are handling Vector[Ground] and just re-establishing this invariant,
            // because our types do not guarantee this
            val filteredTargs = solve.filterNonGround(targs.toVector).get
            val innerData = freshMonoTypeName(tpe, filteredTargs, monoTypeNames)
            nameBuilder.append("_" + innerData.name.name.name)
            innerData
          }
          case TypeArg.Boxed(tpe, capt) => {
            ValueType.Boxed(tpe, capt)
          }
        }
        val freshId = Id(nameBuilder.toString())
        val monoData: ValueType.Data = ValueType.Data(freshId, List.empty)
        monoTypeNames += ((dataName, tpes) -> monoData)
        monoData
      })
    }

    def freshMonoName(baseId: Id, tpes: Vector[Ground]): Id = {
      if (tpes.isEmpty) return baseId

      // Keep the ids of 'Resume' and 'Prompt', so we can detect this case and make typechecking work later
      // also see monomorphize(blockType: BlockType)
      if (baseId == core.Type.ResumeSymbol || baseId == core.Type.PromptSymbol) return baseId

      val tpesString = tpes.map({
        case TypeArg.Data(tpe, targs) => tpe.name.name
        // TODO: Fix naming
        case TypeArg.Boxed(tpe, capt) => "BOXED"
      }).mkString
      Id(baseId.name.name + tpesString)
    }

    def replacementFun(id: FlowVar, targs: List[ValueType])(using ctx: State): Id = {
      if (targs.isEmpty) return id
      val baseTypes: Vector[Ground] = (targs map toTypeArg).toVector
      ctx.funNames(id, baseTypes)
    }

    def replacementData(id: Id, targs: Vector[TypeArg])(using ctx: State, dctx: DeclarationContext): ValueType.Data = {
      if (targs.isEmpty) return ValueType.Data(id, List.empty)

      val groundTpes = solve.filterNonGround(targs).get
      dctx.findExternData(id) match {
        case Some(_) => ValueType.Data(id, targs.toList map monomorphize)
        case None => ctx.tpeNames((id, groundTpes))
      }
    }

    def replacementData(id: Id, targs: List[ValueType])(using ctx: State, dctx: DeclarationContext): ValueType.Data = {
      dctx.findExternData(id) match {
        case Some(_) => {
          ValueType.Data(id, targs map monomorphize)
        }
        case None => {
          val baseTypes: Vector[Ground] = (targs map toTypeArg).toVector
          replacementData(id, baseTypes)
        }
      }
    }

    def toTypeArg(vt: ValueType)(using ctx: State): Ground = vt match {
      case ValueType.Data(name, targs) => TypeArg.Data(name, targs map toTypeArg)
      case ValueType.Var(name) => ctx.replacementTparams(name)
      case ValueType.Boxed(tpe, capt) => TypeArg.Boxed(tpe, capt)
    }
  }
}
