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

  case class Projection(owner: FlowVar, position: Int)

  enum MonoCapture {
    case Bound(level: Int, position: Int)
    case Named(id: Id)
  }

  enum MonoValueType[+V] {
    case Var(variable: V)
    case Bound(level: Int, position: Int)
    case Data(name: Id, targs: List[MonoValueType[V]])
    case Boxed(tpe: MonoBlockType[V], captures: Set[MonoCapture])
  }

  enum MonoBlockType[+V] {
    case Function(
      tarity: Int,
      carity: Int,
      vparams: List[MonoValueType[V]],
      bparams: List[MonoBlockType[V]],
      result: MonoValueType[V]
    )
    case Interface(name: Id, targs: List[MonoValueType[V]])
  }

  type FlowType = MonoValueType[Projection]
  type GroundType = MonoValueType[Nothing]

  case class Flow(from: Vector[FlowType], to: FlowVar)
  type Flows = List[Flow]
  type Solution = Map[FlowVar, Set[Vector[GroundType]]]


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
      private case class Environment(
        typeBinders: List[List[Id]],
        captureBinders: List[List[Id]]
      ) {
        def enter(tparams: List[Id], cparams: List[Id]): Environment =
          Environment(tparams :: typeBinders, cparams :: captureBinders)

        def typeIndex(id: Id): Option[(Int, Int)] = indexOf(id, typeBinders)
        def captureIndex(id: Id): Option[(Int, Int)] = indexOf(id, captureBinders)

        private def indexOf(id: Id, binders: List[List[Id]]): Option[(Int, Int)] =
          binders.zipWithIndex.collectFirst {
            case (level, depth) if level.contains(id) =>
              (depth, level.indexOf(id))
          }
      }

      def apply(tpe: core.BlockType.Function, outer: List[Id])(using Context): MonoBlockType[Nothing] =
        blockType(tpe)(using Environment(List(outer), List(Nil)))

      // Closing free variables in first-occurrence order makes the result
      // invariant under renaming both the local and the enclosing binders.
      def freeTypeVariables(tpe: core.BlockType): List[Id] =
        freeTypeVariables(tpe, Set.empty).distinct

      def freeCaptureVariables(tpe: core.BlockType): Set[Id] =
        freeCaptureVariables(tpe, Set.empty)

      private def blockType(tpe: core.BlockType)(using env: Environment, context: Context): MonoBlockType[Nothing] = tpe match {
        case core.BlockType.Function(tparams, cparams, vparams, bparams, result) =>
          given Environment = env.enter(tparams, cparams)
          MonoBlockType.Function(
            tparams.size,
            cparams.size,
            vparams.map(valueType),
            bparams.map(blockType),
            valueType(result)
          )

        case core.BlockType.Interface(name, targs) =>
          MonoBlockType.Interface(name, targs.map(valueType))
      }

      private def valueType(tpe: core.ValueType)(using env: Environment, context: Context): GroundType = tpe match {
        case core.ValueType.Var(id) =>
          env.typeIndex(id) match {
            case Some((level, position)) => MonoValueType.Bound(level, position)
            case None => Context.abort(pretty"Unbound type variable '${id}' while encoding a polymorphic block")
          }

        case core.ValueType.Data(name, targs) =>
          MonoValueType.Data(name, targs.map(valueType))

        case core.ValueType.Boxed(tpe, captures) =>
          MonoValueType.Boxed(blockType(tpe), captures.map { id =>
            env.captureIndex(id)
              .map((level, position) => MonoCapture.Bound(level, position))
              .getOrElse(MonoCapture.Named(id))
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
      private var encodings: Map[MonoBlockType[Nothing], Encoding] = Map.empty

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
    private case class Environment(
      variables: Map[Id, Projection] = Map.empty,
      typeBinders: List[List[Id]] = Nil,
      captureBinders: List[List[Id]] = Nil
    ) {
      def bind(owner: FlowVar, params: List[Id], offset: Int = 0): Environment =
        copy(variables = variables ++ params.zipWithIndex.map { case (param, index) =>
          param -> Projection(owner, offset + index)
        })

      def enter(tparams: List[Id], cparams: List[Id]): Environment =
        copy(
          typeBinders = tparams :: typeBinders,
          captureBinders = cparams :: captureBinders
        )

      def typeVariable(id: Id)(using Context): FlowType =
        indexOf(id, typeBinders) match {
          case Some((level, position)) => MonoValueType.Bound(level, position)
          case None => variables.get(id).map(MonoValueType.Var.apply).getOrElse {
            Context.abort(pretty"Unbound type variable '${id}' while collecting monomorphization flows")
          }
        }

      def capture(id: Id): MonoCapture =
        indexOf(id, captureBinders) match {
          case Some((level, position)) => MonoCapture.Bound(level, position)
          case None => MonoCapture.Named(id)
        }

      private def indexOf(id: Id, binders: List[List[Id]]): Option[(Int, Int)] =
        binders.zipWithIndex.collectFirst {
          case (level, depth) if level.contains(id) => (depth, level.indexOf(id))
        }
    }

    def apply(module: ModuleDecl)(using Context): Flows = module match {
      case ModuleDecl(_, _, declarations, externs, definitions, _) =>
        val env = Environment()
        declarations.flatMap(declaration(_, env)) ++
          externs.flatMap(extern(_, env)) ++
          definitions.flatMap(toplevel(_, env))
    }

    private def toplevel(definition: Toplevel, env: Environment)(using Context): Flows = definition match {
      case Toplevel.Def(id, literal: BlockLit) => function(id, 0, literal, env)
      case Toplevel.Def(_, binding) => block(binding, env)
      case Toplevel.Val(_, binding) => statement(binding, env)
    }

    private def declaration(declaration: Declaration, env: Environment)(using Context): Flows = declaration match {
      case Data(id, tparams, constructors) =>
        val dataEnv = env.bind(id, tparams)
        constructors.flatMap { constructor =>
          val outerArity = tparams.size
          val memberFlow =
            if outerArity == 0 then Nil
            else List(Flow(
              Vector.tabulate(outerArity)(index => MonoValueType.Var(Projection(constructor.id, index))),
              id
            ))
          val constructorEnv = dataEnv.bind(constructor.id, constructor.tparams, outerArity)
          memberFlow ++ constructor.fields.flatMap(field => valueType(field.tpe, constructorEnv)._2)
        }

      case Interface(id, tparams, properties) =>
        val interfaceEnv = env.bind(id, tparams)
        properties.flatMap { property =>
          val outerArity = tparams.size
          val memberFlow =
            if outerArity == 0 then Nil
            else List(Flow(
              Vector.tabulate(outerArity)(index => MonoValueType.Var(Projection(property.id, index))),
              id
            ))
          memberFlow ++ property.tpe.match {
            case functionType: BlockType.Function =>
              function(property.id, outerArity, functionType, interfaceEnv)
            case interfaceType: BlockType.Interface =>
              blockType(interfaceType, interfaceEnv)._2
          }
        }
    }

    private def extern(extern: Extern, env: Environment)(using Context): Flows = extern match {
      case Extern.Def(id, _, tparams, cparams, vparams, bparams, result, _, body) =>
        val local = env.bind(id, tparams)
        vparams.flatMap(param => valueType(param.tpe, local)._2) ++
          bparams.flatMap(param => blockType(param.tpe, local)._2) ++
          valueType(result, local)._2 ++ externBody(body, local)
      case Extern.Data(_, _, _) | Extern.Interface(_, _, _) | Extern.Include(_, _) => Nil
    }

    private def externBody(body: ExternBody[Expr], env: Environment)(using Context): Flows = body match {
      case ExternBody.StringExternBody(_, Template(_, arguments)) =>
        arguments.flatMap(expression(_, env))
      case ExternBody.Unsupported(_) => Nil
    }

    private def function(owner: FlowVar, offset: Int, literal: BlockLit, env: Environment)(using Context): Flows = literal match {
      case BlockLit(tparams, cparams, vparams, bparams, body) =>
        val local = env.bind(owner, tparams, offset)
        parameters(vparams, bparams, local) ++ statement(body, local)
    }

    private def function(owner: FlowVar, offset: Int, tpe: BlockType.Function, env: Environment)(using Context): Flows = tpe match {
      case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
        val local = env.bind(owner, tparams, offset)
        valueTypes(vparams, local)._2 ++
          bparams.flatMap(blockType(_, local)._2) ++
          valueType(result, local)._2
    }

    private def parameters(vparams: List[ValueParam], bparams: List[BlockParam], env: Environment)(using Context): Flows =
      vparams.flatMap(param => valueType(param.tpe, env)._2) ++
        bparams.flatMap(param => blockType(param.tpe, env)._2)

    private def block(block: Block, env: Environment)(using Context): Flows = block match {
      case BlockVar(id, tpe: BlockType.Function, _) => function(id, 0, tpe, env)
      case BlockVar(_, tpe: BlockType.Interface, _) => blockType(tpe, env)._2
      case literal @ BlockLit(tparams, cparams, vparams, bparams, body) =>
        if tparams.nonEmpty then
          Context.abort(pretty"Anonymous polymorphic block reached monomorphization: ${literal}")
        parameters(vparams, bparams, env) ++ statement(body, env)
      case Unbox(pure) => expression(pure, env)
      case New(implementation) => implementationFlows(implementation, env)
    }

    private def implementationFlows(implementation: Implementation, env: Environment)(using Context): Flows = implementation match {
      case Implementation(interface, operations) =>
        blockType(interface, env)._2 ++
          operations.flatMap(operation(_, interface.targs.size, env))
    }

    private def operation(operation: Operation, offset: Int, env: Environment)(using Context): Flows = operation match {
      case Operation(name, tparams, cparams, vparams, bparams, body) =>
        val local = env.bind(name, tparams, offset)
        parameters(vparams, bparams, local) ++ statement(body, local)
    }

    private def statement(stmt: Stmt, env: Environment)(using Context): Flows = stmt match {
      case Def(id, literal: BlockLit, body) =>
        function(id, 0, literal, env) ++ statement(body, env)
      case Def(_, binding, body) => block(binding, env) ++ statement(body, env)
      case Let(_, binding, body) => expression(binding, env) ++ statement(body, env)
      case ImpureApp(_, callee, targs, vargs, bargs, body) =>
        application(callee, targs, vargs, bargs, env) ++ statement(body, env)
      case Return(expr) => expression(expr, env)
      case Val(_, binding, body) => statement(binding, env) ++ statement(body, env)
      case App(callee, targs, vargs, bargs) => application(callee, targs, vargs, bargs, env)
      case Invoke(callee, method, methodTpe: BlockType.Function, targs, vargs, bargs) =>
        val receiverTargs = callee.tpe match {
          case BlockType.Interface(_, arguments) => arguments
          case other => Context.abort(pretty"Expected an interface receiver, but found '${other}'")
        }
        val (arguments, argumentFlows) = valueTypes(receiverTargs ++ targs, env)
        Flow(arguments.toVector, method) ::
          (argumentFlows ++
            function(method, receiverTargs.size, methodTpe, env) ++
            block(callee, env) ++
            vargs.flatMap(expression(_, env)) ++
            bargs.flatMap(block(_, env)))
      case Invoke(_, _, other, _, _, _) =>
        Context.abort(pretty"Expected a function method type, but found '${other}'")
      case If(cond, thn, els) =>
        expression(cond, env) ++ statement(thn, env) ++ statement(els, env)
      case Match(scrutinee, resultType, clauses, default) =>
        val outerArity = scrutinee.tpe match {
          case ValueType.Data(_, targs) => targs.size
          case _ => 0
        }
        expression(scrutinee, env) ++ valueType(resultType, env)._2 ++
          clauses.flatMap(clause(_, outerArity, env)) ++
          default.toList.flatMap(statement(_, env))
      case Region(body) => block(body, env)
      case Alloc(_, init, _, body) => expression(init, env) ++ statement(body, env)
      case Var(_, init, _, body) => expression(init, env) ++ statement(body, env)
      case Get(_, annotatedType, _, _, body) => valueType(annotatedType, env)._2 ++ statement(body, env)
      case Put(_, _, value, body) => expression(value, env) ++ statement(body, env)
      case Reset(body) => block(body, env)
      case Shift(prompt, continuation, body) =>
        block(prompt, env) ++ blockType(continuation.tpe, env)._2 ++ statement(body, env)
      case Resume(continuation, body) => block(continuation, env) ++ statement(body, env)
      case Hole(tpe, _) => valueType(tpe, env)._2
    }

    private def clause(clause: (Id, BlockLit), offset: Int, env: Environment)(using Context): Flows = clause match {
      case (constructor, BlockLit(tparams, cparams, vparams, bparams, body)) =>
        val local = env.bind(constructor, tparams, offset)
        parameters(vparams, bparams, local) ++ statement(body, local)
    }

    private def application(
      callee: Block,
      targs: List[ValueType],
      vargs: List[Expr],
      bargs: List[Block],
      env: Environment
    )(using Context): Flows = {
      val owner = callee match {
        case BlockVar(id, _, _) => Some(id)
        case Unbox(ValueVar(id, _)) => Some(id)
        case _ => None
      }
      val (arguments, argumentFlows) = valueTypes(targs, env)
      val calleeFlows = owner match {
        case Some(id) => function(id, 0, callee.functionType, env) ++ (callee match {
          case Unbox(pure) => expression(pure, env)
          case _ => Nil
        })
        case None if targs.nonEmpty =>
          Context.abort(pretty"Polymorphic application has no stable flow variable: ${callee}")
        case None => block(callee, env)
      }
      val callFlow = owner.map(id => Flow(arguments.toVector, id)).toList
      callFlow ++ argumentFlows ++ calleeFlows ++
        vargs.flatMap(expression(_, env)) ++ bargs.flatMap(block(_, env))
    }

    private def expression(expr: Expr, env: Environment)(using Context): Flows = expr match {
      case PureApp(callee, targs, vargs) => application(callee, targs, vargs, Nil, env)
      case ValueVar(_, annotatedType) => valueType(annotatedType, env)._2
      case Literal(_, annotatedType) => valueType(annotatedType, env)._2
      case Make(data, tag, targs, vargs) =>
        val (_, dataFlows) = valueType(data, env)
        val (constructorArgs, constructorFlows) = valueTypes(data.targs ++ targs, env)
        val dataDemand = if data.targs.isEmpty then List(Flow(Vector.empty, data.name)) else Nil
        Flow(constructorArgs.toVector, tag) ::
          (dataDemand ++ dataFlows ++ constructorFlows ++ vargs.flatMap(expression(_, env)))
      case Box(value, _) => block(value, env)
    }

    private def valueTypes(tpes: List[ValueType], env: Environment)(using Context): (List[FlowType], Flows) = {
      val results = tpes.map(valueType(_, env))
      (results.map(_._1), results.flatMap(_._2))
    }

    private def valueType(tpe: ValueType, env: Environment)(using Context): (FlowType, Flows) = tpe match {
      case ValueType.Var(id) => (env.typeVariable(id), Nil)
      case ValueType.Data(name, targs) =>
        val (arguments, flows) = valueTypes(targs, env)
        val demand = if arguments.isEmpty then Nil else List(Flow(arguments.toVector, name))
        (MonoValueType.Data(name, arguments), flows ++ demand)
      case ValueType.Boxed(tpe, captures) =>
        val (boxedType, flows) = blockType(tpe, env)
        (MonoValueType.Boxed(boxedType, captures.map(env.capture)), flows)
    }

    private def blockType(tpe: BlockType, env: Environment)(using Context): (MonoBlockType[Projection], Flows) = tpe match {
      case BlockType.Interface(name, targs) =>
        val (arguments, flows) = valueTypes(targs, env)
        (MonoBlockType.Interface(name, arguments), Flow(arguments.toVector, name) :: flows)
      case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
        val local = env.enter(tparams, cparams)
        val valueResults = vparams.map(valueType(_, local))
        val blockResults = bparams.map(blockType(_, local))
        val resultType = valueType(result, local)
        (
          MonoBlockType.Function(
            tparams.size,
            cparams.size,
            valueResults.map(_._1),
            blockResults.map(_._1),
            resultType._1
          ),
          valueResults.flatMap(_._2) ++ blockResults.flatMap(_._2) ++ resultType._2
        )
    }
  }

  object solve {
    def filterBounds(bounds: Map[FlowVar, Set[Vector[FlowType]]]): Solution =
      bounds.view.mapValues(filterNonGround).toMap

    def filterNonGround(bounds: Set[Vector[FlowType]]): Set[Vector[GroundType]] =
      bounds.flatMap(filterNonGround)

    def filterNonGround(bound: Vector[FlowType]): Option[Vector[GroundType]] =
      sequence(bound.map(groundValueType(_, Nil, Nil)))

    private def groundValueType(tpe: FlowType, typeBinders: List[Int], captureBinders: List[Int]): Option[GroundType] = tpe match {
      case MonoValueType.Var(_) => None
      case MonoValueType.Bound(level, position) =>
        Option.when(isBound(level, position, typeBinders))(MonoValueType.Bound(level, position))
      case MonoValueType.Data(name, targs) =>
        sequence(targs.map(groundValueType(_, typeBinders, captureBinders))).map(targs => MonoValueType.Data(name, targs.toList))
      case MonoValueType.Boxed(tpe, captures) =>
        for {
          groundType <- groundBlockType(tpe, typeBinders, captureBinders)
          if captures.forall {
            case MonoCapture.Bound(level, position) => isBound(level, position, captureBinders)
            case MonoCapture.Named(_) => true
          }
        } yield MonoValueType.Boxed(groundType, captures)
    }

    private def groundBlockType(tpe: MonoBlockType[Projection], typeBinders: List[Int], captureBinders: List[Int]): Option[MonoBlockType[Nothing]] = tpe match {
      case MonoBlockType.Function(tarity, carity, vparams, bparams, result) =>
        val localTypes = tarity :: typeBinders
        val localCaptures = carity :: captureBinders
        for {
          groundVparams <- sequence(vparams.map(groundValueType(_, localTypes, localCaptures)))
          groundBparams <- sequence(bparams.map(groundBlockType(_, localTypes, localCaptures)))
          groundResult <- groundValueType(result, localTypes, localCaptures)
        } yield MonoBlockType.Function(tarity, carity, groundVparams.toList, groundBparams.toList, groundResult)
      case MonoBlockType.Interface(name, targs) =>
        sequence(targs.map(groundValueType(_, typeBinders, captureBinders))).map(targs => MonoBlockType.Interface(name, targs.toList))
    }

    private def sequence[A](values: Iterable[Option[A]]): Option[Vector[A]] =
      values.foldLeft(Option(Vector.empty[A])) {
        case (Some(result), Some(value)) => Some(result :+ value)
        case _ => None
      }

    private def isBound(level: Int, position: Int, binders: List[Int]): Boolean =
      level >= 0 && position >= 0 && binders.lift(level).exists(position < _)

    // One specific variant of a type variable
    type Variant = (FlowVar, Vector[FlowType])
    type Variants = List[Variant]

    // Substitution of all combinations of variants of type variables
    type Substitution = Map[FlowVar, Vector[FlowType]]
    type Substitutions = List[Substitution]

    def apply(constraints: Flows)(using Context): Solution = {
      val initial = constraints.groupMap(_.to)(_.from).view.mapValues(_.toSet).toMap
      filterBounds(fixedPoint(initial))
    }

    @annotation.tailrec
    private def fixedPoint(bounds: Map[FlowVar, Set[Vector[FlowType]]])(using Context): Map[FlowVar, Set[Vector[FlowType]]] = {
      val next = bounds.map { case (owner, variants) =>
        owner -> propagate(owner, variants, bounds)
      }
      if next == bounds then bounds else fixedPoint(next)
    }

    private def propagate(
      owner: FlowVar,
      variants: Set[Vector[FlowType]],
      bounds: Map[FlowVar, Set[Vector[FlowType]]]
    )(using Context): Set[Vector[FlowType]] = {
      rejectGrowingRecursion(owner, variants)
      val result = variants.flatMap { variant =>
        val substitutions = dependencies(variant).foldLeft(List(Map.empty): Substitutions) {
          case (substitutions, dependency) =>
            val alternatives = bounds.getOrElse(dependency, Set.empty).map(dependency -> _).toList
            mapProductAppend(substitutions, alternatives)
        }
        substitutions.map(substitution => variant.map(substitute(_, substitution)))
      }
      rejectGrowingRecursion(owner, result)
      result
    }

    private def substitute(tpe: FlowType, substitution: Substitution): FlowType = tpe match {
      case MonoValueType.Var(Projection(owner, position)) => substitution(owner)(position)
      case MonoValueType.Bound(level, position) => MonoValueType.Bound(level, position)
      case MonoValueType.Data(name, targs) =>
        MonoValueType.Data(name, targs.map(substitute(_, substitution)))
      case MonoValueType.Boxed(tpe, captures) =>
        MonoValueType.Boxed(substitute(tpe, substitution), captures)
    }

    private def substitute(tpe: MonoBlockType[Projection], substitution: Substitution): MonoBlockType[Projection] = tpe match {
      case MonoBlockType.Function(tarity, carity, vparams, bparams, result) =>
        MonoBlockType.Function(
          tarity,
          carity,
          vparams.map(substitute(_, substitution)),
          bparams.map(substitute(_, substitution)),
          substitute(result, substitution)
        )
      case MonoBlockType.Interface(name, targs) =>
        MonoBlockType.Interface(name, targs.map(substitute(_, substitution)))
    }

    private def dependencies(variant: Vector[FlowType]): List[FlowVar] =
      variant.flatMap(dependencies).distinct.toList

    private def dependencies(tpe: FlowType): List[FlowVar] = tpe match {
      case MonoValueType.Var(Projection(owner, _)) => List(owner)
      case MonoValueType.Bound(_, _) => Nil
      case MonoValueType.Data(_, targs) => targs.flatMap(dependencies)
      case MonoValueType.Boxed(tpe, _) => dependencies(tpe)
    }

    private def dependencies(tpe: MonoBlockType[Projection]): List[FlowVar] = tpe match {
      case MonoBlockType.Function(_, _, vparams, bparams, result) =>
        vparams.flatMap(dependencies) ++ bparams.flatMap(dependencies) ++ dependencies(result)
      case MonoBlockType.Interface(_, targs) => targs.flatMap(dependencies)
    }

    private def rejectGrowingRecursion(owner: FlowVar, variants: Set[Vector[FlowType]])(using Context): Unit =
      if variants.exists(_.exists(grows(owner, _, guarded = false))) then
        Context.abort(pretty"Detected polymorphic recursion for '${owner}'")

    private def grows(owner: FlowVar, tpe: FlowType, guarded: Boolean): Boolean = tpe match {
      case MonoValueType.Var(Projection(candidate, _)) => guarded && candidate == owner
      case MonoValueType.Bound(_, _) => false
      case MonoValueType.Data(_, targs) => targs.exists(grows(owner, _, guarded = true))
      case MonoValueType.Boxed(tpe, _) => grows(owner, tpe)
    }

    private def grows(owner: FlowVar, tpe: MonoBlockType[Projection]): Boolean = tpe match {
      case MonoBlockType.Function(_, _, vparams, bparams, result) =>
        vparams.exists(grows(owner, _, guarded = true)) ||
          bparams.exists(grows(owner, _)) ||
          grows(owner, result, guarded = true)
      case MonoBlockType.Interface(_, targs) => targs.exists(grows(owner, _, guarded = true))
    }

    def productAppend[A](ls: List[List[A]], rs: List[A]): List[List[A]] =
      for { l <- ls; r <- rs } yield l :+ r

    // Cross product of existing substitutions and all variants for one type variable
    def mapProductAppend(ls: Substitutions, rs: Variants): List[Substitution] =
      for { l <- ls; r <- rs } yield l + r
  }

  object specialize {
    type FunctionNames = Map[(FlowVar, Vector[GroundType]), Id]
    type TypeNames = collection.mutable.Map[(FlowVar, Vector[GroundType]), ValueType.Data]

    case class State(solution: Solution, funNames: FunctionNames, tpeNames: TypeNames, polyExternDefs: List[Id]) {
      var replacementTparams: Map[Id, GroundType] = Map.empty

      lazy val invertedTpeNames: Map[ValueType.Data, (FlowVar, Vector[GroundType])] = tpeNames.map { case (k, v) => (v, k) }.toMap

      def instantiateTparams(tparams: List[Id], targs: List[GroundType]) = {
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

    def monomorphize(property: Property, variant: Vector[GroundType])(using ctx: State)(using DeclarationContext): List[Property] = property match {
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

    def monomorphize(constructor: Constructor, variant: Vector[GroundType])(using ctx: State)(using DeclarationContext): List[Constructor] = constructor match
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
        val variant = (targs map toGroundType).toVector
        Implementation(BlockType.Interface(replacementFun(name, targs), List.empty), operations.flatMap(op => monomorphize(op, variant)))

    def monomorphize(interface: BlockType.Interface)(using ctx: State): BlockType.Interface = interface match
      case BlockType.Interface(name, targs) =>
        val funName = replacementFun(name, targs)
        BlockType.Interface(funName, List.empty)

    def monomorphize(operation: Operation, variant: Vector[GroundType])(using ctx: State)(using Context, DeclarationContext): List[Operation] = operation match
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
        ctx.instantiateTparams(tparams, targs map toGroundType)
        val annotatedTpe = BlockType.Function(tparams, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
        BlockVar(id, annotatedTpe, annotatedCapt)
      case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCapt) =>
        ctx.instantiateTparams(tparams, targs map toGroundType)
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
        ctx.instantiateTparams(callee.functionType.tparams, targs map toGroundType)
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
        def nestDefs(defnTypes: List[Vector[GroundType]]): Stmt = defnTypes match {
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

    def monomorphize(clause: (Id, BlockLit), variant: Vector[GroundType])(using ctx: State)(using Context, DeclarationContext): List[(Id, BlockLit)] = clause match
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
        val funName = ctx.funNames.getOrElse((name, (targs map toGroundType).toVector), name)
        // Special case here if we have 'Resume' or 'Prompt' we didn't change the name which we can detect here
        // then we don't change the targs for typechecking to work
        if (funName == name) {
          BlockType.Interface(funName, targs map monomorphize)
        } else {
          BlockType.Interface(funName, List.empty)
        }
    }

    def monomorphize(valueType: ValueType)(using ctx: State)(using dctx: DeclarationContext): ValueType = valueType match {
      case ValueType.Var(name) => monomorphize(ctx.replacementTparams(name))
      // We do not monomorphize targs here, because our name lookup for types is looking for
      // Option[Option[Int]] -> Option_Option_Int
      // and not
      // Option[Option_Int] -> Option_Option_Int
      case ValueType.Data(name, targs) => replacementData(name, targs)
      case boxed: ValueType.Boxed => monomorphize(toGroundType(boxed))
    }

    private case class Binders(
      types: List[List[Id]] = Nil,
      captures: List[List[Id]] = Nil
    ) {
      def enter(tparams: List[Id], cparams: List[Id]): Binders =
        Binders(tparams :: types, cparams :: captures)

      def typeIndex(id: Id): Option[(Int, Int)] = indexOf(id, types)
      def captureIndex(id: Id): Option[(Int, Int)] = indexOf(id, captures)
      def typeBinder(level: Int, position: Int): Id = types(level)(position)
      def captureBinder(level: Int, position: Int): Id = captures(level)(position)

      private def indexOf(id: Id, binders: List[List[Id]]): Option[(Int, Int)] =
        binders.zipWithIndex.collectFirst {
          case (level, depth) if level.contains(id) => (depth, level.indexOf(id))
        }
    }

    def monomorphize(tpe: GroundType)(using ctx: State)(using dctx: DeclarationContext): ValueType =
      monomorphize(tpe, Binders())

    private def monomorphize(tpe: GroundType, binders: Binders)(using ctx: State, dctx: DeclarationContext): ValueType = tpe match {
      case MonoValueType.Var(impossible) => impossible
      case MonoValueType.Bound(level, position) => ValueType.Var(binders.typeBinder(level, position))
      case MonoValueType.Data(name, targs) if containsBound(tpe) =>
        ValueType.Data(name, targs.map(monomorphize(_, binders)))
      case MonoValueType.Data(name, targs) =>
        replacementData(name, targs.toVector)
      case MonoValueType.Boxed(tpe, captures) =>
        ValueType.Boxed(monomorphize(tpe, binders), captures.map {
          case MonoCapture.Bound(level, position) => binders.captureBinder(level, position)
          case MonoCapture.Named(id) => id
        })
    }

    private def monomorphize(tpe: MonoBlockType[Nothing], binders: Binders)(using ctx: State, dctx: DeclarationContext): BlockType = tpe match {
      case MonoBlockType.Function(tarity, carity, vparams, bparams, result) =>
        val tparams = List.tabulate(tarity)(position => Id(s"A${position}"))
        val cparams = List.tabulate(carity)(position => Id(s"c${position}"))
        val local = binders.enter(tparams, cparams)
        BlockType.Function(
          tparams,
          cparams,
          vparams.map(monomorphize(_, local)),
          bparams.map(monomorphize(_, local)),
          monomorphize(result, local)
        )
      case MonoBlockType.Interface(name, targs) if targs.exists(containsBound) =>
        BlockType.Interface(name, targs.map(monomorphize(_, binders)))
      case MonoBlockType.Interface(name, targs) =>
        val replacement = ctx.funNames.getOrElse((name, targs.toVector), name)
        if replacement == name then BlockType.Interface(name, targs.map(monomorphize(_, binders)))
        else BlockType.Interface(replacement, Nil)
    }

    private def containsBound(tpe: GroundType): Boolean = tpe match {
      case MonoValueType.Var(impossible) => impossible
      case MonoValueType.Bound(_, _) => true
      case MonoValueType.Data(_, targs) => targs.exists(containsBound)
      case MonoValueType.Boxed(tpe, _) => containsBound(tpe)
    }

    private def containsBound(tpe: MonoBlockType[Nothing]): Boolean = tpe match {
      case MonoBlockType.Function(_, _, vparams, bparams, result) =>
        vparams.exists(containsBound) || bparams.exists(containsBound) || containsBound(result)
      case MonoBlockType.Interface(_, targs) => targs.exists(containsBound)
    }

    def freshMonoTypeName(dataName: Id, tpes: Vector[GroundType], monoTypeNames: TypeNames): ValueType.Data = {
      monoTypeNames.getOrElse((dataName, tpes), {
        val nameBuilder = StringBuilder(dataName.name.name)
        tpes.foreach {
          case MonoValueType.Data(tpe, targs) =>
            val innerData = freshMonoTypeName(tpe, targs.toVector, monoTypeNames)
            nameBuilder.append("_" + innerData.name.name.name)
          case MonoValueType.Boxed(_, _) => nameBuilder.append("_BOXED")
          case MonoValueType.Bound(_, _) => throw new IllegalArgumentException("A free bound variable cannot name a specialization")
          case MonoValueType.Var(impossible) => impossible
        }
        val freshId = Id(nameBuilder.toString())
        val monoData: ValueType.Data = ValueType.Data(freshId, List.empty)
        monoTypeNames += ((dataName, tpes) -> monoData)
        monoData
      })
    }

    def freshMonoName(baseId: Id, tpes: Vector[GroundType]): Id = {
      if (tpes.isEmpty) return baseId

      // Keep the ids of 'Resume' and 'Prompt', so we can detect this case and make typechecking work later
      // also see monomorphize(blockType: BlockType)
      if (baseId == core.Type.ResumeSymbol || baseId == core.Type.PromptSymbol) return baseId

      val tpesString = tpes.map({
        case MonoValueType.Data(tpe, _) => tpe.name.name
        case MonoValueType.Boxed(_, _) => "BOXED"
        case MonoValueType.Bound(_, _) => throw new IllegalArgumentException("A free bound variable cannot name a specialization")
        case MonoValueType.Var(impossible) => impossible
      }).mkString
      Id(baseId.name.name + tpesString)
    }

    def replacementFun(id: FlowVar, targs: List[ValueType])(using ctx: State): Id = {
      if (targs.isEmpty) return id
      val baseTypes: Vector[GroundType] = (targs map toGroundType).toVector
      ctx.funNames(id, baseTypes)
    }

    def replacementData(id: Id, targs: Vector[GroundType])(using ctx: State, dctx: DeclarationContext): ValueType.Data = {
      if (targs.isEmpty) return ValueType.Data(id, List.empty)

      dctx.findExternData(id) match {
        case Some(_) => ValueType.Data(id, targs.toList map monomorphize)
        case None => ctx.tpeNames((id, targs))
      }
    }

    def replacementData(id: Id, targs: List[ValueType])(using ctx: State, dctx: DeclarationContext): ValueType.Data = {
      dctx.findExternData(id) match {
        case Some(_) => {
          ValueType.Data(id, targs map monomorphize)
        }
        case None => {
          val baseTypes: Vector[GroundType] = (targs map toGroundType).toVector
          replacementData(id, baseTypes)
        }
      }
    }

    def toGroundType(tpe: ValueType)(using ctx: State): GroundType =
      toGroundType(tpe, Binders())

    private def toGroundType(tpe: ValueType, binders: Binders)(using ctx: State): GroundType = tpe match {
      case ValueType.Var(name) =>
        binders.typeIndex(name) match {
          case Some((level, position)) => MonoValueType.Bound(level, position)
          case None => ctx.replacementTparams(name)
        }
      case ValueType.Data(name, targs) => MonoValueType.Data(name, targs.map(toGroundType(_, binders)))
      case ValueType.Boxed(tpe, captures) =>
        MonoValueType.Boxed(toGroundType(tpe, binders), captures.map { id =>
          binders.captureIndex(id) match {
            case Some((level, position)) => MonoCapture.Bound(level, position)
            case None => MonoCapture.Named(id)
          }
        })
    }

    private def toGroundType(tpe: BlockType, binders: Binders)(using ctx: State): MonoBlockType[Nothing] = tpe match {
      case BlockType.Function(tparams, cparams, vparams, bparams, result) =>
        val local = binders.enter(tparams, cparams)
        MonoBlockType.Function(
          tparams.size,
          cparams.size,
          vparams.map(toGroundType(_, local)),
          bparams.map(toGroundType(_, local)),
          toGroundType(result, local)
        )
      case BlockType.Interface(name, targs) =>
        MonoBlockType.Interface(name, targs.map(toGroundType(_, binders)))
    }
  }
}
