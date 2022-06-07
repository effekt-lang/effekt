package effekt
package source

import effekt.context.{ Context, ContextOps, Annotations }
import effekt.symbols._
import effekt.context.assertions._
import effekt.source.Tree.Rewrite

import effekt.typer.typeMapToSubstitution

/**
 * Transformation on source trees that translates programs into explicit capability-passing style
 *
 * That is, block parameters are introduced to bind capabilities and arguments are introduced at
 * the call sites. Resume is currently _not_ introduced as a block parameter.
 */
object CapabilityPassing extends Phase[Typechecked, Typechecked] with Rewrite {

  val phaseName = "capability-passing"

  def run(input: Typechecked)(implicit C: Context) =
    val transformedTree = rewrite(input.tree)
    Some(input.copy(tree = transformedTree))

  override def defn(implicit C: Context) = {
    case f @ FunDef(id, tps, vps, bps, ret, body) =>
      val capabilities = C.annotation(Annotations.BoundCapabilities, f)
      val capParams = capabilities.map(Context.definitionFor)
      f.copy(bparams = bps ++ capParams, body = rewrite(body))
  }

  override def expr(implicit C: Context) = {

    // an effect call -- translate to method call on the inferred capability
    case c @ Call(fun: IdTarget, targs, vargs, bargs) if fun.definition.isInstanceOf[Operation] =>
      val transformedValueArgs = vargs.map { a => rewrite(a) }
      val transformedBlockArgs = bargs.map { a => rewrite(a) }

      val capabilities = C.annotation(Annotations.CapabilityArguments, c)

      // here we use the invariant that the receiver of an effect call is the first supplied capability:
      val (List(receiver), others) = capabilities.splitAt(1)

      // the remaining capabilities are provided as arguments
      val capabilityArgs = others.map { e => InterfaceArg(C.freshReferenceTo(e)) }

      // construct the member selection on the capability as receiver
      val target = ExprTarget(Select(Var(C.freshReferenceTo(receiver)), fun.id).inheritPosition(fun))

      val typeArguments = C.annotation(Annotations.TypeArguments, c)
      val typeArgs = typeArguments.map { e => ValueTypeTree(e) }

      Call(target, typeArgs, transformedValueArgs, transformedBlockArgs ++ capabilityArgs)

    // the target is a mutable variable --> rewrite it to an expression first, then rewrite again
    case c @ Call(fun: IdTarget, targs, vargs, bargs) if fun.definition.isInstanceOf[VarBinder] =>

      val target = visit[source.CallTarget](fun) { _ =>
        val access = Var(fun.id).inheritPosition(fun)
        // heal the missing type
        // TODO refactor this
        C.annotate(Annotations.InferredValueType, access, C.valueTypeOf(fun.definition))
        ExprTarget(access)
      }
      rewrite(visit(c) { c => Call(target, targs, vargs, bargs) })

    // a "regular" function call
    // assumption: typer removed all ambiguous references, so there is exactly one
    case c @ Call(fun: IdTarget, targs, vargs, bargs) =>
      val valueArgs = vargs.map { a => rewrite(a) }
      val blockArgs = bargs.map { a => rewrite(a) }

      val capabilities = C.annotation(Annotations.CapabilityArguments, c)
      val capabilityArgs = capabilities.map { e => InterfaceArg(C.freshReferenceTo(e)) }

      val typeArguments = C.annotation(Annotations.TypeArguments, c)
      val typeArgs = typeArguments.map { e => ValueTypeTree(e) }

      Call(fun, typeArgs, valueArgs, blockArgs ++ capabilityArgs)

    // TODO share code with Call case above
    case c @ Call(ExprTarget(expr), targs, vargs, bargs) =>
      val transformedExpr = rewrite(expr)
      val valueArgs = vargs.map { a => rewrite(a) }
      val blockArgs = bargs.map { a => rewrite(a) }

      val capabilities = C.annotation(Annotations.CapabilityArguments, c)
      val capabilityArgs = capabilities.map { e => InterfaceArg(C.freshReferenceTo(e)) }

      val typeArguments = C.annotation(Annotations.TypeArguments, c)
      val typeArgs = typeArguments.map { e => ValueTypeTree(e) }

      Call(ExprTarget(transformedExpr), typeArgs, valueArgs, blockArgs ++ capabilityArgs)

    case h @ TryHandle(prog, handlers) =>
      val body = rewrite(prog)

      val capabilities = C.annotation(Annotations.BoundCapabilities, h)

      assert(capabilities.size == handlers.size)

      // here we use the invariant that the order of capabilities is the same as the order of handlers

      val hs = (handlers zip capabilities).map {
        case (h, cap) => visit(h) {
          case h @ Handler(eff, _, clauses) =>
            val cls = clauses.map { cl =>
              visit(cl) {
                case OpClause(id, params, body, resume: IdDef) =>

                  // OpClause also binds a block parameter for resume, which is _not_ annotated here
                  OpClause(id, params, rewrite(body), resume)
              }
            }

            // here we annotate the synthesized capability
            Handler(eff, Some(Context.definitionFor(cap)), cls)
        }
      }

      TryHandle(body, hs)
  }

  override def rewrite(b: source.FunctionArg)(implicit C: Context): source.FunctionArg = visit(b) {
    case b @ source.FunctionArg(tps, vps, bps, body) =>
      val capabilities = C.annotation(Annotations.BoundCapabilities, b)
      val capParams = capabilities.map(Context.definitionFor)
      source.FunctionArg(tps, vps, bps ++ capParams, rewrite(body))
  }

  /**
   * Copies all annotations and position information from source to target
   */
  override def visit[T <: Tree](source: T)(block: T => T)(implicit C: Context): T = {
    val target = block(source)
    target.inheritPosition(source)
    C.copyAnnotations(source, target)
    target
  }
}
trait CapabilityPassingOps extends ContextOps { Context: Context =>

  private[source] def freshReferenceTo(s: symbols.BlockParam): IdRef =
    val id = IdRef(s.name.name)
    assignSymbol(id, s)
    id

  private[source] def definitionFor(s: symbols.BlockParam): source.BlockParam =
    val id = IdDef(s.name.name)
    assignSymbol(id, s)
    val tree = source.BlockParam(id, source.BlockTypeTree(s.tpe))
    tree

}
