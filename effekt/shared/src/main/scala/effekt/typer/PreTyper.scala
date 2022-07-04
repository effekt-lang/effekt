package effekt
package typer

import effekt.context.{ Annotations, Context, ContextOps }
import effekt.symbols.*
import effekt.context.assertions.*
import effekt.source.{ Def, ExprTarget, IdTarget, MatchPattern, Tree }
import effekt.source.Tree.Rewrite
import effekt.typer.typeMapToSubstitution

object PreTyper extends Phase[NameResolved, NameResolved] {

  val phaseName = "pre-typer"

  def run(input: NameResolved)(implicit C: Context) = {
    val traversal = new Traversal
    val transformedTree = traversal.rewrite(input.tree)

    if (Context.buffer.hasErrors) { None }
    else { Some(input.copy(tree = transformedTree)) }
  }
}

class Traversal extends Rewrite {

  var firstClassPosition: Boolean = false
  var boxInsertionAllowed: Boolean = true

  /**
   * Copies all annotations and position information from source to target
   */
  override def visit[T <: Tree](source: T)(block: T => T)(implicit C: Context): T = {
    val positionBefore = firstClassPosition
    val insertionBefore = boxInsertionAllowed
    val target = block(source)
    firstClassPosition = positionBefore
    boxInsertionAllowed = insertionBefore
    target.inheritPosition(source)
    C.copyAnnotations(source, target)
    target
  }
}
