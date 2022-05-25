package effekt

import effekt.source.NoSource
import effekt.symbols.*
import effekt.typer.*
import effekt.util.messages.{ ErrorReporter, MessageBuffer }
import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class ConstraintSetTests extends AnyFunSpec {

  given ErrorReporter with { var focus = NoSource; def buffer = new MessageBuffer }

  lazy val scope = { val s = new Unification; s.enterScope(); s }

  lazy val S = freshVariable("S")
  lazy val T = freshVariable("T")
  lazy val U = freshVariable("U")
  lazy val A = freshVariable("A")
  lazy val B = freshVariable("B")
  lazy val C = freshVariable("C")

  def freshVariable(name: String) =
    scope.fresh(UnificationVar.TypeVariableInstantiation(TypeVar(Name.local(name))))

  def freshGraph() = new ConstraintSet

  describe("connecting") {
    it("two variables in subtype relation") {
      val graph = freshGraph()

      // {S,T}
      graph.connect(S, T)

      assert(graph.isSubtypeOf(S, T))
      assert(graph.isSubtypeOf(T, S))
    }
    it("uses the bounds of the second node") {
      val graph = freshGraph()

      graph.updateLowerBound(S, builtins.TInt)
      assert(graph.lowerBound(S) == builtins.TInt)

      graph.updateLowerBound(T, builtins.TBoolean)

      graph.dumpConstraints()

      // {S,T}
      graph.connect(S, T)

      assert(graph.lowerBound(S) == builtins.TBoolean)
      assert(graph.lowerBound(T) == builtins.TBoolean)
    }
  }
}
