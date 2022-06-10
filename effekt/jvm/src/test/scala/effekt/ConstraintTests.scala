package effekt
package typer

import effekt.source.NoSource
import effekt.symbols.*
import effekt.typer.*
import effekt.util.messages.{ ErrorReporter, FatalPhaseError, MessageBuffer }
import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class ConstraintTests extends AnyFunSpec {
  var messages = new MessageBuffer
  given ErrorReporter with { var focus = NoSource; def buffer = messages }

  lazy val scope = { val s = new Unification; s.enterScope(); s }

  lazy val R = freshTypeVar("R")
  lazy val S = freshTypeVar("S")
  lazy val T = freshTypeVar("T")
  lazy val U = freshTypeVar("U")

  lazy val A = freshCaptVar("A")
  lazy val B = freshCaptVar("B")
  lazy val C = freshCaptVar("C")
  lazy val D = freshCaptVar("D")

  lazy val x = CaptureParam(Name.local("x"))
  lazy val y = CaptureParam(Name.local("y"))
  lazy val z = CaptureParam(Name.local("z"))


  def freshTypeVar(name: String) =
    scope.fresh(UnificationVar.TypeVariableInstantiation(TypeVar(Name.local(name)), NoSource))

  def freshCaptVar(name: String) =
    scope.freshCaptVar(CaptUnificationVar.VariableInstantiation(CaptureParam(Name.local(name)), NoSource))

  def freshGraph() = {
    messages = new MessageBuffer
    new Constraints
  }


  describe("Simple flow") {
    it ("should propagate captures through lower bounds") {
      val graph = freshGraph()
      import graph.*

      connect(A, B)
      requireLower(Set(x), A)

      // should propagate
      assert(A.lower == Some(Set(x)))
      assert(B.lower == Some(Set(x)))

      // should not affect upper bounds
      assert(A.upper == None)
      assert(B.upper == None)
    }

    it ("should transitively propagate captures through lower bounds") {
      val graph = freshGraph()
      import graph.*

      connect(A, B)
      connect(B, C)
      requireLower(Set(x), A)

      // should propagate
      assert(A.lower == Some(Set(x)))
      assert(B.lower == Some(Set(x)))
      assert(C.lower == Some(Set(x)))

      // should not affect upper bounds
      assert(A.upper == None)
      assert(B.upper == None)
      assert(C.upper == None)
    }

    it ("should propagate captures after connecting") {
      val graph = freshGraph()
      import graph.*

      requireLower(Set(x), A)
      connect(A, B)

      // should propagate
      assert(A.lower == Some(Set(x)))
      assert(B.lower == Some(Set(x)))

      // should not affect upper bounds
      assert(A.upper == None)
      assert(B.upper == None)
    }

    it ("should transitively propagate captures through lower bounds after connecting") {
      val graph = freshGraph()
      import graph.*

      requireLower(Set(x), A)

      connect(A, B)
      connect(B, C)


      // should propagate
      assert(A.lower == Some(Set(x)))
      assert(B.lower == Some(Set(x)))
      assert(C.lower == Some(Set(x)))

      // should not affect upper bounds
      assert(A.upper == None)
      assert(B.upper == None)
      assert(C.upper == None)
    }
  }

  describe("Errors") {

    it ("should report an error when conflicting bounds flow") {
      val graph = freshGraph()
      import graph.*

      connect(A, B)
      requireUpper(Set(y), B)

      assertThrows[FatalPhaseError] {
        requireLower(Set(x), A)
      }
    }
  }

  describe("Substitutions") {
    it ("should add a substitution for a capture variable, when leaving the scope") {
      val graph = freshGraph()
      import graph.*

      requireLower(Set(x), A)
      leave(Nil, List(A))

      assert(subst.isDefinedAt(A))
      assert(subst.get(A) == Some(CaptureSet(x)))
    }

    it ("should check consistency with already substituted variables") {
      val graph = freshGraph()
      import graph.*

      requireLower(Set(x), A)
      leave(Nil, List(A))

      connect(A, B)
      assert(B.lower == Some(Set(x)))

      requireUpper(Set(y), C)
      assertThrows[FatalPhaseError] {
        connect(A, C)
      }
    }
  }

  describe("Subtracting") {
    it ("should not propagate filtered captures into bounds") {
      val graph = freshGraph()
      import graph.*

      connect(A, B, Set(x))
      requireLower(Set(x), A)

      // should propagate
      assert(A.lower == Some(Set(x)))
      assert(B.lower == Some(Set()))

      // should not affect upper bounds
      assert(A.upper == None)
      assert(B.upper == None)
    }
    it ("should not propagate filtered existing captures into bounds") {
      val graph = freshGraph()
      import graph.*

      requireLower(Set(x), A)
      connect(A, B, Set(x))

      // should propagate
      assert(A.lower == Some(Set(x)))
      assert(B.lower == Some(Set()))

      // should not affect upper bounds
      assert(A.upper == None)
      assert(B.upper == None)
    }
    it ("should not conflict with solved substitutions") {
      val graph = freshGraph()
      import graph.*

      requireLower(Set(x), A)
      leave(Nil, List(A))

      connect(A, B, Set(x))

      // should propagate
      assert(B.lower == Some(Set()))

      // should not affect upper bounds
      assert(B.upper == None)
    }
    it ("if bounded from both sides it should still filter, appropriately") {
      val graph = freshGraph()
      import graph.*

      // before:
      //                         ({} B {*}) <: ({x, y} A {*})
      requireLower(Set(x, y), A)
      connect(B, A)

      // after:
      //   ({x, y} A {*}) [x] <: ({y} B {*}) <: ({x, y} A {*})
      connect(A, B, Set(x))

      // should propagate
      assert(B.lower == Some(Set(y)))

      // should not affect upper bounds
      assert(B.upper == None)
    }
  }
}