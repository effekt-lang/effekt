package effekt
package typer
package constraints

import effekt.source.NoSource
import effekt.symbols.*
import effekt.util.messages.{ DebugMessaging, ErrorReporter, FatalPhaseError }
import kiama.util.Positions

import scala.language.implicitConversions

abstract class ConstraintTests extends munit.FunSuite {

  object messages extends DebugMessaging

  given ErrorReporter with { var focus = NoSource; val messaging = messages; val positions = new Positions }

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
    scope.fresh(TypeParam(Name.local(name)), NoSource)

  def freshCaptVar(name: String) =
    scope.freshCaptVar(CaptUnificationVar.VariableInstantiation(CaptureParam(Name.local(name)), NoSource))

  def freshGraph() = {
    messages.clear()
    new Constraints
  }
}

class TestSimpleFlow extends ConstraintTests {

    test("should propagate captures through lower bounds") {
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

    test("should transitively propagate captures through lower bounds") {
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

    test("should propagate captures after connecting") {
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

    test("should transitively propagate captures through lower bounds after connecting") {
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

class TestErrors extends ConstraintTests {
    test("should report an error when conflicting bounds flow") {
      val graph = freshGraph()
      import graph.*

      connect(A, B)
      requireUpper(Set(y), B)

      intercept[FatalPhaseError] {
        requireLower(Set(x), A)
      }
    }
}

class TestSubstitutions extends ConstraintTests {
    test("should add a substitution for a capture variable, when leaving the scope") {
      val graph = freshGraph()
      import graph.*

      requireLower(Set(x), A)
      leave(Nil, List(A))

      assert(subst.isDefinedAt(A))
      assert(subst.get(A) == Some(CaptureSet(x)))
    }

    test("should check consistency with already substituted variables") {
      val graph = freshGraph()
      import graph.*

      requireLower(Set(x), A)
      leave(Nil, List(A))

      connect(A, B)
      assert(B.lower == Some(Set(x)))

      requireUpper(Set(y), C)
      intercept[FatalPhaseError] {
        connect(A, C)
      }
    }
}

class TestSubtracting extends ConstraintTests {
    test ("should not propagate filtered captures into bounds") {
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
    test ("should not propagate filtered existing captures into bounds") {
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
    test ("should not conflict with solved substitutions") {
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
    test ("if bounded from both sides it should still filter, appropriately") {
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
    test ("filtering from above should admit *more* capabilities, not less") {
      val graph = freshGraph()
      import graph.*

      // before:
      //   {y} A {*}
      //   {} B {x}
      requireUpper(Set(x), B)
      requireLower(Set(y), A)

      // connecting:
      //   ({y} A {*}) <[y]< ({} B {x})
      connect(A, B, Set(y))

      // should result in:
      //   ({y} A {x, y}) <[y]< ({} B {x})

      // Bounds are unchanged
      assert(A.lower == Some(Set(y)))
      assert(A.upper == Some(Set(x, y)))
      assert(B.lower == Some(Set()))
      assert(B.upper == Some(Set(x)))
    }
    test ("filtering from above transitively should admit *more* capabilities, not less") {
      val graph = freshGraph()
      import graph.*

      // before:
      //   {y,z} A {*}
      //   {}    B {*}
      //   {}    C {x}
      requireUpper(Set(x), C)
      requireLower(Set(y, z), A)

      connect(A, B, Set(y))
      connect(B, C, Set(z))

      // lower bound of A stays unchanged
      assert(A.lower == Some(Set(y, z)))
      // z flows from A to B (but y is filtered)
      assert(B.lower == Some(Set(z)))
      // everything is filtered before arriving at C
      assert(C.lower == Some(Set()))

      // upper bound of C is unchanged
      assert(C.upper == Some(Set(x)))
      // upper bound x on C flows to B, but also adds z
      assert(B.upper == Some(Set(x, z)))
      // all three flow as upper bound to A
      assert(A.upper == Some(Set(x, y, z)))
    }
}
