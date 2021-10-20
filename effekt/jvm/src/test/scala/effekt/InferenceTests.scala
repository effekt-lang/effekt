package effekt

import effekt.source.Tree
import effekt.util.messages.{ ErrorReporter, FatalPhaseError, MessageBuffer }
import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions
import effekt.symbols.builtins.{ TBoolean, TInt, TString, TUnit }
import symbols._
import effekt.symbols.{ LocalName, Name, QualifiedName }
import effekt.substitutions._

class InferenceTests extends AnyFunSpec {

  def reporter: ErrorReporter = new ErrorReporter {
    override var focus: Tree = _
    override def buffer = new MessageBuffer
  }

  implicit val C = reporter

  describe("unification") {

    it("should unify simple types") {
      val scope = new UnificationScope
      scope.requireEqual(TInt, TInt)
      val (subst, _, _) = scope.solve
      assert(subst.values.isEmpty)
    }

    it("should unifying two type variables") {
      val scope = new UnificationScope
      val T1 = scope.fresh(TypeVar(Name.local("T1")))
      val T2 = scope.fresh(TypeVar(Name.local("T2")))

      scope.requireEqual(TInt, T1)
      scope.requireEqual(T1, T2)
      val (subst, _, _) = scope.solve
      assert(subst.values(T1) == TInt)
      assert(subst.values(T2) == TInt)
    }

    it("should unify function types") {
      val scope = new UnificationScope
      val T1 = TypeVar(Name.local("T1"))
      val T2 = TypeVar(Name.local("T2"))

      scope.requireEqual(
        FunctionType(List(T1), Nil, List(T1), Nil, BoxedType(FunctionType(List(T2), Nil, List(T2), Nil, T2), Pure)),
        FunctionType(List(T2), Nil, List(T2), Nil, BoxedType(FunctionType(List(T1), Nil, List(T1), Nil, T1), Pure))
      )

      val (subst, _, _) = scope.solve
      println(subst)
    }
  }
  //      try {
  //        println(scope.solve)
  //      } catch {
  //        case e: FatalPhaseError => println(e.msg)
  //      }
}
