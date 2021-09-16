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

  describe("Foo") {
    implicit val C = reporter
    println(Unification.unify(TInt, TInt))

    val T1 = UnificationVar(TypeVar(Name.local("T1")))
    val T2 = UnificationVar(TypeVar(Name.local("T2")))

    println(Unification.unify(TInt, T1))
    val Unifier(subst, constraints) = Unification.unify(T1, TInt)

    // TODO test capture avoiding substitution
    val substituted = subst.substitute(FunctionType(Nil, List(T1), Nil, T1))
    assert(substituted == FunctionType(Nil, List(TInt), Nil, TInt))

    val substituted2 = subst.substitute(FunctionType(List(T1), List(T1), Nil, T1))
    println(substituted2)

    println(Unification.unify(
      FunctionType(List(T1), List(T1), Nil, T1),
      FunctionType(List(T2), List(T2), Nil, T2)
    ))
    println(Unification.unify(
      FunctionType(List(T1), List(T1), Nil, BoxedType(FunctionType(List(T2), List(T2), Nil, T2))),
      FunctionType(List(T2), List(T2), Nil, BoxedType(FunctionType(List(T1), List(T1), Nil, T1)))
    ))
    println(Unification.unify(
      FunctionType(List(T1), List(T1), Nil, BoxedType(FunctionType(List(T1), List(T1), Nil, T1))),
      FunctionType(List(T2), List(T2), Nil, BoxedType(FunctionType(List(T2), List(T2), Nil, T2)))
    ))
  }
}
