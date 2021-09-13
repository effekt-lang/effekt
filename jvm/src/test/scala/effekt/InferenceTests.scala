package effekt

import java.io.File

import effekt.source.Tree
import effekt.util.messages
import effekt.util.messages.{ ErrorReporter, MessageBuffer }

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

import effekt.symbols.builtins.{ TInt, TBoolean, TString, TUnit }
import symbols._

import effekt.symbols.{ Name, LocalName, QualifiedName }
import effekt.substitutions._

class InferenceTests extends AnyFunSpec {

  def reporter: ErrorReporter = new ErrorReporter {
    override var focus: Tree = _
    override def buffer = new MessageBuffer
  }

  describe("Foo") {
    implicit val C = reporter
    println(Unification.unify(TInt, TInt))

    val T1 = RigidVar(TypeVar(Name.local("T1")))
    println(Unification.unify(TInt, T1))
    val Unifier(subst, constraints) = Unification.unify(T1, TInt)

    // TODO test capture avoiding substitution
    val substituted = subst.substitute(FunctionType(Nil, List(List(T1)), T1))
    assert(substituted == FunctionType(Nil, List(List(TInt)), TInt))

    val substituted2 = subst.substitute(FunctionType(List(T1), List(List(T1)), T1))
    println(substituted2)
  }
}
