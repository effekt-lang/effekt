package effekt

/**
 * A Backend is composed of a compiler implementation and a runner.
 *
 * The compiler defines the respective pipeline for the backend. It inherits from [[Compiler]]
 * and can make use of common phases, such as [[Frontend]].
 *
 * The runner decides how to run an executable created by the compiler. It inherits from [[Runner]]
 * and can also configure the standard library of the particular backend.
 *
 * Compiler and Runner can communicate via a self-chosen type [[E]] -- the executable. Most
 * of the time, [[E]] will be String and correspond to the name of the generated file.
 *
 * @param name the name of the backend (should be identical to the --backend flag)
 * @param compiler the compiler for this backend
 * @param runner the runner for this backend
 * @tparam E the type of executables, mostly String
 */
case class Backend[E](name: String, compiler: Compiler[E], runner: Runner[E])

object Backend {
  def js = Backend("js", JSCompiler(), JSRunner)
  def chezMonadic = Backend("chez-monadic", ChezMonadicCompiler(), ChezMonadicRunner)
  def chezCallCC = Backend("chez-callcc", ChezCallCCCompiler(), ChezCallCCRunner)
  def chezLift = Backend("chez-lift", ChezLiftCompiler(), ChezLiftRunner)
  def llvm = Backend("llvm", LLVMCompiler(), LLVMRunner)
  def ml = Backend("ml", MLCompiler(), MLRunner)

  def backend(name: String): Backend[_] = name match {
    case "js" => Backend.js
    case "chez-monadic" => Backend.chezMonadic
    case "chez-callcc" => Backend.chezCallCC
    case "chez-lift" => Backend.chezLift
    case "llvm" => Backend.llvm
    case "ml" => Backend.ml
  }
}
