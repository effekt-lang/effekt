package effekt

import effekt.generator.*

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
 * The lifetime of a backend is tightly coupled to a corresponding instance of [[EffektConfig]].
 * This is important for the right caching / recomputation behavior.
 *
 * @param name the name of the backend (should be identical to the --backend flag)
 * @param compiler the compiler for this backend
 * @param runner the runner for this backend
 * @tparam E the type of executables, mostly String
 */
case class Backend[E](name: String, compiler: Compiler[E], runner: Runner[E])

object Backend {

  def backend(name: String): Backend[_] = name match {
    case "js"           => Backend("js", js.JavaScript(), JSRunner)
    case "chez-monadic" => Backend("chez-monadic", chez.ChezSchemeMonadic(), ChezMonadicRunner)
    case "chez-callcc"  => Backend("chez-callcc", chez.ChezSchemeCallCC(), ChezCallCCRunner)
    case "chez-lift"    => Backend("chez-lift", chez.ChezSchemeLift(), ChezLiftRunner)
    case "llvm"         => Backend("llvm", llvm.LLVM(), LLVMRunner)
    case "ml"           => Backend("ml", ml.ML(), MLRunner)
    case "jit"          => Backend("jit", jit.JIT(), JITRunner)
  }
}
