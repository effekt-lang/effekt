package effekt
package generator
package llvm

import effekt.context.Context
import effekt.lifted.LiftInference
import effekt.machine
import kiama.output.PrettyPrinterTypes.{Document, emptyLinks}
import kiama.util.Source


class LLVM extends Compiler[String] {

  // Implementation of the Compiler Interface:
  // -----------------------------------------
  def extension = ".ll"

  override def supportedFeatureFlags: List[String] = Transformer.llvmFeatureFlags

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => None
    case Stage.Machine => steps.afterMachine(source).map { res => machine.PrettyPrinter.format(res.program) }
    case Stage.Target => steps.afterLLVM(source).map { res => pretty(res) }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => res.core }
    case Stage.Lifted => None
    case Stage.Machine => steps.afterMachine(source).map { res => res.program }
    case Stage.Target => steps.afterLLVM(source)
  }

  override def compile(source: Source)(using C: Context) =
    Compile(source) map { (mod, defs) =>
      val mainFile = path(mod)
      (Map(mainFile -> pretty(defs).layout), mainFile)
    }


  // The Compilation Pipeline
  // ------------------------
  // Source => Core => Lifted => Machine => LLVM
  lazy val Compile = allToCore(Core) andThen Aggregate andThen core.PolymorphismBoxing andThen core.Optimizer andThen Machine map {
    case (mod, main, prog) => (mod, llvm.Transformer.transform(prog))
  }

  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }


  // The Compilation Pipeline for VSCode
  // -----------------------------------
  object steps {
    // intermediate steps for VSCode
    val afterCore = allToCore(Core) andThen Aggregate andThen core.PolymorphismBoxing andThen core.Optimizer
    val afterMachine = afterCore andThen Machine map { case (mod, main, prog) => prog }
    val afterLLVM = afterMachine map {
      case machine.Program(decls, prog) =>
        // we don't print declarations here.
        llvm.Transformer.transform(machine.Program(Nil, prog))
    }
  }

  // Helpers
  // -------
  private def pretty(defs: List[llvm.Definition])(using Context): Document =
    Document(llvm.PrettyPrinter.show(defs), emptyLinks)
}
