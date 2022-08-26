package effekt
package generator
package llvm

import kiama.output.PrettyPrinterTypes.{ Document, emptyLinks }
import kiama.util.{ Source, Counter }

import effekt.context.Context
import effekt.lifted.*
import effekt.machine
import effekt.llvm._
import effekt.symbols.Module
import effekt.symbols.{ BlockSymbol, Name, Symbol, ValueSymbol }
import effekt.context.assertions._

import scala.language.implicitConversions
import effekt.util.paths._

object LLVM extends Backend {
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(implicit C: Context): Option[Compiled] = {

    val mainSymbol = C.checkMain(main.mod);
    val mainFile = path(main.mod);

    val Some(liftedMod) = LiftInference(main).map(_.core);
    // TODO this flatmap is wrong, or?
    val liftedDeps = dependencies.flatMap { dep => LiftInference(dep).map(_.core) };

    val machineMod = C.using(module = main.mod) {
      machine.Transformer.transform(mainSymbol, liftedMod, liftedDeps)
    };

    val llvmDefinitions = Transformer.transform(machineMod);

    val llvmFragment = PrettyPrinter.asFragment(llvmDefinitions);

    val result = Document(llvmFragment, emptyLinks);

    Some(Compiled(mainFile, Map(mainFile -> result)))
  }

  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ll"

  // apparently only used by the LSP; currently unsupported
  def compileSeparate(input: CoreTransformed)(implicit C: Context): Option[Document] = ???
}
