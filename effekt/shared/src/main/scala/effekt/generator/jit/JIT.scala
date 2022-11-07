package effekt
package generator
package jit

import effekt.context.Context
import effekt.lifted.*
import effekt.jit.*
import effekt.symbols.Module

import scala.language.implicitConversions
import effekt.util.paths.*
import kiama.output.PrettyPrinterTypes

import scala.sys.process.Process

object JIT extends Backend {
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(implicit C: Context): Option[Compiled] = {

    val mainSymbol = C.checkMain(main.mod);
    val mainFile = path(main.mod);

    val Some(liftedMod) = LiftInference(main).map(_.core) : @unchecked
    // TODO this flatmap is wrong, or?
    val liftedDeps = dependencies.flatMap { dep => LiftInference(dep).map(_.core) };

    val machineMod = C.using(module = main.mod) {
      machine.Transformer.transform(mainSymbol, liftedMod, liftedDeps)
    };

    val jitProgram = effekt.jit.Transformer.transform(machineMod);
    val doc = effekt.jit.PrettyPrinter.toDocument(jitProgram);

    Some(Compiled(mainFile, Map(mainFile -> doc)))
  }

  override def compileSeparate(input: CoreTransformed)(implicit C: Context): Option[PrettyPrinterTypes.Document] = ???

  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".rpyeffect"
}
