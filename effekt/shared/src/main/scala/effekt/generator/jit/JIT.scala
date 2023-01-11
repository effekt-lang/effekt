package effekt
package generator
package jit

import effekt.context.Context
import effekt.symbols.{Module, TermSymbol}
import effekt.util.paths.*

import scala.language.implicitConversions
import kiama.output.PrettyPrinterTypes.{ Document, emptyLinks }

import scala.sys.process.Process

object JIT extends Backend {
  def compileWhole(main: CoreTransformed, mainSymbol: TermSymbol)(implicit C: Context): Option[Compiled] = {
    val mainFile = path(main.mod);

    val machineMod = machine.Transformer.transform(main, mainSymbol)

    val jitProgram = Transformer.transform(machineMod);
    val doc = PrettyPrinter.format(jitProgram);

    Some(Compiled(main.source, mainFile, Map(mainFile -> doc)))
  }

  override def compileSeparate(main: AllTransformed)(implicit C: Context): Option[Document] = {
    val machine.Program(decls, prog) = machine.Transformer.transform(main.main, symbols.TmpValue())

    // we don't print declarations here.
    val jitDefinitions = Transformer.transform(machine.Program(Nil, prog))

    val jitString = PrettyPrinter.format(jitDefinitions)

    Some(Document(jitString.layout, emptyLinks))
  }

  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".rpyeffect"
}
