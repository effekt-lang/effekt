package effekt
package generator
package llvm

import kiama.output.PrettyPrinterTypes.{ Document, emptyLinks }
import kiama.util.{ Source, Counter }

import effekt.context.Context
import effekt.lifted.*
import effekt.machine
import effekt.llvm.*
import effekt.symbols.{ Module, BlockSymbol, Name, Symbol, ValueSymbol, TermSymbol }
import effekt.context.assertions.*
import effekt.util.paths.*

object LLVM extends Backend {
  def compileWhole(main: CoreTransformed, mainSymbol: TermSymbol)(using Context): Option[Compiled] = {
    val mainFile = path(main.mod)
    val machineMod = machine.Transformer.transform(main, mainSymbol)
    val llvmDefinitions = Transformer.transform(machineMod)

    val llvmString = effekt.llvm.PrettyPrinter.show(llvmDefinitions)

    val result = Document(llvmString, emptyLinks)

    Some(Compiled(mainFile, Map(mainFile -> result)))
  }

  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ll"

  /**
   * Only used by LSP to show the generated LLVM code in VSCode.
   *
   * Right now, we use the same mechanism as for [[compileWhole]].
   * This could be optimized in the future to (for instance) not show the standard library
   * and the prelude.
   */
  def compileSeparate(main: CoreTransformed)(using Context): Option[Document] = {
    val machine.Program(decls, prog) = machine.Transformer.transform(main, symbols.TmpValue())

    // we don't print declarations here.
    val llvmDefinitions = Transformer.transform(machine.Program(Nil, prog))

    val llvmString = effekt.llvm.PrettyPrinter.show(llvmDefinitions)

    Some(Document(llvmString, emptyLinks))
  }
}
