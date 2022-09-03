package effekt
package generator
package llvm

import kiama.output.PrettyPrinterTypes.{ Document, emptyLinks }
import kiama.util.{ Source, Counter }

import effekt.context.Context
import effekt.lifted.*
import effekt.machine
import effekt.llvm.*
import effekt.symbols.{ Module, BlockSymbol, Name, Symbol, ValueSymbol }
import effekt.context.assertions.*
import effekt.util.paths.*

object LLVM extends Backend {
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(using C: Context): Option[Compiled] = {
    val mainFile = path(main.mod)
    val machineMod = machine.Transformer.transform(main, dependencies)
    val llvmDefinitions = Transformer.transform(machineMod)
    val llvmString = effekt.llvm.PrettyPrinter.show(llvmDefinitions)
    val result = Document(llvmString, emptyLinks)

    Some(Compiled(mainFile, Map(mainFile -> result)))
  }

  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ll"

  // apparently only used by the LSP; currently unsupported
  def compileSeparate(input: CoreTransformed)(implicit C: Context): Option[Document] = ???
}
