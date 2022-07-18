package effekt
package generator
package llvm

import effekt.llvm.{ LLVMFragmentPrinter }

import kiama.output.PrettyPrinterTypes.{ Document, emptyLinks }
import kiama.util.{ Source, Counter }

import effekt.context.Context
import effekt.machine
import effekt.llvm._
import effekt.symbols.Module
import effekt.symbols.{ BlockSymbol, Name, Symbol, ValueSymbol }
import effekt.context.assertions._

import scala.language.implicitConversions
import effekt.util.paths._

object LLVM extends Backend {
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(implicit C: Context): Option[Compiled] = {
    if (dependencies.length > 0)
      C.abort("llvm does not support dependencies (yet?)")

    val doc = run(C.module.source).get

    val p = path(main.mod)
    Some(Compiled(p, Map(p -> doc)))
  }

  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath

  private def run(src: Source)(implicit C: Context): Option[Document] = {
    None
    val mod = C.frontend(src).get

    val mainName = C.checkMain(mod)

    // TODO why is backend returning Option? What if compilation fails?
    val coreMods = (mod.dependencies :+ mod).flatMap(m => C.backend(m.source))
    // TODO get rid of this object!
    val machiner = new machine.Transformer
    // TODO get rid of the module! we only need it for fresh name generation
    val llvmDefs = C.using(module = mod) {
      val machineMods = coreMods.map(m => machiner.transform(m)(machiner.TransformerContext(C)))
      machineMods.flatMap(m => LLVMTransformer.transform(m))
    }

    val llvm: LLVMFragment = LLVMFragmentPrinter.wholeProgramOneshot(mainName, llvmDefs)
    return Some(Document(llvm, emptyLinks))
  }

  // apparently only used by the LSP; currently unsupported
  def compileSeparate(input: CoreTransformed)(implicit C: Context): Option[Document] = ???
}
