package effekt.generator

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

class LLVM { // extends Generator {
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath

  /**
   * This is only called on the main entry point, we have to manually traverse the dependencies
   * and write them.
   */
  //TODO-LLVM refactor
  def run(src: Source)(implicit C: Context): Option[Document] = {
    None
    // val modQ = C.frontend(src)
    // if (modQ.isEmpty)
    //   return None
    // val mod = modQ.get

    // val mainName = C.checkMain(mod)

    // // TODO why is backend returning Option? What if compilation fails?
    // val coreMods = (mod.dependencies :+ mod).flatMap(m => C.backend(m.source))
    // // TODO get rid of this object!
    // val machiner = new machine.Transformer
    // // TODO get rid of the module! we only need it for fresh name generation
    // val llvmDefs = C.using(module = mod) {
    //   val machineMods = coreMods.map(m => machiner.transform(m)(machiner.TransformerContext(C)))
    //   machineMods.flatMap(m => LLVMTransformer.transform(m))
    // }

    // val llvm: LLVMFragment = LLVMFragmentPrinter.wholeProgramOneshot(mainName, llvmDefs)
    // return Some(Document(llvm, emptyLinks))
  }
}

