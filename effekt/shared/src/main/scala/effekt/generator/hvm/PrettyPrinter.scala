package effekt
package generator
package hvm

import effekt.util.intercalate
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

object PrettyPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  val prelude = "#!/usr/local/bin/hvm --script\n\n(import (hvm))\n\n"

  //def toDoc(name: ChezName): Doc = name.name

  //def format(defs: List[Def]): Document =
    //pretty(vsep(defs map toDoc, line <> line))

  //def toDoc(binding: Binding): Doc = brackets(toDoc(binding.name) <+> toDoc(binding.expr))

  def toDoc(expr: Term): Doc = String("IT WORKS!!!!!")
  }

 

