package effekt
package util

import effekt.source.ModuleDecl
import kiama.util.Source

/**
 * Use by the REPL with synthesized modules. "input" contains the last REPL entry,
 * not the whole source.
 */
case class VirtualSource(virtualModule: ModuleDecl, input: Source) extends Source {
  val name = input.name
  val content = input.content

  // Are these ever used?
  override def reader: java.io.Reader = ???
  override def useAsFile[T](fn: String => T): T = ???
}
