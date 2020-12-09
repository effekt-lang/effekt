package effekt
package util

import effekt.source.SourceModuleDef
import org.bitbucket.inkytonik.kiama.util.Source

/**
 * Use by the REPL with synthesized modules. "input" contains the last REPL entry,
 * not the whole source.
 */
case class VirtualSource(virtualModule: SourceModuleDef, input: Source) extends Source {
  val name = input.name
  val content = input.content

  // Are these every used?
  def reader: java.io.Reader = ???
  def useAsFile[T](fn: String => T): T = ???
}
