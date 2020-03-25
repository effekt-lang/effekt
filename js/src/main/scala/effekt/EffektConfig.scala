package effekt

import java.io.File
import org.bitbucket.inkytonik.kiama.util.Emitter

trait EffektConfig {

  def output(): Emitter

  def includes(): List[File] = Nil
}