package effekt

import java.io.File

import org.bitbucket.inkytonik.kiama.util.{ Emitter, OutputEmitter }

trait EffektConfig {

  def output(): Emitter = new OutputEmitter

  def includes(): List[File] = Nil

  def generator(): String = "js"

  def outputPath(): String = "out"

  def requiresLift(): Boolean = false
}
