package effekt

import java.io.File

import kiama.util.{ Emitter, OutputEmitter }

trait EffektConfig {

  def output(): Emitter = new OutputEmitter

  def includes(): List[File] = Nil

  def backend(): String = "js"

  def outputPath(): String = "out"

  def requiresLift(): Boolean = false

  def prelude() = List("effekt")
}
