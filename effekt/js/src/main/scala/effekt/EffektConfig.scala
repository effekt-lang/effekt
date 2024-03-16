package effekt

import java.io.File

import kiama.util.{ Emitter, OutputEmitter }

trait EffektConfig {

  def output(): Emitter = new OutputEmitter

  def includes(): List[File] = Nil

  def backend(): Backend = Backend()

  def outputPath(): String = "out"

  def requiresLift(): Boolean = false

  def prelude() = List("effekt")
  
  def timed() = false
}
