package effekt

import java.io.File

import kiama.util.{ Emitter, OutputEmitter }

trait EffektConfig {

  def output(): Emitter = new OutputEmitter

  def includes(): List[String] = List(
    "common",
    "js",
    ".")

  def backend(): Backend = Backend()

  def outputPath(): String = "out"

  def requiresLift(): Boolean = false

  def prelude(): List[String] = List(
    "effekt",
    "option",
    "list",
    "result",
    "exception",
    "array",
    "string",
    "ref"
  )

  def optimize() = true

  def maxInlineSize() = 50L

  def timed() = false

  def debug() = false
}
