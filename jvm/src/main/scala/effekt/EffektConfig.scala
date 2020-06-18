package effekt

import java.io.File

import org.bitbucket.inkytonik.kiama.util.REPLConfig
import org.rogach.scallop.{ ScallopOption, ValueConverter }

class EffektConfig(args: Seq[String]) extends REPLConfig(args) {

  lazy val compile: ScallopOption[Boolean] = toggle(
    "compile",
    descrYes = "Compile the Effekt program",
    descrNo = "Run the effekt program in the interpreter",
    default = Some(false)
  )

  lazy val outputPath: ScallopOption[File] = opt[File](
    "out",
    descr = "Path to write generated files to (defaults to ./out)",
    default = Some(new File("./out")),
    required = false
  )

  lazy val includePath: ScallopOption[List[File]] = opt[List[File]](
    "includes",
    descr = "Path to consider for includes (can be set multiple times)",
    default = Some(List(new File("."))),
    noshort = true
  )

  lazy val stdlibPath: ScallopOption[File] = opt[File](
    "lib",
    descr = "Path to the standard library to be used",
    required = false
  )

  lazy val generator: ScallopOption[String] = choice(
    choices = List("js", "cs", "cslift", "jslift"),
    name = "generator",
    descr = "The code generator that should be used",
    default = Some("js"),
    noshort = true
  )

  dependsOnAll(generator, List(compile))

  var _libPath: Option[File] = None

  def libPath: File = stdlibPath orElse _libPath getOrElse {
    sys error "Path to standard library not properly set"
  }

  def includes() = libPath :: includePath()

  def requiresCompilation(): Boolean = !server()

  def interpret(): Boolean = !server() && !compile()

  validateFilesIsDirectory(includePath)
}
