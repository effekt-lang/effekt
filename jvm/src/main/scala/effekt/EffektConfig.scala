package effekt

import java.io.File

import effekt.util.AmmoniteConsole

import org.bitbucket.inkytonik.kiama.util.{ Console, FileConsole, REPLConfig, StringConsole }
import org.rogach.scallop.{ ArgType, ScallopOption, ValueConverter }
import scala.reflect.runtime.universe.TypeTag

class EffektConfig(args: Seq[String]) extends REPLConfig(args) {
  lazy val compile: ScallopOption[Boolean] = toggle(
    "compile",
    descrYes = "Compile the Effekt program to JavaScript",
    descrNo = "Run the effekt program in the interpreter",
    default = Some(false)
  )

  lazy val outputPath: ScallopOption[File] = opt[File](
    "out",
    descr = "Path to write generated JavaScript files to (defaults to ./out)",
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

  var _libPath: Option[File] = None

  def libPath: File = stdlibPath orElse _libPath getOrElse {
    sys error "Path to standard library not properly set"
  }

  //  override object consoleConverter extends ValueConverter[Console] {
  //
  //    val argType = ArgType.LIST
  //
  //    def parse(s: List[(String, List[String])]): Either[String, Option[Console]] =
  //      s match {
  //        case List((_, List("ammonite"))) =>
  //          Right(Some(AmmoniteConsole))
  //        case List((_, List("file", filename))) =>
  //          Right(Some(new FileConsole(filename)))
  //        case List((_, List("string", contents))) =>
  //          Right(Some(new StringConsole(contents)))
  //        case List((_, _)) =>
  //          Left("expected ammonite, 'file name' or 'string value'")
  //        case _ =>
  //          Right(None)
  //      }
  //
  //    val tag = implicitly[TypeTag[Console]]
  //  }

  def includes() = libPath :: includePath()

  def requiresCompilation(): Boolean = !server()

  def interpret(): Boolean = !server() && !compile()

  validateFilesIsDirectory(includePath)

}
