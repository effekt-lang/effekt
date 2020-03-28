package effekt

import java.io.File

import org.bitbucket.inkytonik.kiama.util.REPLConfig
import org.rogach.scallop.ScallopOption

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

  lazy val includes: ScallopOption[List[File]] = opt[List[File]](
    "includes",
    descr = "Path to consider for includes (can be set multiple times)",
    default = Some(List(new File("."))),
    noshort = true
  )

  def requiresCompilation(): Boolean = {
    val compilerMode = compile()
    val interpreterMode = filenames().isEmpty && !server()
    compilerMode || interpreterMode
  }

  validateFilesIsDirectory(includes)
}
