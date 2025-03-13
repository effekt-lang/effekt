package effekt

import org.rogach.scallop.exceptions.ScallopException

object Main {
  /**
   * Main entry point for the Effekt compiler.
   *
   * Depending on the command line arguments, we run in one of the following modes:
   *
   * - Launch the Effekt language server (e.g. `effekt --server`)
   * - Launch the REPL (e.g. `effekt`)
   * - Build the provided files (e.g. `effekt --build hello.effekt`)
   */
  def main(args: Array[String]): Unit = {
    val config = try {
      parseArgs(args)
    } catch {
      case e: ScallopException =>
        System.err.println(e.getMessage())
        return
    }

    if (config.server()) {
      Server.launch(config)
    } else if (config.repl()) {
      new Repl(Server).run(config)
    } else {
      compileFiles(config)
    }
  }

  /**
   * Parse command line arguments into an EffektConfig.
   */
  private def parseArgs(args: Array[String]): EffektConfig = {
    val config = new EffektConfig(args.toIndexedSeq)
    config.verify()
    config
  }

  /**
   * Compile files specified in the configuration.
   */
  private def compileFiles(config: EffektConfig): Unit = {
    for (filename <- config.filenames()) {
      Server.compileFile(filename, config)
    }
  }
}
