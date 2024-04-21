package effekt

import kiama.util.REPLConfig
import org.rogach.scallop.{ ScallopOption, fileConverter, fileListConverter, longConverter, stringConverter, stringListConverter }

import java.io.File

class EffektConfig(args: Seq[String]) extends REPLConfig(args) {

}
