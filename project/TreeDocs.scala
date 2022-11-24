import sbt._
import Keys._

object TreeDocs {

  import scala.util.matching.Regex

  // 2) next up, search for a magic string like
  val path = "[a-zA-Z.$_]+".r
  val startPattern = new Regex(s"----------\\[\\[\\s*(${ path })\\s*\\]\\]----------", "path")
  val endPattern = "------------------------------".r


  case class Marker(prefix: String, className: String)
  def findStartMarker(line: String): Option[Marker] = startPattern.findFirstMatchIn(line) map { m =>
    val path = m.group("path")
    val prefix = line.substring(0, m.start) // everything on this line before the marker
    Marker(prefix, path)
  }

  def containsEndMarker(line: String): Boolean = endPattern.findFirstIn(line).isDefined

  lazy val generator = Def.task {

    val srcs = (Compile / unmanagedSources).value

    def timed[T](desc: String)(block: => T): T = {

      val before = System.currentTimeMillis()
      val res = block
      val after = System.currentTimeMillis()

      // println(s"$desc took ${ after - before }ms")
      res
    }

    var classes: Seq[String] = Seq.empty

    timed("Looking for markers") {
      srcs.foreach { src =>
        val (base, ext) = src.baseAndExt
        if (ext == "scala") {
          IO.readLines(src).foreach { line =>
            findStartMarker(line).foreach {
              case Marker(prefix, path) =>
                // println(s"File: ${ src }, Class: ${ path }")
                classes = classes :+ path
            }
          }
        }
      }
    }

    def methodNameFor(className: String): String =
      className.replace('.', '_')

    def methodFor(className: String): String =
      s"  def ${methodNameFor(className)}(indentation: String, level: Int) = summon[effekt.util.Docs[$className]].show(indentation, level)"

    // now that we collected the classes we want to render the trees for,
    // we generate a scala file:
    val contents =
    s"""package effekt.util
       |
       |class TreeDocs {
       |${classes.distinct.map(methodFor).mkString("\n")}
       |}
       |""".stripMargin

    val sourceDir = (Compile / sourceManaged).value
    val sourceFile = sourceDir / "TreeDocs.scala"
    IO.write(sourceFile, contents)
    Seq(sourceFile)
  }

  sealed trait Processing
  case object SearchingStart extends Processing
  case class SearchingEnd(docs: String, prefix: String) extends Processing

  /**
   * All of this is pieced together from:
   * - [[sbt.Defaults.runnerInit]]
   * - [[sbt.Run]]
   */
  def replacer: Def.Initialize[Task[Unit]] = Def.task {

    import sbt.internal.inc.classpath.ClasspathUtil

    // this is the magic needed to get the appropriate class loader:
    val si = (Compile / scalaInstance).value
    val cp: Classpath = (Compile / fullClasspath).value
    val cl = ClasspathUtil.makeLoader(cp.map(x => x.data.toPath), si)

    // the remainder is standard Java reflection
    val cls = cl.loadClass("effekt.util.TreeDocs")
    val instance = cls.getDeclaredConstructor().newInstance()

    def showTreeDocs(className: String, prefix: String, level: Int): String = {
      val method = cls.getMethod(className.replace('.', '_'), classOf[String], classOf[Int])
      method.invoke(instance, prefix: java.lang.String, level: java.lang.Integer).asInstanceOf[String]
    }

    val srcs = (Compile / unmanagedSources).value

    def processFile(src: File): Unit = {
      var state: Processing = SearchingStart

      val (base, ext) = src.baseAndExt
      if (ext != "scala") return;

      val lines = IO.readLines(src)

      val stringWriter = new java.io.StringWriter()
      val output = new java.io.PrintWriter(stringWriter)

      // only write to files that contain documentation
      var changed = false

      lines.foreach { line =>
        def copyLine() = output.println(line)

        state match {
          case SearchingStart =>
            copyLine()
            findStartMarker(line).foreach {
              case Marker(prefix, path) =>
                changed = true
                val prefixIndented = prefix + "  "
                val docs = showTreeDocs(path, prefixIndented, 2)
                state = SearchingEnd(docs, prefixIndented)
            }
          case SearchingEnd(docs, prefix) if containsEndMarker(line) =>
            output.println(prefix.stripTrailing)
            output.println(docs.stripTrailing)
            copyLine() // copy end marker
            state = SearchingStart
          case SearchingEnd(_, _) =>
            // skip line
        }
      }
      if (state != SearchingStart) {
        println(s"Cannot find end marker for generating documentation in: ${src}")
        return;
      }

      if (changed) {
        // println(s"Generated TreeDoc in ${src}")
        IO.write(src, stringWriter.toString)
      }
    }

    srcs.foreach { processFile }
  }
}
