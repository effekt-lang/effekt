package effekt.core

import effekt.context.{Context, ModuleDB}
import kiama.util.Positions
import kiama.util.Source
import effekt.util.messages.DebugMessaging
import scala.collection.immutable.Map

trait TestModuleDB extends ModuleDB { self: Context =>

  private var fileContents: Map[String, String] = Map.empty
  private var sources: Map[String, Source] = Map.empty

  /** Ensures that [[contentsOf()]] returns [[contents]] when called with [[path]] in the future. */
  def provideFileContents(path: String, contents: String): Unit = {
    fileContents = fileContents.updated(path, contents)
  }
  /** Ensures that [[findSource()]] returns [[source]] when called with [[path]] in the future. */
  def provideSource(path: String, source: Source): Unit = {
    sources = sources.updated(path, source)
  }

  /** Ensures that [[contentsOf()]] returns [[contents]] when called with [[path]] within [[body]]. */
  def withFileContents[R](path: String, contents: String)(body: => R): R = {
    val oldFileContents = fileContents
    fileContents = fileContents.updated(path, contents)
    try {
      body
    } finally {
      fileContents = oldFileContents
    }
  }
  /** Ensures that [[findSource()]] returns [[source]] when called with [[path]] within [[body]]. */
  def withSource[R](path: String, source: Source)(body: => R): R = {
    val oldSources = sources
    sources = sources.updated(path, source)
    try {
      body
    } finally {
      sources = oldSources
    }
  }

  override def contentsOf(path: String): Option[String] = fileContents.get(path)
  override def findSource(modulePath: String): Option[Source] = sources.get(modulePath)
}

class TestContext extends Context(new Positions) with TestModuleDB {

  object messaging extends DebugMessaging
}