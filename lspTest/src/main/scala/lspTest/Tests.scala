package lspTest

import scala.scalajs.js
import scala.util.{Success, Failure}
import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext
import typings.node.fsMod

class Tests(val client: Client) {
  def testsDir = "examples2"

  def testHover(file: String) = {
    implicit val ec: ExecutionContext = JSExecutionContext.queue

    client.sendHover(file, 0, 8).onComplete {
      case Success(v) => println(js.JSON.stringify(v.asInstanceOf[js.Object]))
      case Failure(_) => println("error during hoverRequest")
    }
  }

  def runTests(file: String) = {
    implicit val ec: ExecutionContext = JSExecutionContext.queue

    client.openDocument(file, fsMod.readFileSync(file).toString).onComplete {
      case Success(_) => {
        testHover(file)
      }
      case Failure(_) => println("error during didOpenTextDocumentNotification")
    }
  }

  def runTestsInDirectory(dir: String): Unit =
    fsMod.readdirSync(dir).foreach(sub => {
      val path = s"$dir/$sub"
      if (fsMod.statSync(path).get.isDirectory)
        runTestsInDirectory(path) // iterate in sub directory
      else if (path.endsWith(".effekt"))
        runTests(path)
    })

  def runAll() = runTestsInDirectory(testsDir)
}