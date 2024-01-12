package lspTest

import scala.scalajs.js
import typings.node.fsMod
import utest._

object Checker {
  // This must not necessarily be the same as Tests.scala's testDir since we might run lspTest on all examples
  def checkDir = "lspTest/tests"

  def compareWithFile(value: String, path: String) = {
    val reference = fsMod.readFileSync(path).toString.strip

    // TODO: overwrite file if flag is given
    assert(reference == value)
  }

  def checkStats(name: String, result: js.Object) =
    compareWithFile(js.JSON.stringify(result), s"${checkDir}/stats/${name}.check.json")

  def checkSample(request: String, path: String, result: js.Object) = {
    val allReferences = fsMod.readFileSync(s"${path}.check.json").toString.strip // TODO: cache per path
    val obj = js.JSON.parse(allReferences)
    val reference = js.JSON.stringify(obj.selectDynamic(request)) // TODO: this is a hack and should probably be typed

    // TODO: overwrite file if flag is given
    assert(reference == js.JSON.stringify(result))
  }
}