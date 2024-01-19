package lspTest

import scala.scalajs.js
import typings.node.fsMod
import utest._

object Checker {
  // This must not necessarily be the same as Tests.scala's testDir since we might run lspTest on all examples
  def checkDir = "lspTest/tests"

  def objEqual(a: js.Any, b: js.Any) =
    js.JSON.stringify(a) == js.JSON.stringify(b)

  def compareWithFile(value: js.Object, path: String) = {
    val reference = js.JSON.parse(fsMod.readFileSync(path).toString.strip)

    // TODO: overwrite file if flag is given
    assert(objEqual(reference, value))
  }

  def getReferences(path: String) = 
    js.JSON.parse(fsMod.readFileSync(s"${path}.check.json").toString.strip) // TODO: cache per path

  def checkStats(name: String, result: js.Object) =
    compareWithFile(result, s"${checkDir}/stats/${name}.check.json")

  def checkSample(request: String, path: String, result: js.Object) = {
    val allReferences = getReferences(path)
    val reference = allReferences.selectDynamic(request)

    // TODO: overwrite file if flag is given
    assert(objEqual(reference, result))
  }

  def checkContextualSample(request: String, context: js.Object, path: String, result: js.Object) = {
    val contextualReferences = getReferences(path).selectDynamic(request).asInstanceOf[js.Array[js.Dynamic]]
    val maybeReference = contextualReferences.find { obj => objEqual(obj.selectDynamic("context"), context) }
    assert(maybeReference.isDefined)

    // TODO: overwrite file if flag is given
    maybeReference.map { reference =>
      val referenceResult = reference.selectDynamic("result")
      assert(objEqual(referenceResult, result))
    }
  }
}