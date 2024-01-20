package lspTest

import scala.collection.mutable.HashMap
import scala.scalajs.js
import typings.node.fsMod
import utest._

object Checker {
  // This must not necessarily be the same as Tests.scala's testDir since we might run lspTest on all examples
  val checkDir = "lspTest/tests"

  // Overwrite .check files with test results
  // TODO: add CLI flag for this
  val overwriteResults = false

  def toCheckPath(testPath: String, subDir: String) =
    val basename = testPath.split('/').last
    s"$checkDir/$subDir/$basename"

  def objEqual(a: js.Any, b: js.Any) =
    js.JSON.stringify(a) == js.JSON.stringify(b)

  def compareWithFile(value: js.Object, path: String) =
    val check = js.JSON.parse(fsMod.readFileSync(s"${path}.check.json").toString.strip)
    assert(objEqual(check, value))

  val checkCache = HashMap[String, js.Dynamic]()
  def readChecks(path: String) = 
    val file = s"${path}.check.json"
    if (!fsMod.existsSync(file)) js.Object().asInstanceOf[js.Dynamic]
    else checkCache.getOrElseUpdate(path, js.JSON.parse(fsMod.readFileSync(file).toString.strip))

  def writeChecks(path: String, checks: js.Dynamic) = 
    val pretty = js.JSON.stringify(checks, (_, b: js.Any) => b, "\t")
    fsMod.writeFileSync(s"${path}.check.json", pretty)

  def checkStats(name: String, result: js.Object) =
    val path  = toCheckPath(name, "stats")
    if (overwriteResults) writeChecks(path, result.asInstanceOf[js.Dynamic])
    compareWithFile(result, path)

  def checkSample(request: String, testPath: String, result: js.Object) =
    val path = toCheckPath(testPath, "samples")
    val allChecks = readChecks(path)
    if (overwriteResults) allChecks.updateDynamic(request)(result)
    val check = allChecks.selectDynamic(request)
    assert(objEqual(check, result))

  def checkContextualSample(request: String, context: js.Object, testPath: String, result: js.Object) = {
    val path = toCheckPath(testPath, "samples")

    var allChecks = readChecks(path)
    var contextualChecks = allChecks.selectDynamic(request).asInstanceOf[js.Array[js.Dynamic]]
    if (contextualChecks == js.undefined) contextualChecks = js.Array()

    var index = contextualChecks.indexWhere { obj => objEqual(obj.context, context) }

    if (overwriteResults) {
      if (index == -1) {
        val updated = js.Object().asInstanceOf[js.Dynamic]
        updated.updateDynamic("context")(context)
        updated.updateDynamic("result")(result)
        contextualChecks.push(updated)
        index = contextualChecks.length - 1
      } else {
        contextualChecks(index).updateDynamic("result")(result)
      }
      allChecks.updateDynamic(request)(contextualChecks)
      writeChecks(path, allChecks)
    }

    assert(index != -1)
    val checkResult = contextualChecks(index).selectDynamic("result")
    assert(objEqual(checkResult, result))
  }
}