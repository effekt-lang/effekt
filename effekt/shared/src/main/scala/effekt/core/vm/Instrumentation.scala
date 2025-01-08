package effekt
package core
package vm

trait Instrumentation {
  def staticDispatch(id: Id): Unit = ()
  def dynamicDispatch(id: Id): Unit = ()
  def patternMatch(comparisons: Int): Unit = ()
  def branch(): Unit = ()
  def pushFrame(): Unit = ()
  def popFrame(): Unit = ()
  def allocate(v: Value.Data): Unit = ()
  def closure(): Unit = ()
  def step(state: State): Unit = ()
  def readMutableVariable(id: Id): Unit = ()
  def writeMutableVariable(id: Id): Unit = ()
  def allocateVariable(id: Id): Unit = ()
  def allocateRegion(region: Address): Unit = ()
  def allocateVariableIntoRegion(id: Id, region: Address): Unit = ()
  def reset(): Unit = ()
  def shift(): Unit = ()
  def resume(): Unit = ()
  def builtin(name: String): Unit = ()
}
object NoInstrumentation extends Instrumentation

class Counting extends Instrumentation {
  var staticDispatches = 0
  var dynamicDispatches = 0
  var patternMatches = 0
  var branches = 0
  var pushedFrames = 0
  var poppedFrames = 0
  var allocations = 0
  var closures = 0
  var variableReads = 0
  var variableWrites = 0

  var resets = 0
  var shifts = 0
  var resumes = 0

  override def staticDispatch(id: Id): Unit = staticDispatches += 1
  override def dynamicDispatch(id: Id): Unit = dynamicDispatches += 1
  override def patternMatch(comparisons: Int): Unit = patternMatches += 1
  override def branch(): Unit = branches += 1
  override def pushFrame(): Unit = pushedFrames += 1
  override def popFrame(): Unit = poppedFrames += 1
  override def allocate(v: Value.Data): Unit = allocations += 1
  override def closure(): Unit = closures += 1
  override def readMutableVariable(id: Id): Unit = variableReads += 1
  override def writeMutableVariable(id: Id): Unit = variableWrites += 1
  override def reset(): Unit = resets += 1
  override def shift(): Unit = shifts += 1
  override def resume(): Unit = resumes += 1

  def report() =
    println(s"Static dispatches: ${staticDispatches}")
    println(s"Dynamic dispatches: ${dynamicDispatches}")
    println(s"Branches: ${branches}")
    println(s"Pattern matches: ${patternMatches}")
    println(s"Frames (pushed: ${pushedFrames}, popped: ${poppedFrames})")
    println(s"Allocations: ${allocations}")
    println(s"Closures: ${closures}")
    println(s"Variable reads: ${variableReads}")
    println(s"Variable writes: ${variableWrites}")
    println(s"Installed delimiters: ${resets}")
    println(s"Captured continuations: ${shifts}")
    println(s"Resumed continuations: ${resumes}")
}

trait BuiltinHistogram extends Instrumentation {
  var builtins: Map[String, Int] = Map.empty

  override def builtin(name: String): Unit =
    val before = builtins.getOrElse(name, 0)
    builtins = builtins.updated(name, before + 1)
}
