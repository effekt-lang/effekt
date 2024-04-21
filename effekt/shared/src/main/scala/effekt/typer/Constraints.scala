package effekt
package typer

import effekt.symbols.*

// Auxiliary Definitions
// ---------------------

private[typer]
class Node

private[typer]
type CNode = CaptUnificationVar

// non present filter stands for "everything is passed through"
private[typer]
type Filter = Set[Capture]

private[typer]
case class CaptureNodeData(
  // non present bounds represent bottom (the empty set)
  lower: Option[Set[Capture]],
  // non present bounds represent top (the universal set)
  upper: Option[Set[Capture]],
  lowerNodes: Map[CNode, Filter],
  upperNodes: Map[CNode, Filter]
)
private val emptyData = CaptureNodeData(None, None, Map.empty, Map.empty)

private[typer]
type CaptureConstraints = Map[CNode, CaptureNodeData]

class Constraints(

) {

}
