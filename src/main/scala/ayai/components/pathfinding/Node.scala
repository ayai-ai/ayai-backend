package ayai.components.pathfinding

import ayai.components.Position

// A traversable graph for AI purposes, to be constructed out of game maps

object NodeState extends Enumeration {
  type NodeState = Value
  val IMPASS, SLOW, NORMAL = Value
}

import NodeState.{NodeState, IMPASS, NORMAL, SLOW}

case class Node(val state: NodeState) {
  override def toString = s"$printableState Node"

  private def printableState = state match {
    case IMPASS => "Impassable"
    case NORMAL => "Normal"
    case SLOW => "Slow"
    case _ => "Unknown"
  }
}

case class NodeMatrix(nestedNodeArrays: Array[Array[Node]]) {
  def apply(position: NodeMatrixPosition): NodeState = {
    if (positionIsInBounds(position)) {
      nestedNodeArrays(position.y)(position.x).state
    } else {
      IMPASS
    }
  }

  private def positionIsInBounds(position: NodeMatrixPosition): Boolean = {
    val numRows = nestedNodeArrays.length
    if (numRows == 0) return false

    val firstRow = nestedNodeArrays(0)
    val numColumns = firstRow.length
    if (numColumns == 0) return false

    position.x >= 0 && position.x < numColumns && position.y >= 0 && position.y < numRows
  }

  override def equals(other: Any): Boolean = other match {
    case that: NodeMatrix => that.nestedNodeArrays.deep == this.nestedNodeArrays.deep
    case _ => false
  }

  override def toString: String = {
    val nodesRepresentation = nestedNodeArrays.map(_.map(_.toString).mkString(" ")).mkString(", ")
    s"NodeMatrix(\n$nodesRepresentation)"
  }
}

/**
 * NodeMatrixPosition is different from Position in that
 * it is adjusted for the tile map's tile size, and only applies to a NodeMatrix
 * @param x is always >= 0 and increases to the right
 * @param y is always >= 0 and increases downwards
 */
case class NodeMatrixPosition(x: Int, y: Int) {
  def toPosition(ratio: Float): Position = {
    Position(math.round(this.x * ratio), math.round(this.y * ratio))
  }

  override def toString: String = s"($x,$y)"
}
object NodeMatrixPosition {
  def fromPosition(position: Position, ratio: Float): NodeMatrixPosition = {
    NodeMatrixPosition(math.round(position.x / ratio), math.round(position.y / ratio))
  }
}

/**
 * Compared to a NodeMatrix, a NodeMatrixGraphView lets you view graph edges, not just nodes.
 */
class NodeMatrixGraphView(nodeMatrix: NodeMatrix, movementStyle: GridMovementStyle) {
  private val terrainCosts = Map(NORMAL -> 1, SLOW -> 2)
  def costOfNeighborMove(from: NodeMatrixPosition, to: NodeMatrixPosition): Double = terrainCosts(nodeMatrix(to))

  def neighbors(pos: NodeMatrixPosition): List[NodeMatrixPosition] = {
    movementStyle.neighboringPositionFinder(nodeMatrix).passableNeighboringPositions(pos)
  }
}
