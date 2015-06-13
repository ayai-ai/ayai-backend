package ayai.components.pathfinding

abstract class NeighboringPositionFinder(nodeMatrix: NodeMatrix) {
  def passableNeighboringPositions(pos: NodeMatrixPosition): List[NodeMatrixPosition] = {
    neighboringPositionsIncludingImpassable(pos).filter(position => nodeMatrix(position) != NodeState.IMPASS)
  }

  protected def neighboringPositionsIncludingImpassable(pos: NodeMatrixPosition): List[NodeMatrixPosition]

  protected def relativePositionsFromCoordinateDifferences(position: NodeMatrixPosition, differences: List[(Int, Int)]): List[NodeMatrixPosition] = {
    differences.map(_ match {
      case (xDiff, yDiff) => NodeMatrixPosition(position.x + xDiff, position.y + yDiff)
    })
  }
}

/**
 * Returns the positions marked 'x' on this diagram, where 'o' is the given position:
 *
 * .x.
 * xox
 * .x.
 */
class ManhattanNeighboringPositionFinder(nodeMatrix: NodeMatrix) extends NeighboringPositionFinder(nodeMatrix) {
  private val coordinateDifferences = List(
    (1, 0),
    (-1, 0),
    (0, 1),
    (0, -1)
  )
  protected override def neighboringPositionsIncludingImpassable(pos: NodeMatrixPosition): List[NodeMatrixPosition] = {
    relativePositionsFromCoordinateDifferences(pos, coordinateDifferences)
  }
}

/**
 * Returns the positions marked 'x' on this diagram, where 'o' is the given position:
 *
 * xxx
 * xox
 * xxx
 */
class ChebyshevNeighboringPositionFinder(nodeMatrix: NodeMatrix) extends NeighboringPositionFinder(nodeMatrix) {
  private val coordinateDifferences = List(
    (1, 0),
    (1, 1),
    (0, 1),
    (-1, 1),
    (-1, 0),
    (-1, -1),
    (0, -1),
    (1, -1)
  )
  override protected def neighboringPositionsIncludingImpassable(pos: NodeMatrixPosition): List[NodeMatrixPosition] = {
    relativePositionsFromCoordinateDifferences(pos, coordinateDifferences)
  }
}

/**
 * Returns the positions marked 'x' on this diagram, where 'o' is the given position:
 *
 * xxx
 * xox
 * xxx
 *
 * But also avoids crossing diagonals when that would mean passing through the corner of a wall '#':
 *
 * xx#   xx.   .#.
 * xox   xo#   xo#
 * xxx   xx.   xx.
 */
class WallSensitiveChebyshevNeighboringPositionFinder(nodeMatrix: NodeMatrix) extends NeighboringPositionFinder(nodeMatrix) {
  private val orthogonalCoordinateDifferences = List(
    (1, 0),
    (-1, 0),
    (0, 1),
    (0, -1)
  )
  private val diagonalCoordinateDifferences = List(
    (1, 1),
    (1, -1),
    (-1, 1),
    (-1, -1)
  )

  override protected def neighboringPositionsIncludingImpassable(pos: NodeMatrixPosition): List[NodeMatrixPosition] = {
    val orthogonalNeighborPositions = relativePositionsFromCoordinateDifferences(pos, orthogonalCoordinateDifferences)
    val diagonalNeighborPositions = relativePositionsFromCoordinateDifferences(pos, diagonalCoordinateDifferences)
    orthogonalNeighborPositions ++ diagonalNeighborPositions.filter(adjacentNonDiagonalNeighborsArePassable)
  }

  private def adjacentNonDiagonalNeighborsArePassable(position: NodeMatrixPosition): Boolean = {
    val adjacentNeighbors = List(
      NodeMatrixPosition(0, position.y),
      NodeMatrixPosition(position.x, 0)
    )
    adjacentNeighbors.forall(nodeMatrix(_) != NodeState.IMPASS)
  }
}
