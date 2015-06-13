package ayai.components.pathfinding

trait GridMovementStyle {
  def distanceHeuristic: DistanceHeuristic
  def neighboringPositionFinder(map: NodeMatrix): NeighboringPositionFinder
}

/**
 * Movement by one square up, down, right, or left.
 */
class ManhattanMovementStyle extends GridMovementStyle {
  override def distanceHeuristic: DistanceHeuristic = new ManhattanDistance
  override def neighboringPositionFinder(map: NodeMatrix): NeighboringPositionFinder = new ManhattanNeighboringPositionFinder(map)
}

/**
 * Movement along axes or diagonally, to eight possible squares, like a king on a chessboard.
 */
class ChebyshevMovementStyle extends GridMovementStyle {
  override def distanceHeuristic: DistanceHeuristic = new ChebyshevDistance
  override def neighboringPositionFinder(map: NodeMatrix): NeighboringPositionFinder = new ChebyshevNeighboringPositionFinder(map)
}

/**
 * Movement along axes or diagonally, to eight possible squares, like a king on a chessboard.
 * But disallowing diagonal movement that would clip the corner of a wall.
 */
class WallSensitiveChebyshevMovementStyle extends GridMovementStyle {
  override def distanceHeuristic: DistanceHeuristic = new ChebyshevDistance
  override def neighboringPositionFinder(map: NodeMatrix): NeighboringPositionFinder = new WallSensitiveChebyshevNeighboringPositionFinder(map)
}
