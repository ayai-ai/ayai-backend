package ayai.components.pathfinding

import crane.Component

import scala.collection.mutable


abstract class Pathfinder(movementStyle: GridMovementStyle) extends Component {
  def findPath(map: NodeMatrix, start: NodeMatrixPosition, goal: NodeMatrixPosition): Option[Seq[NodeMatrixPosition]] = {
    findPath(new NodeMatrixGraphView(map, movementStyle), start, goal)
  }

  def findPath(graph: NodeMatrixGraphView, start: NodeMatrixPosition, goal: NodeMatrixPosition): Option[Seq[NodeMatrixPosition]]
}

/**
 * Pathfinding component which implements the A* pathfinding algorithm
 *
 * http://en.wikipedia.org/wiki/A*_search_algorithm
 */
class AStarPathfinder(movementStyle: GridMovementStyle) extends Pathfinder(movementStyle) {
  def findPath(graph: NodeMatrixGraphView, start: NodeMatrixPosition, goal: NodeMatrixPosition): Option[Seq[NodeMatrixPosition]] = {
    // this method is ported from these example implementations:
    // http://www.redblobgames.com//pathfinding/a-star/implementation.html#sec-1-4
    // http://www.redblobgames.com//pathfinding/a-star/implementation.html#sec-2-4

    val frontier = new mutable.PriorityQueue[PrioritizedValue[Double, NodeMatrixPosition]]
    val cameFrom = new mutable.HashMap[NodeMatrixPosition, NodeMatrixPosition]
    val costSoFar = new mutable.HashMap[NodeMatrixPosition, Double]

    frontier.enqueue(PrioritizedValue[Double, NodeMatrixPosition](0, start))
    cameFrom(start) = start
    costSoFar(start) = 0

    while (frontier.nonEmpty) {
      val current: NodeMatrixPosition = frontier.dequeue().value

      if (current == goal) {
        return Some( reconstructPath(start, goal, cameFrom) )
      }

      for (next <- graph.neighbors(current)) {
        val newCost = costSoFar(current) + graph.costOfNeighborMove(current, next)

        if (! costSoFar.contains(next) || newCost < costSoFar(next)) {
          costSoFar += (next -> newCost)
          val priority = -(newCost + movementStyle.distanceHeuristic.estimateDistance(next, goal))
          frontier.enqueue(PrioritizedValue(priority, next))
          cameFrom += (next -> current)
        }
      }
    }

    // the whole graph was explored, but we never reached the goal
    return None
  }

  /**
   * An generic object to put in a PriorityQueue, for when you want to
   * set explicit priorities instead of relying on the values' natural ordering.
   * @tparam P The type of the priority, e.g. Int or Double. Must be Ordered.
   * @tparam V The type of the value, which is the data you actually care about.
   */
  private case class PrioritizedValue[P <% Ordered[P], V](val priority: P, val value: V) extends Ordered[PrioritizedValue[P, V]] {
    override def compare(that: PrioritizedValue[P, V]): Int = priority.compare(that.priority)
  }

  /**
   * @return A sequence of adjacent positions leading from the start to the goal, including goal, but not including start.
   */
  private def reconstructPath(start: NodeMatrixPosition, goal: NodeMatrixPosition, cameFrom: collection.Map[NodeMatrixPosition, NodeMatrixPosition]): Seq[NodeMatrixPosition] = {
    // this method was adapted from these example implementations:
    // http://www.redblobgames.com//pathfinding/a-star/implementation.html#sec-1-3
    // http://www.redblobgames.com//pathfinding/a-star/implementation.html#sec-2-3

    val path = new mutable.ListBuffer[NodeMatrixPosition]
    path.prepend(goal)
    var current = cameFrom(goal)
    while (current != start) {
      path.prepend(current)
      current = cameFrom(current)
    }
    path
  }
}
