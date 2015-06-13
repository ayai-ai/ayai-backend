package ayai.ai

import ayai.components.pathfinding._
import org.scalatest._

class PathfinderSpec extends FlatSpec with Matchers {
  val movementStyle = new ManhattanMovementStyle
  val pathfinder = new AStarPathfinder(movementStyle)

  "A* Manhattan pathfinding" should "return None when there is no valid path" in {
    val mapWithNoPath = new NodeMatrix(Array(
      Array(Node(NodeState.NORMAL), Node(NodeState.IMPASS)),
      Array(Node(NodeState.IMPASS), Node(NodeState.NORMAL))
    ))
    pathfinder.findPath(mapWithNoPath, NodeMatrixPosition(0, 0), NodeMatrixPosition(1, 1)) should equal(None)
  }

  it should "be able to find a uniquely valid path" in {
    val map = createNodeMatrixFromString(
      """# # # .
        |. . # .
        |# . . .""".stripMargin)
    val start = NodeMatrixPosition(3, 0)
    val goal = NodeMatrixPosition(0, 1)

    val path = pathfinder.findPath(map, start, goal) match {
      case Some(path) => path
      case None => fail("path should exist")
    }
    path should equal(List(
      NodeMatrixPosition(3, 1),
      NodeMatrixPosition(3, 2),
      NodeMatrixPosition(2, 2),
      NodeMatrixPosition(1, 2),
      NodeMatrixPosition(1, 1),
      NodeMatrixPosition(0, 1)
    ))
  }

  it should "find a maximally short path across almost-empty space" in {
    val map = createNodeMatrixFromString(
      """. . . #
        |. . . .
        |. . # .
        |# . . .""".stripMargin)
    val start = NodeMatrixPosition(0, 0)
    val goal = NodeMatrixPosition(3, 3)

    val path = pathfinder.findPath(map, start, goal) match {
      case Some(path) => path
      case None => fail("path should exist")
    }
    path.length should equal(6)
    assertPathIsValid(path, movementStyle, start, goal)
  }

  it should "return a global optimum even if it is locally suboptimal" in {
    val map = createNodeMatrixFromString(
      """. . . . .
        |. # # # .
        |. . . # .
        |# # . # .
        |. . . # .
        |. # # # .
        |. . . . .""".stripMargin)
    val start = NodeMatrixPosition(0, 1)
    val goal = NodeMatrixPosition(4, 6)

    val path = pathfinder.findPath(map, start, goal) match {
      case Some(path) => path
      case None => fail("path should exist")
    }
    path(0) should equal(NodeMatrixPosition(0, 0))
    path.length should equal(11) // the slower path has length 13
    assertPathIsValid(path, movementStyle, start, goal)
  }

  it should "penalize slow terrain appropriately" in {
    // the map includes a wall of slow tiles that must be crossed, to check that crossing is possible,
    // and a wall of slow tiles with a gap, to check that the gap is found
    val map = createNodeMatrixFromString(
      """. ~ . . ~ ~
        |~ ~ . ~ ~ ~
        |. . . ~ ~ ~
        |. ~ . ~ ~ ~
        |~ ~ . . . .""".stripMargin)
    val start = NodeMatrixPosition(0, 0)
    val goal = NodeMatrixPosition(5, 4)

    val path = pathfinder.findPath(map, start, goal) match {
      case Some(path) => path
      case None => fail("path should exist")
    }

    // I don’t test exact equality because I don’t care about tiebreaker diagonal moves,
    // e.g. the first move could be (0, 1) or (1, 0)
    path.length should equal(9)
    assertPathIsValid(path, movementStyle, start, goal)
    assert(path.containsSlice(List(
      NodeMatrixPosition(2, 2),
      NodeMatrixPosition(2, 3),
      NodeMatrixPosition(2, 4),
      NodeMatrixPosition(3, 4)
    )), "path did not avoid the slow tiles")
  }

  def assertPathIsValid(path: Seq[NodeMatrixPosition], movementStyle: GridMovementStyle, start: NodeMatrixPosition, goal: NodeMatrixPosition): Unit = {
    def positionsAreAdjacent(pos1: NodeMatrixPosition, pos2: NodeMatrixPosition, movementStyle: GridMovementStyle): Boolean = {
      movementStyle.distanceHeuristic.estimateDistance(pos1, pos2) == 1
    }

    assert(positionsAreAdjacent(path.head, start, movementStyle))
    assert(path.last == goal)

    path.sliding(2).foreach({
      case Seq(pos, nextPos) =>
        assert(
          positionsAreAdjacent(pos, nextPos, movementStyle),
          s"positions $pos and $nextPos in the path are not adjacent"
        )
    })
  }

  def createNodeMatrixFromString(charGrid: String): NodeMatrix = {
    val lines = charGrid.split("\\r?\\n")
    val nodeArrays = lines.map (line =>
      line.flatMap (char => char match {
          case '.' => Some( Node(NodeState.NORMAL) )
          case '~' => Some( Node(NodeState.SLOW) )
          case '#' => Some( Node(NodeState.IMPASS) )
          case _ => None
        }
      ).toArray
    ).toArray
    new NodeMatrix(nodeArrays)
  }
  "createNodeMatrixFromString (a test helper)" should "generate maps correctly" in {
    val mapString =
      """. ~ .
        |# # .""".stripMargin
    val map = createNodeMatrixFromString(mapString)
    val desiredMap = new NodeMatrix(Array(
      Array(Node(NodeState.NORMAL), Node(NodeState.SLOW), Node(NodeState.NORMAL)),
      Array(Node(NodeState.IMPASS), Node(NodeState.IMPASS), Node(NodeState.NORMAL))
    ))
    map should equal(desiredMap)
  }
}
