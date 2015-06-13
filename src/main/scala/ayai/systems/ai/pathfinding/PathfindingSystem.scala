package ayai.systems.ai.pathfinding

import akka.actor.ActorSystem
import ayai.components.Position
import ayai.components.pathfinding._
import ayai.factories.GraphFactory
import ayai.gamestate.RoomWorld
import crane.{Entity, EntityProcessingSystem}

object PathfindingSystem {
  val COMPONENT_INCLUDES: List[AnyRef] = List(
    // TODO: currently Crane doesn't allow for specifying component types by root class (superclass)
    // It would be nice to change this so we could query for all entities which have components inheriting from "Pathfinder"
    classOf[AStarPathfinder],
    classOf[Position]
  )

  def apply(actorySystem: ActorSystem): PathfindingSystem = new PathfindingSystem(actorySystem)
}

class PathfindingSystem(actorSystem: ActorSystem) extends EntityProcessingSystem(include = PathfindingSystem.COMPONENT_INCLUDES) {

  def processEntity(e: Entity, delta: Int): Unit = {
    val roomWorld = world.asInstanceOf[RoomWorld]
    val map = GraphFactory.generateGraph(roomWorld)

    for {
      pathfinding <- e.getComponent(classOf[AStarPathfinder]).toList.map(_.asInstanceOf[AStarPathfinder])
      position <- e.getComponent(classOf[Position]).toList.map(_.asInstanceOf[Position])
    } {
      //println(s"Processing entity = ${e.uuid} for AStarPathfinder at position $position")

      // TODO: replace with real target
      val movementStyle = new ManhattanMovementStyle
      val graphView = new NodeMatrixGraphView(map, movementStyle)
      val conversionRatio: Float = roomWorld.tileMap.tileSize
      val nmPosition = NodeMatrixPosition.fromPosition(position, conversionRatio)
      pathfinding.findPath(graphView, nmPosition, nmPosition) match {
        case Some(path) => {
          if (path.isEmpty) {
            // We are at the goal
          } else {
            // Change velocity, position, whatever
            // This part will probably call .toPosition(conversionRatio) on a NodeMatrixPosition
          }
        }
        case None => {
          // There is no path from start to end
        }
      }
    }
  }
}
