package ayai.factories

import ayai.gamestate.{TileMap, RoomWorld}
import ayai.components.{Position, Bounds}
import ayai.components.pathfinding.NodeState
import ayai.components.pathfinding.Node
import ayai.components.pathfinding.NodeMatrix
import ayai.components.pathfinding.NodeMatrixPosition

import NodeState.{IMPASS, NORMAL, SLOW}

// Merges tile map and world into a traversable graph for AI purposes

object GraphFactory {
  def generateGraph(world: RoomWorld): NodeMatrix = {
    val tileMap = world.tileMap
    val entities = world.getEntitiesByComponents(classOf[Position], classOf[Bounds])

    // start by assuming all nodes are passable
    val nodesAccordingToEntities: Array[Array[Node]] = Array.fill[Node](tileMap.width, tileMap.height) { Node(NORMAL) }

    // mark locations where an entity is standing as impassable
    entities.foreach { entity =>
      val position = entity.getComponent(classOf[Position])
      (position: @unchecked) match {
        case Some(p: Position) =>
          val gridPosition = NodeMatrixPosition.fromPosition(p, tileMap.tileSize.toFloat)
          if (positionIsWithinMapBounds(gridPosition, tileMap))
            nodesAccordingToEntities(gridPosition.x)(gridPosition.y) = Node(IMPASS)
      }
    }

    // create another node matrix that also accounts for whether the node’s tile is impassable
    val tilesAndNodes = zipTwoNestedArrays(tileMap.array, nodesAccordingToEntities)
    val nodesAccountingForBothEntitiesAndTiles = tilesAndNodes.map{ row => row.map{ case (tile, node) =>
      // Treat grid locations as impassable if either they were marked impassable earlier or its tile is impassable.
      // Otherwise, assume they are normal.
      // (NodeStates other than IMPASS and NORMAL are currently not looked for and created.)
      (node.state, tile.isCollidable) match {
        case (IMPASS, _) =>
          Node(IMPASS)
        case (_, true) =>
          Node(IMPASS)
        case _ =>
          Node(NORMAL)
      }
    }}

    new NodeMatrix(nodesAccountingForBothEntitiesAndTiles)
  }

  private def positionIsWithinMapBounds(gridPosition: NodeMatrixPosition, tileMap: TileMap): Boolean = {
    Range(0, tileMap.width).contains(gridPosition.x) && Range(0, tileMap.height).contains(gridPosition.y)
  }

  private def zipTwoNestedArrays[A, B](matrixA: Array[Array[A]], matrixB: Array[Array[B]]): Array[Array[(A, B)]] = {
    (matrixA zip matrixB) map { case (rowOfA, rowOfB) => rowOfA zip rowOfB }
  }
}
