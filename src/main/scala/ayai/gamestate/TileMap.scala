package ayai.gamestate

import ayai.components._
import ayai.maps.{Tile, Layer, TransportInfo, Tileset}

// 128 x 128 is only a default
class TileMap(val array: Array[Array[Tile]], var transports: List[TransportInfo], var tilesets: List[Tileset]) {
  var file: String = ""
  var width: Int = 128
  var height: Int = 128
  var tileSize: Int = 32

  //get the maximum position value for x
  def maximumWidth: Int = array.length * tileSize

  //get the maximum position value for y
  def maximumHeight: Int = array(0).length * tileSize

  def getTileByPosition(position: Position): Tile = array(valueToTile(position.x))(valueToTile(position.y))

  // Get a tile by a x or y value from the array (example: 32 tileSize value, 65 (position) / 32) = 2 tile
  def valueToTile(value: Int): Int = value / tileSize

  def clipPositionToBounds(position: Position) {
    val maxX = maximumWidth - tileSize
    val maxY = maximumHeight - tileSize
    if (position.x < 0) position.x = 0 else if (position.x > maxX) position.x = maxX
    if (position.y < 0) position.y = 0 else if (position.y > maxY) position.y = maxY
  }

  def regionCollidesWithATile(position: Position, bounds: Bounds): Boolean = {
    val newPos = new Position(position.x + bounds.width, position.y + bounds.height)
    clipPositionToBounds(newPos)
    val tile = getTileByPosition(newPos)

    if (tile.isCollidable) {
      return true
    }

    if (getTileByPosition(position).isCollidable) {
      return true
    }

    val bottomLeftPosition = new Position(position.x, position.y + bounds.height)
    clipPositionToBounds(bottomLeftPosition)
    if (getTileByPosition(bottomLeftPosition).isCollidable){
      return true
    }

    val topRightPosition = new Position(position.x + bounds.width, position.y)
    clipPositionToBounds(topRightPosition)
    if (getTileByPosition(topRightPosition).isCollidable) {
      return true
    }

    false
  }

  /**
   * For checkIfTransport, use the character’s position and see if they are in any of the transport areas
   * If inside transport area, then return the new transport
   */
  def checkIfTransport(characterPosition: Position): Option[TransportInfo] = {
    for(transport <- transports) {
      val startPosition = transport.startingPosition
      val endPosition = transport.endingPosition

      if ((characterPosition.x >= (startPosition.x * tileSize) && characterPosition.y >= (startPosition.y * tileSize)) &&
         (characterPosition.x < (endPosition.x * tileSize) && characterPosition.y < (endPosition.y * tileSize))) {
        // TODO CHANGE STARTING POSITION
        // return transport.toRoomId
        return Some(transport)
      }
    }

    None
  }
}
