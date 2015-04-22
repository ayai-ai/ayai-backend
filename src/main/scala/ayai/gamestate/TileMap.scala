package ayai.gamestate

/** Ayai Imports **/
import ayai.components._
import ayai.maps.{Tile, Layer, TransportInfo, Tileset}

/** External Imports **/
import scala.math._

//128 x 128 is only default
class TileMap(val array: Array[Array[Tile]], var transports: List[TransportInfo], var tilesets: List[Tileset]) {
  var file: String = ""
  var width: Int = 128
  var height: Int = 128
  var tileSize: Int = 32

  //getMaximumPosition - get the maximum position value for x
  def maximumWidth: Int = array.length * tileSize

  //getMaximumHeight - get the maximum position value for y
  def maximumHeight: Int = array(0).length * tileSize

  def getTileByPosition(position: Position): Tile = array(valueToTile(position.x))(valueToTile(position.y))

  // Get a tile by a x or y value from the array (example: 32 tileSize value, 65 (position) / 32) = 2 tile
  def valueToTile(value: Int): Int = value / tileSize

  def clipPositionToBounds(position: Position) = Position(
    position.x.max(0).min(maximumWidth - tileSize),
    position.y.max(0).min(maximumHeight - tileSize)
  )

  def onTileCollision(position: Position, bounds: Bounds): Boolean = {
    val newPos = Position(position.x + bounds.width, position.y + bounds.height)
    val tile = getTileByPosition(clipPositionToBounds(newPos))

    return tile.isCollidable ||
      getTileByPosition(position).isCollidable ||
      getTileByPosition(clipPositionToBounds(Position(position.x + bounds.width, position.y))).isCollidable ||
      getTileByPosition(clipPositionToBounds(Position(position.x, position.y + bounds.height))).isCollidable
  }

  /**
   * For checkIfTransport, use the characters position and see if they are in any of the transport areas
   * If inside transport area, then return the new transport
   */
  def checkIfTransport(characterPosition: Position): Option[TransportInfo] = {
    for (transport <- transports) {
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
