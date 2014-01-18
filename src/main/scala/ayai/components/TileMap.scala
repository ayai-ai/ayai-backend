package ayai.components

import com.artemis.Component
import ayai.maps.Tile
import ayai.maps.Layer

import scala.math._

import ayai.components.Position

//128 x 128 is only default
class TileMap(val array : Array[Array[Tile]], val tileSize : Int = 32) extends Component {
	var file : String = ""
	var width : Int = _
	var height : Int = _
	
	//getMaximumPosition - get the maximum position value for x
	def getMaximumWidth() : Int = {
		return array.length * tileSize
	}

	//getMaximumHeight - get the maximum position value for y
	def getMaximumHeight() : Int = {
		return (array(0).length * tileSize)
	}

	
	def getTileByPosition(position : Position) : Tile = {
		val x : Int = position.x
		val y : Int = position.y

		return array(valueToTile(x))(valueToTile(y))

	}

	def valueToTile(value : Int) : Int = {
		return (value / array.length)
	}

	def isPositionInBounds(position : Position) : Position = {

		//check if its past the x boundaries
		//println("Current X: " + position.x + " MaximumWidth: " + getMaximumWidth)
		if(max(position.x, 0) <= 0)
			position.x = 0
		else if(min(position.x,getMaximumWidth-tileSize) >= getMaximumWidth-tileSize)
			position.x = getMaximumWidth - tileSize

		if(max(position.y, 0) <= 0) 
			position.y = 0
		else if(min(position.y,getMaximumHeight) >= getMaximumHeight-tileSize)
			position.y = getMaximumHeight - tileSize
		position
	}
}