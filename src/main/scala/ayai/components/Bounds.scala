package ayai.components

import crane.Component

class Bounds (var width: Int, var height: Int) extends Component {

	def getWidth() : Int = {
		width
	}

	def getHeight() : Int = {
		height
	}
}
