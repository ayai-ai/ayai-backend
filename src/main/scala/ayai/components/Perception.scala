package ayai.components

/** Crane Imports **/

import ayai.gamestate.TileMap
import ayai.systems.PerceptionEvent
import crane.{Component, Entity}

import scala.collection.mutable.ArrayBuffer
import math.floor

/** External Imports **/
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

class SenseComponent extends Component {
  def notifySystem() = {
    //...
  }
}

class Hearing( var hearingAbility: Double) extends SenseComponent {
}

class SoundProducing( var intensity: Int) extends Component {
}

class SoundEntity( var intensity: Int, var origin: Position ) extends Entity {
}

class Vision( var los: LOS, var visionRange: Int ) extends SenseComponent {
  def drawLine(start: Position, end: Position, bounds: Bounds, tileMap: TileMap): Boolean = {
    return los.drawLine(start, end, bounds, tileMap)
  }

}

class LOS {
  def drawLine(start: Position, end: Position, bounds: Bounds, tileMap: TileMap): Boolean = {
    false
  }

  def isCollision(position: Position, bounds: Bounds, tileMap: TileMap): Boolean = {
    tileMap.clipPositionToBounds(position)
    tileMap.regionCollidesWithATile(position, bounds)
  }
}

class BresenhamLOS extends LOS {
  override def drawLine(start: Position, end: Position, bounds: Bounds, tileMap: TileMap): Boolean = {
    val signX = if (start.x < end.x) 1 else -1
    val signY = if (start.y < end.y) 1 else -1
    var deltaX = math.abs(end.x - start.x)
    val deltaY = math.abs(end.y - start.y)
    if (deltaX == 0) deltaX = 1 // avoid divide by zero errors
    var error: Double = 0.0
    val deltaErr: Double = math.abs(deltaY / deltaX)

    var yIter: Int = start.y
    var xIter: Int = start.x
    var result: Boolean = true
    while (xIter != end.x) {
      if (isCollision(new Position(xIter, yIter), bounds, tileMap)) {
        result = false
      }
      error = error + deltaErr
      while (error >= 0.5) {
        if (isCollision(new Position(xIter, yIter), bounds, tileMap)) {
          result = false
        }
        yIter = yIter + signY
        error = error - 1.0
      }
      xIter = xIter + signX
    }
    result
  }
}

class WuLOS extends LOS {
  override def drawLine(start: Position, end: Position, bounds: Bounds, tileMap: TileMap): Boolean = {
    val steep = math.abs(end.y - start.y) > math.abs(end.x - start.x)
    val (p3, p4) = if (steep) (end, start) else (start, end)
    val (a, b) = if (p3.x > p4.x) (p4, p3) else (p3, p4)
    var deltaX = b.x - a.x
    val deltaY = b.y - a.y
    if (deltaX == 0) deltaX = 1 // avoid divide by zero errors
    val gradient = deltaY / deltaX
    var intersection = 0.0
    var result = true
    var endpointResult = true

    var xPixel1 = math.round(a.x);
    {
      val yEnd = a.y + gradient * (xPixel1 - a.x)
      val xGap = rearFractional(a.x + 0.5)
      endpointResult = endpoint(xPixel1, yEnd, steep, bounds, tileMap)
      if (!endpointResult) {result = false}
      intersection = yEnd + gradient
    }

    val xPixel2 = math.round(b.x);
    {
      val yEnd = b.y + gradient * (xPixel2 - b.x)
      val xGap = fractional(b.x + 0.5)
      endpointResult = endpoint(xPixel2, yEnd, steep, bounds, tileMap)
      if (!endpointResult) {result = false}
    }

    for (x <- (xPixel1 + 1) to (xPixel2 - 1)) {
      if (steep) {
        if (isCollision(new Position(intersection.toInt, x), bounds, tileMap)) { result = false } // rearFractional(intersection))
        if (isCollision(new Position(intersection.toInt+1, x),  bounds, tileMap)) { result = false } // fractional(intersection))
      } else {
        if (isCollision(new Position(x, intersection.toInt), bounds, tileMap)) { result = false } // rearFractional(intersection))
        if (isCollision(new Position(x, intersection.toInt+1), bounds, tileMap)) { result = false } // fractional(intersection))
      }
      intersection = intersection + gradient
    }

    result
  }

  def endpoint(xPixel: Double, yEnd: Double, steep: Boolean, bounds: Bounds, tileMap: TileMap): Boolean = {
    val yPixel = floor(yEnd)
    var result = true
    if (steep) {
      if (isCollision(new Position(yPixel.toInt, xPixel.toInt), bounds, tileMap)) { result = false } // rearFractional(yEnd) * xGap)
      if (isCollision(new Position(yPixel.toInt+1, xPixel.toInt), bounds, tileMap)) { result = false } // fractional(yEnd) * xGap)
    } else {
      if (isCollision(new Position(xPixel.toInt, yPixel.toInt), bounds, tileMap)) { result = false } // rearFractional(yEnd) * xGap)
      if (isCollision(new Position(xPixel.toInt, yPixel.toInt+1), bounds, tileMap)) { result = false } // fractional(yEnd) * xGap)
    }
    result
  }

  def fractional(x: Double) = x - floor(x)
  def rearFractional(x: Double) = 1 - fractional(x)
  def average(a: Float, b: Float) = (a + b) / 2
}

class MemoryItem(event : PerceptionEvent ) {
  var memoryEvent = event
  var entityID: Int = -1
  var entityPosition: Option[Position] = None
  var relationship: Int = -1
  var counter: Int = 0

  def equals(o: MemoryItem) : Boolean = {
    val source : Boolean = o.memoryEvent.evtSource == this.memoryEvent.evtSource
    val target : Boolean = o.memoryEvent.evtTarget == this.memoryEvent.evtTarget
    val system : Boolean = o.memoryEvent.evtSystem == this.memoryEvent.evtSystem
    if (source && target && system) {
      true
    }
    else {
      false
    }
  }
}

class MemoryContents() {
  private val contents : ArrayBuffer[MemoryItem] = new ArrayBuffer[MemoryItem]()
  def add(a : MemoryItem, allowDuplicates: Boolean = false) = {
    if(allowDuplicates) {
      contents += a
    } else {
      for (item: MemoryItem <- contents) {
        if (!item.equals(a)) {
          contents += a
        }
      }
    }
  }

  def += (a : MemoryItem) = {
    this.add(a)
  }

  def -= (a : MemoryItem) = {
    contents -= a
  }
}

class Memory extends SenseComponent {
  var memoryAbility: Int = 2000 // ticks until particular memory is forgotten
  var memoryContents:  ArrayBuffer[MemoryItem] = new ArrayBuffer[MemoryItem]()

  def addMemory(a : MemoryItem, allowDuplicates: Boolean = false) = {
    if(allowDuplicates || memoryContents.isEmpty) {
      memoryContents += a
    } else {
      for (item: MemoryItem <- memoryContents) {
        var duplicate : Boolean = false
        if (item.equals(a)) {
          duplicate = true
        }
        if (!duplicate) {
          memoryContents += a
        }
      }
    }
  }

  def += (a : MemoryItem) = {
    this.addMemory(a)
  }

  def -= (a : MemoryItem) = {
    memoryContents -= a
  }
}