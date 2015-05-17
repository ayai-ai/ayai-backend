package ayai.systems

import ayai.components.SenseComponent

import crane.EntityProcessingSystem
import akka.actor.ActorSystem
import org.slf4j.LoggerFactory
import scala.collection.mutable.ArrayBuffer

trait SenseSystem {
  def notify(evt: PerceptionEvent): Unit
}

abstract class GenericPerceptionSystem[S](actorSystem: ActorSystem, include: List[Class[S]] = List(classOf[SenseComponent])) extends EntityProcessingSystem(include=include) with SenseSystem {
  protected var senseSystems: ArrayBuffer[SenseSystem] = new ArrayBuffer[SenseSystem]()
  protected val log = LoggerFactory.getLogger(getClass)
  protected val spamLog = false

  protected val TICKS_BETWEEN_PROCESSING = 10
  protected var counter = 0
}