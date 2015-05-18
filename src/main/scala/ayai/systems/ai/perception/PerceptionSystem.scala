package ayai.systems.ai.perception

import akka.actor.ActorSystem
import ayai.components.SenseComponent
import ayai.systems._
import crane.Entity

import scala.collection.mutable.ArrayBuffer

/**
 * Created by Daniel on 5/12/2015.
 */
object PerceptionSystem {
  def apply(actorSystem: ActorSystem) = new PerceptionSystem(actorSystem)
}

class PerceptionSystem(actorSystem: ActorSystem) extends GenericPerceptionSystem(actorSystem, include=List(classOf[SenseComponent])){
  def getSensSystems() : ArrayBuffer[SenseSystem] = {return senseSystems}

  senseSystems += VisionSystem(actorSystem)
  senseSystems += SoundSystem(actorSystem)
  senseSystems += MemorySystem(actorSystem)
  senseSystems += CommunicationSystem(actorSystem)

  override def processEntity(e: Entity, delta: Int): Unit = {}

  def publish(evt: PerceptionEvent): Unit = {
    this.notify(evt)
    for (sys : SenseSystem <- senseSystems) {
      sys.notify(evt)
    }
  }

  override def notify(evt: PerceptionEvent): Unit = {
    if (spamLog) log.warn("Event Received: "+evt.evtMsg)
  }
}
