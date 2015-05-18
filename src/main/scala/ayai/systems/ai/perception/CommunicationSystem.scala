package ayai.systems

import akka.actor.ActorSystem
import ayai.components._
import crane.Entity

object CommunicationSystem {
  def apply(actorSystem: ActorSystem) = new CommunicationSystem(actorSystem)
}

class CommunicationSystem(actorSystem: ActorSystem) extends GenericPerceptionSystem(actorSystem, include=List(classOf[SenseComponent])) {
  override def processEntity(e: Entity, deltaTime: Int): Unit = {

  }

  def inContact(e: Entity, e2: Entity): Boolean = ???

  def communicate(e: Entity, e2: Entity) = ???

  override def notify(evt: PerceptionEvent): Unit = {
    if (spamLog) log.warn("Event Received: "+evt.evtMsg)
  }
}