package ayai.systems

import akka.actor.ActorSystem
import ayai.components.{SoundEntity, _}
import ayai.systems.ai.perception.PerceptionSystem
import crane.Entity

object SoundSystem {
  def apply(actorSystem: ActorSystem) = new SoundSystem(actorSystem)
}

class SoundSystem(actorSystem: ActorSystem) extends GenericPerceptionSystem(actorSystem, include=List(classOf[Hearing])) {
  override def processEntity(e: Entity, deltaTime: Int): Unit = {
    if (counter == TICKS_BETWEEN_PROCESSING) {
      var soundProducingEntity = e
      (soundProducingEntity.getComponent(classOf[Position]), soundProducingEntity.getComponent(classOf[Bounds]),
        soundProducingEntity.getComponent(classOf[SoundProducing]), soundProducingEntity.getComponent(classOf[Actionable])) match {
        case (Some(soundPosition: Position), Some(bounds: Bounds), Some(soundProd: SoundProducing), Some(actionable: Actionable)) => {
          if (actionable.active) {

            val soundEntity = new SoundEntity(soundProd.intensity, soundPosition)

            val hearingEntities = world.getEntitiesWithExclusions(include = List(classOf[Position], classOf[Bounds], classOf[Hearing]),
              exclude = List(classOf[Respawn], classOf[Transport], classOf[Dead]))

            for (hearingEntity <- hearingEntities) {
              (hearingEntity.getComponent(classOf[Position]), hearingEntity.getComponent(classOf[Hearing])) match {
                case (Some(hearPosition: Position), Some(hearing: Hearing)) => {

                  if (soundProducingEntity != hearingEntity && (soundEntity.intensity * hearing.hearingAbility) > getDistance(soundEntity.origin, hearPosition)) {

                    (soundProducingEntity.getComponent(classOf[Character]),
                      hearingEntity.getComponent(classOf[Character])) match {
                      case (Some(char1: Character), Some(char2: Character)) => {

                        val msg = char2.name + " hears " + char1.name
                        val evt = new PerceptionEvent(soundProducingEntity, hearingEntity, msg, SoundSystem)
                        if (spamLog) log.warn(msg)
                        PerceptionSystem(actorSystem).publish(evt)
                      }
                      case _ =>
                    }
                  }
                }
                case _ =>
              }
            }
          }
        }
        case _ =>
      }
      counter = 0
    }
    else {
      counter+=1
    }
  }

  def calculateSoundPropagation(soundEntity: Entity) {

  }

  def getDistance(p1: Position, p2: Position): Int = {
    // Manhattan distance
    math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y)
  }

  override def notify(evt: PerceptionEvent): Unit = {
    if (spamLog) log.warn("Event Received: "+evt.evtMsg)
  }
}
