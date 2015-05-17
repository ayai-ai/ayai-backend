package ayai.systems

import akka.actor.ActorSystem
import ayai.components._
import ayai.systems.ai.perception.PerceptionSystem
import crane.Entity

object MemorySystem {
  def apply(actorSystem: ActorSystem) = new MemorySystem(actorSystem)
}

class MemorySystem(actorSystem: ActorSystem) extends GenericPerceptionSystem(actorSystem, include = List(classOf[Memory])) {
  override def processEntity(e: Entity, deltaTime: Int): Unit = {
    if (counter == TICKS_BETWEEN_PROCESSING) {
      (e.getComponent(classOf[Memory])) match {
        case (Some(memory: Memory)) => {
          for (memoryComponent: MemoryItem <- memory.memoryContents) {
            if (memoryComponent.counter >= memory.memoryAbility) {
              forgetMemory(e, memoryComponent)
            } else {
              //Increase the age of the memory
              memoryComponent.counter += 1
            }
          }
        }
      }
      counter = 0
    }
    else {
      counter += 1
    }
  }

  override def notify(evt: PerceptionEvent): Unit = {
    val e: Entity = evt.evtTarget
    (e.getComponent(classOf[Memory])) match {
      case (Some(memory: Memory)) => {
        (evt.evtSystem) match {
          case SoundSystem => {
            storeMemory(e, new MemoryItem(evt))
          }
          case VisionSystem => {
            storeMemory(e, new MemoryItem(evt))
          }
          case _ =>
        }
      }
      case _ =>
    }
  }

  def setMemoryAttenuation(e: Entity, attenuation: Int) = {
    (e.getComponent(classOf[Memory])) match {
      case (Some(memory: Memory)) => {
        memory.memoryAbility = attenuation
      }
      case _ => {}
    }
  }

  def storeMemory(e: Entity, memoryItem: MemoryItem) = {
    (e.getComponent(classOf[Memory])) match {
      case (Some(memory: Memory)) => {
        memory += memoryItem
        (memoryItem.memoryEvent.evtSource.getComponent(classOf[Character]),
          memoryItem.memoryEvent.evtTarget.getComponent(classOf[Character])) match {
          case (Some(char1: Character), Some(char2: Character)) => {
            val msg = char1.name + " remembers " + char2.name
            val evt = new PerceptionEvent(memoryItem.memoryEvent.evtSource, memoryItem.memoryEvent.evtTarget, msg, MemorySystem)
            if (spamLog) log.warn(msg)
            if (spamLog) log.warn(memory.memoryContents.size.toString())
            PerceptionSystem(actorSystem).publish(evt)
          }
          case _ => {}
        }

      }
      case _ => {}
    }
  }

  def forgetMemory(e: Entity, memoryItem: MemoryItem) = {
    (e.getComponent(classOf[Memory])) match {
      case (Some(memory: Memory)) => {
        memory -= memoryItem
        (memoryItem.memoryEvent.evtSource.getComponent(classOf[Character]),
          memoryItem.memoryEvent.evtTarget.getComponent(classOf[Character])) match {
          case (Some(char1: Character), Some(char2: Character)) => {
            val msg = char1.name + " forgets " + char2.name
            val evt = new PerceptionEvent(memoryItem.memoryEvent.evtSource, memoryItem.memoryEvent.evtTarget, msg, MemorySystem)
            if (spamLog) log.warn(msg)
            if (spamLog) log.warn(memory.memoryContents.size.toString())

            PerceptionSystem(actorSystem).publish(evt)
          }
          case _ => {}
        }

      }
      case _ => {}
    }
  }
}

