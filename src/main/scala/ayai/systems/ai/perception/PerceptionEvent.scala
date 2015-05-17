package ayai.systems

import crane.Entity

class PerceptionEvent(source : Entity, target : Entity, msg : String, system : scala.AnyRef) {
  final val evtSource : Entity = source
  final val evtTarget : Entity = target
  final val evtMsg : String = msg
  final val evtSystem = system
}