package ayai.gamestate

/**
 * ayai.gamestate.MessageProcessorSupervisor
 * Supervising actor for the MessageProcessor
 */

import ayai.networking._

/** Akka Imports **/
import akka.actor.{Actor, ActorRef, Props, OneForOneStrategy}
import akka.actor.SupervisorStrategy.Escalate
import akka.routing.FromConfig

/** External Imports **/
import scala.concurrent.duration._
import scala.collection.concurrent.{Map => ConcurrentMap}
import scala.collection.mutable.HashMap

object MessageProcessorSupervisor {
  def apply(world: RoomWorld) = new MessageProcessorSupervisor(world)
}

class MessageProcessorSupervisor(world: RoomWorld) extends Actor {

  // Escalate exceptions, try up to 10 times, if one actor fails, try just that one again
  val escalator = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 5 seconds) {
    case _: Exception => Escalate
  }

val router = context.system.actorOf(Props(
  MessageProcessor(world)).withRouter(FromConfig.withSupervisorStrategy(escalator)), name = "processorrouter")

  def receive = {
    case message: Message =>
      router forward message 
    case _ =>
      println("Error: in interpreter supervisor")
  }

}
