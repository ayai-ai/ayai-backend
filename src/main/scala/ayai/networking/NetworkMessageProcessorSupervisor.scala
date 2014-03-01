package ayai.networking

/**
 * ayai.networking.NetworkMessageProcessorSupervisor
 * Supervising actor for the NetworkMessageProcessor
 */

/** Akka Imports **/
import akka.actor.{Actor, ActorRef, Props, OneForOneStrategy}
import akka.actor.SupervisorStrategy.Escalate
import akka.routing.FromConfig

/** Crane Imports **/
import crane.{World}

/** External Imports **/
import scala.concurrent.duration._
import scala.collection.concurrent.{Map => ConcurrentMap}

class NetworkMessageProcessorSupervisor(world: World, socketMap: ConcurrentMap[String, String]) extends Actor {

  // Escalate exceptions, try up to 10 times, if one actor fails, try just that one again
  val escalator = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 5 seconds) {
    case _: Exception => Escalate
  }

val router = context.system.actorOf(Props(
  new NetworkMessageProcessor(world,socketMap)).withRouter(
    FromConfig.withSupervisorStrategy(escalator)),
  name = "processorrouter")

  def receive = {
    case message: NetworkMessage =>
      router forward message
    case _ =>
      println("Error: in processor supervisor")
  }

}