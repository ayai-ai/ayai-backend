package ayai.apps

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{ ExecutionContext, Promise }
import java.rmi.server.UID

import ayai.systems.{MovementSystem,CollisionSystem,RoomChangingSystem}
import ayai.gamestate.{Effect, EffectType, GameStateSerializer, CharacterRadius, MapRequest}
import crane.World
import crane.Entity

import ayai.networking._
import java.lang.Boolean
import ayai.components._
import ayai.persistence._

/** Socko Imports **/
import org.mashupbots.socko.events.WebSocketFrameEvent


import net.liftweb.json._
import net.liftweb.json.JsonDSL._

import scala.collection.{immutable, mutable}
import scala.collection.mutable._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.collection.JavaConversions._

import scala.collection.mutable.HashMap

import scala.io.Source

import crane.{EntityProcessingSystem}

object GameLoop {
  var roomHash : HashMap[Int, Entity] = HashMap.empty[Int, Entity]

  var running : Boolean = _

  def main(args: Array[String]) {
    running = true
    DBCreation.ensureDbExists()

    var socketMap: mutable.ConcurrentMap[String, String] = new java.util.concurrent.ConcurrentHashMap[String, String]
    var world: World = new World()

    world.createGroup("ROOMS")    
    world.createGroup("CHARACTERS")
    var room : Entity = EntityFactory.loadRoomFromJson(world, Constants.STARTING_ROOM_ID, "map3.json")
    roomHash.put(Constants.STARTING_ROOM_ID, room)
    world.createGroup("ROOM"+Constants.STARTING_ROOM_ID)
    world.addEntity(room)

    room = EntityFactory.loadRoomFromJson(world, 1, "map2.json")
    roomHash.put(1, room)
    world.createGroup("ROOM"+1)
    world.addEntity(room)
    //create a room 
    //room.addToWorld

    ItemFactory.bootup(world)

    world.addSystem(new MovementSystem(roomHash))
    world.addSystem(new RoomChangingSystem(roomHash))
    world.addSystem(new CollisionSystem(world))
    //world.initialize()
    
    //load all rooms


    implicit val timeout = Timeout(Constants.NETWORK_TIMEOUT seconds)

    val networkSystem = ActorSystem("NetworkSystem")
    val messageQueue = networkSystem.actorOf(Props(new NetworkMessageQueue()))
    val interpreter = networkSystem.actorOf(Props(new NetworkMessageInterpreter(messageQueue)))
    val messageProcessor = networkSystem.actorOf(Props(new NetworkMessageProcessor(networkSystem, world, socketMap)))
    val authorization = networkSystem.actorOf(Props(new AuthorizationProcessor()))

    val serializer = networkSystem.actorOf(Props(new GameStateSerializer(world, Constants.LOAD_RADIUS)) , name = (new UID()).toString)

    val receptionist = new SockoServer(networkSystem, interpreter, messageQueue, authorization)
    receptionist.run(Constants.SERVER_PORT)

    //GAME LOOP RUNS AS LONG AS SERVER IS UP
    while(running) {
      //get the time 
      val start = System.currentTimeMillis
      world.process()

      val future = messageQueue ? new FlushMessages() // enabled by the “ask” import
      val result = Await.result(future, timeout.duration).asInstanceOf[QueuedMessages]

      result.messages.foreach { message =>
        messageProcessor ! new ProcessMessage(message)
      }

      val characterEntities =  world.groups("CHARACTERS")

      for (characterEntity <- characterEntities) {
        //need better way of figuring if something is bullet, or figuring 
        // out what each entity has
        val characterId: String = (characterEntity.getComponent(classOf[Character])) match {
          case Some(c : Character) => c.id 
          case None =>
            println("BLAAAA")
            ""
        }
        if(!characterEntity.getComponent(classOf[MapChange]).isEmpty) {
          characterEntity.getComponent(classOf[MapChange]) match {
            case Some(map : MapChange) =>
              val future2 = serializer ? new MapRequest(roomHash(map.roomId))
              val result2 = Await.result(future2, timeout.duration).asInstanceOf[String]
              val actorSelection1 = networkSystem.actorSelection("user/SockoSender"+characterId)
              println(result2)
              actorSelection1 ! new ConnectionWrite(result2)  
              characterEntity.removeComponent(classOf[MapChange])
          }
          
        }

        //This is how we get character specific info, once we actually integrate this in.
        val future1 = serializer ? new CharacterRadius(characterId)
        val result1 = Await.result(future1, timeout.duration).asInstanceOf[String]
        val actorSelection = networkSystem.actorSelection("user/SockoSender"+characterId)
        actorSelection ! new ConnectionWrite(result1)
      }
      val end = System.currentTimeMillis
      if((end - start) < (1000/Constants.FRAMES_PER_SECOND)) {
        Thread.sleep((1000 / Constants.FRAMES_PER_SECOND) - (end-start))
      }
    }
  }
}


