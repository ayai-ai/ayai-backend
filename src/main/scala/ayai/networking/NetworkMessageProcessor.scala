package ayai.networking

/** Ayai Imports **/
import ayai.gamestate.{Effect, EffectType}
import ayai.actions._
import ayai.components._
import ayai.networking.chat._
//import ayai.persistence.{User, UserQuery}
import ayai.apps.Constants

import crane.{Entity, World}

/** Akka Imports **/
import akka.actor.{Actor, ActorSystem, ActorRef, Props}

/** Socko Imports **/
import org.mashupbots.socko.events.WebSocketFrameEvent

/** External Imports **/
import scala.util.Random
import scala.collection.{immutable, mutable}
import scala.collection.mutable._

import java.rmi.server.UID
import ayai.apps.GameLoop

import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

class NetworkMessageProcessor(actorSystem: ActorSystem, world: World, socketMap: mutable.ConcurrentMap[String, String]) extends Actor {
  implicit val formats = Serialization.formats(NoTypeHints)

  def processMessage(message: NetworkMessage) {
    message match {
      case AddNewCharacter(id: String, x: Int, y: Int) => {
        println("Adding a character: " +  id)
        val p: Entity = world.createEntity(tag="CHARACTER"+id)
        p.components += new Position(x, y)
        p.components += new Bounds(32, 32)
        p.components += new Velocity(4, 4)
        p.components += new Movable(false, new MoveDirection(0,0))
        p.components += new Health(100,100)
        p.components += new Mana(200,200)
        p.components += new Room(Constants.STARTING_ROOM_ID)
        p.components += new Character(id)
        val inventory = new ArrayBuffer[Item]()
        inventory += new Weapon(name = "Iron Axe", value = 10,
  weight = 10, range = 0, damage = 5, damageType = "physical")
        p.components += new Inventory(inventory)
//        p.components += (new Room(1))
        world.addEntity(p)
        world.groups("CHARACTERS") += p
        world.groups("ROOM"+Constants.STARTING_ROOM_ID) += p

      }

      case RemoveCharacter(id: String) => {
        println("Removing character: " + id)
        (world.getEntityByTag("CHARACTER" + socketMap(id))) match {
          case None =>
            System.out.println(s"Can't find character attached to socket $id.")
          case Some(character : Entity) =>
            character.kill
            socketMap.remove(id)
        }
        println("CHARACTER KILLED")
      }

      case MoveMessage(webSocket: WebSocketFrameEvent, start: Boolean, direction: MoveDirection) => {
        //println("Direction: " + direction.xDirection.toString + ", " + direction.yDirection.toString)
        val id: String = socketMap(webSocket.webSocketId)
        (world.getEntityByTag("CHARACTER"+id)) match {
          case None => 
            println("Cant dind character attached to id")
          case Some(e : Entity) =>
            if (!start) {
              val oldMovement = (e.getComponent(classOf[Movable])) match {
                case Some(oldMove : Movable) => oldMove
              }
              
              e.removeComponent(classOf[Movable])
              e.components += new Movable(start, oldMovement.direction)
              return;
            }
            e.removeComponent(classOf[Movable])
            e.components += new Movable(start, direction)
        }
      }

       // give id of the item, and what action it should do (equip, use, unequip, remove from inventory)
      case AttackMessage(webSocket : WebSocketFrameEvent) => {
        //create a projectile
        println("Created Bullet")
        val id : String = socketMap(webSocket.webSocketId)
        val bulletId = (new UID()).toString

        (world.getEntityByTag("CHARACTER"+id)) match {

        //for rob temporary
        //initiator.getComponent(classOf[Health]).currentHealth -= 10
        //initiator.getComponent(classOf[Mana]).currentMana -= 20
        case Some(initiator : Entity) =>
          val position = initiator.getComponent(classOf[Position])
          val movable = initiator.getComponent(classOf[Movable])
          val character = initiator.getComponent(classOf[Character])
          (position, movable, character) match {
            case(Some(pos: Position), Some(m : Movable), Some(c : Character)) =>
              val upperLeftx = pos.x
              val upperLefty = pos.y

              println("Player position: " + upperLeftx.toString + ", " + upperLefty.toString)


              val xDirection = m.direction.xDirection
              val yDirection = m.direction.yDirection

              val topLeftOfAttackx = (33 * xDirection) + upperLeftx
              val topLeftOfAttacky = (33 * yDirection) + upperLefty

              println("Attack box position: " + topLeftOfAttackx.toString + ", " + topLeftOfAttacky.toString)

              val p: Entity = world.createEntity("ATTACK"+bulletId)

              p.components += (new Position(topLeftOfAttackx, topLeftOfAttacky))
              p.components += (new Bounds(10, 10))
              p.components += (new Attack(12));
              p.components += (c)
              world.addEntity(p)
              world.groups("ROOM"+Constants.STARTING_ROOM_ID) += p

          }
        } 
      }

      case ItemMessage(id : String, itemAction : ItemAction) => {

      }

      case OpenMessage(webSocket: WebSocketFrameEvent, containerId : String) => {
          println("Open Received!")
          val inventory = new ArrayBuffer[Item]()
        inventory += new Weapon(name = "Orcrist", value = 100000000,
  weight = 10, range = 1, damage = 500000, damageType = "physical")

          val fakeChest = new Inventory(inventory)

          val jsonLift = 
            ("type" -> "open") ~
            ("containerId" -> containerId) ~
            (fakeChest.asJson)


          println(compact(render(jsonLift)))

          webSocket.writeText(compact(render(jsonLift)))
      }

      case SocketCharacterMap(webSocket: WebSocketFrameEvent, id: String) => {
        println(webSocket)
        socketMap(webSocket.webSocketId) = id
      }

      case PublicChatMessage(message: String, sender: String) => {
        //// Will do this later - we don't have accounts working quite yet, so we will wait until that is ready
        //var sUser = None: Option[User]
        //sUser = UserQuery.getByUsername(sender)
        //
        //sUser match {
        //  case Some(user) =>
        //    val mh = new ChatHolder(new PublicChat(message, user))
        //    actorSystem.actorOf(Props(new ChatReceiver())) ! mh
        //  case _ =>
        //    println("Error from PublicChatMessage")
        //}
      }
      case _ => println("Error from NetworkMessageProcessor.")
    } 
  }

  def receive = {
    case ProcessMessage(message) => processMessage(message)
    case _ => println("Error: from interpreter.")
  }
}
