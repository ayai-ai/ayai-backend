package ayai.gamestate

/** Ayai Imports **/
import ayai.actions._
import ayai.components._
import ayai.networking._
import ayai.networking.chat._
import ayai.persistence.CharacterTable
import ayai.factories.EntityFactory
import ayai.apps.{Constants, GameLoop}

/** Crane Imports **/
import crane.{Entity}

/** Akka Imports **/
import akka.actor.{Actor, Props}
import akka.actor.Status.{Success, Failure}
import akka.pattern.ask
import akka.util.Timeout

/** External Imports **/
import scala.util.Random
import scala.collection.concurrent.{Map => ConcurrentMap}
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Promise, Future}


import java.rmi.server.UID

import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

import org.slf4j.{Logger, LoggerFactory}

object MessageProcessor {
  implicit val timeout = Timeout(2 seconds)
  import ExecutionContext.Implicits.global


  def apply(world: RoomWorld) = new MessageProcessor(world)
}

class MessageProcessor(world: RoomWorld) extends Actor {
  implicit val formats = Serialization.formats(NoTypeHints)
  implicit val timeout = Timeout(2 seconds)
  val actorSystem = context.system
  val characterTable = actorSystem.actorOf(Props(new CharacterTable()))
  private val log = LoggerFactory.getLogger(getClass)

  def processMessage(message: Message) {
    message match {
      //Should take characterId: Long as a parameter instead of characterName
      //However can't do that until front end actually gives me the characterId
      case AddNewCharacter(userId: String, characterName: String, x: Int, y: Int) => {
        val actor = actorSystem.actorSelection(s"user/SockoSender$userId")
        // CHANGE THIS SECTION WHEN DOING DATABASE WORK
        EntityFactory.loadCharacter(world, userId, "Ness", x, y, actor, actorSystem) //Should use characterId instead of characterName
        sender ! Success
      }

      case RemoveCharacter(socketId: String) => {
        println("trying a get?")
        val future = context.system.actorSelection("user/SocketUserMap") ? GetUserId(socketId)
        val userId = Await.result(future, timeout.duration).asInstanceOf[String]
        println(s"Removing character: $userId")
        world.getEntityByTag(s"CHARACTER$userId") match {
          case None =>
            System.out.println(s"Can't find character attached to socket $socketId.")
          case Some(character : Entity) =>
            character.kill
            context.system.actorSelection("user/SocketUserMap") ! RemoveSocketUser(socketId)
        }
        sender ! Success
      }

      case MoveMessage(userId: String, start: Boolean, direction: MoveDirection) => {
        (world.getEntityByTag(s"CHARACTER$userId")) match {
          case None =>
            println(s"Can't find character attached to id: $userId")
          case Some(e: Entity) =>
              val oldMovement = (e.getComponent(classOf[Actionable])) match {
                case Some(oldMove : Actionable) =>
                  oldMove.active = start
                  oldMove.action = direction
                case _ =>
                  log.warn("a07270d: getComponent failed to return anything")
              }
        }
        sender ! Success
      }

       // give id of the item, and what action it should do (equip, use, unequip, remove from inventory)
      case AttackMessage(userId: String) => {
        //create a projectile
        println("Created Bullet")
        val bulletId = (new UID()).toString

        (world.getEntityByTag(s"CHARACTER$userId")) match {

        case Some(initiator: Entity) =>
          val position = initiator.getComponent(classOf[Position])
          val movable = initiator.getComponent(classOf[Actionable])
          val character = initiator.getComponent(classOf[Character])
          (position, movable, character) match {
            case(Some(pos: Position), Some(a: Actionable), Some(c: Character)) =>
              val m = a.action match {
                case (move: MoveDirection) => move
                case _ => println("Not match for movedirection")
                return

              }
              val upperLeftx = pos.x
              val upperLefty = pos.y

              println("Player position: " + upperLeftx.toString + ", " + upperLefty.toString)

              val xDirection = m.xDirection
              val yDirection = m.yDirection

              val topLeftOfAttackx = (33 * xDirection) + upperLeftx
              val topLeftOfAttacky = (33 * yDirection) + upperLefty

              println("Attack box position: " + topLeftOfAttackx.toString + ", " + topLeftOfAttacky.toString)

              val p: Entity = world.createEntity("ATTACK"+bulletId)

              p.components += (new Position(topLeftOfAttackx, topLeftOfAttacky))
              p.components += (new Bounds(10, 10))
              p.components += (new Attack(initiator));
              p.components += (new Frame(10,0))
              world.addEntity(p)
              //world.groups("ROOM"+Constants.STARTING_ROOM_ID) += p
            case _ =>
              log.warn("424e244: getComponent failed to return anything")
          }
          case _ =>
            log.warn("8a87265: getComponent failed to return anything")
        }
        sender ! Success
      }

      case ItemMessage(userId: String, itemAction: ItemAction) => {
        sender ! Success
      }

      case OpenMessage(userId: String, containerId : String) => {
        sender ! Success
      }

      case SocketCharacterMap(socketId: String, userId: String) => {
        println("ADD1")
        //context.system.actorSelection("user/SocketUserMap") ! AddSocketUser(socketId, userId)
        sender ! Success
      }

      case CharacterList(socketId: String, accountName: String) => {
        characterTable ! new CharacterList(socketId, accountName)
        sender ! Success
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
      case _ => println("Error from MessageProcessor.")
        sender ! Failure
    } 
  }

  def receive = {
    case ProcessMessage(message) => processMessage(message)
    case _ => println("Error: from interpreter.")
      sender ! Failure
  }
}