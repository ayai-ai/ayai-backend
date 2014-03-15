package ayai.persistence

import ayai.components.{Character, Inventory, Item, Equipment}
import ayai.apps.Constants

import scala.collection.mutable.ArrayBuffer

import crane.Entity

/** External Imports **/
import org.squeryl.Session
import org.squeryl.SessionFactory
import org.squeryl.adapters.H2Adapter
import org.squeryl.PrimitiveTypeMode._
import org.mindrot.jbcrypt.BCrypt

/** Socko Imports **/
import org.mashupbots.socko.events.{HttpRequestEvent, HttpResponseStatus}

import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

object InventoryTable {

  //MUST BE CALLED FROM WITHIN TRANSACTION
  //This is so it can be done multiple times within a single transaction.
  def upsertRow(inventoryRow: InventoryRow) {
    val itemQuery =
      from(AyaiDB.inventory)(row =>
        where(row.playerId === inventoryRow.playerId
          and row.itemId === inventoryRow.itemId)
        select(row)
      )

    if(itemQuery.size == 0) {
      AyaiDB.inventory.insert(inventoryRow)
    }
    else if (itemQuery.size == 1) {
      AyaiDB.inventory.update(inventoryRow)
    }
    else {
      println("Error: duplicate inventory row.")
    }
  }

  //MUST BE CALLED FROM WITHIN TRANSACTION
  //This is so it can be done multiple times within a single transaction.
  //Deletes all copies of the item in the character's inventory
  def deleteAllOfItem(itemEnity: Entity, characterEntity: Entity) {
   (itemEnity.getComponent(classOf[Item]),
      characterEntity.getComponent(classOf[Character])) match {
      case (Some(item: Item), Some(character: Character)) =>
        val characterQuery =
          from(AyaiDB.characters)(row =>
            where(row.name === character.name)
            select(row)
          )

        AyaiDB.inventory.deleteWhere(row =>
          (row.playerId === characterQuery.single.id) and
          (row.itemId === item.id)
        )

      case _ =>
        println("Error in InventoryTable - deleteAllOfItem")
      }
  }

  //MUST BE CALLED FROM WITHIN TRANSACTION
  //This is so it can be done multiple times within a single transaction.
  def deleteItem(itemEnity: Entity, characterEntity: Entity) {
   (itemEnity.getComponent(classOf[Item]),
      characterEntity.getComponent(classOf[Character])) match {
      case (Some(item: Item), Some(character: Character)) =>
        val characterQuery =
            from(AyaiDB.characters)(row =>
              where(row.name === character.name)
              select(row)
            )

        val itemQuery =
          from(AyaiDB.inventory)(row =>
            where(row.playerId === characterQuery.single.id
              and row.itemId === item.id)
            select(row)
          )

        if (itemQuery.size == 1) {
          var inventoryRow = itemQuery.single
          if(inventoryRow.quantity > 0) {
            var newRow = new InventoryRow(inventoryRow.playerId, inventoryRow.itemId, inventoryRow.quantity - 1, inventoryRow.equipped)
            AyaiDB.inventory.update(newRow)
          }
          else {
            AyaiDB.inventory.deleteWhere(row =>
              (row.playerId === inventoryRow.playerId) and
              (row.itemId === item.id)
            )
          }
        }
        else {
          println("Error: duplicate inventory row.")
        }
      case _ =>
        println("Error in InventoryTable - deleteItem")
    }
  }

  def slotToInt(slot: String): Int = {
    slot match {
      case "weapon1" =>
        1
      case "weapon2" =>
        2
      case "helmet" =>
        4
      case "feet" =>
        8
      case "torso" =>
        16
      case "legs" =>
        32
      case _ =>
        println("Slot not found.")
        -1
    }
  }

  //Saves all items in a character's inventory to the database.
  //Does not delete, which could be a problem later
  def saveInventory(entity: Entity) {
    (entity.getComponent(classOf[Inventory]),
      entity.getComponent(classOf[Character]),
      entity.getComponent(classOf[Equipment])) match {
      case (Some(inventory: Inventory), Some(character: Character), Some(equipment: Equipment)) =>
        AyaiDB.getCharacter(character.name) match {
          case Some(characterRow: CharacterRow) =>
            Class.forName("org.h2.Driver");
            SessionFactory.concreteFactory = Some (() =>
                Session.create(
                java.sql.DriverManager.getConnection("jdbc:h2:ayai"),
                new H2Adapter))


//make equipped field int for slot
//combine inventory and equipment into one list
//do the groupby
//equipped field = lookup
//need a map of id -> slotVal
// m1 foreach {case (key, value) => println (key + "-->" + value)}
            val idsToSlots = equipment.equipmentMap filter (_._2.id >= 0) map
                {case (slot: String, item: Item) => (slotToInt(slot), item.id)} groupBy(
                  (_._1)) map(
                    )

              // case (slot: String, item: Item) => (item.id >= 0))
                // {case (slot: String, item: Item) => (item.id, slotToInt(slot))}
            println(idsToSlots)

            val equippedIds = equipment.equipmentMap.values.toList map ((item:Item) => item.id)

            val inventoryRows = inventory.inventory groupBy(
              (item:Item) => item.id) map {
                p => new InventoryRow(characterRow.id, p._1, p._2.length, false)}

            println(s"Saving $inventoryRows")
            transaction {
              inventoryRows map upsertRow
            }
          case _ =>
            println(s"Can't find account for $character.name")
        }
      case _ =>
        println("Can't get inventory table for some reason.")
    }
  }

  //Upserts the item.
  //  If it doesn't exist creates it with quantity 1.
  //  If it does exist it will increment the quantity field.
  //Don't use for a lot of saves. Each call will be a seperate transaction.
  def incrementItem(itemEnity: Entity, characterEntity: Entity) {
    incrementItemMultiple(itemEnity, characterEntity, 1)
  }

  //Upserts the item.
  //  If it doesn't exist creates it with quantity=quantity.
  //  If it does exist it will increment the quantity field by the quantity parameter.
  //Don't use this in a loop. Each call will be a seperate transaction.
  def incrementItemMultiple(itemEnity: Entity, characterEntity: Entity, quantity: Int) {
  (itemEnity.getComponent(classOf[Item]),
    characterEntity.getComponent(classOf[Character])) match {
      case(Some(item: Item), Some(character : Character)) =>
        Class.forName("org.h2.Driver");
        SessionFactory.concreteFactory = Some (() =>
            Session.create(
            java.sql.DriverManager.getConnection("jdbc:h2:ayai"),
            new H2Adapter))

        transaction {
          val characterQuery =
            from(AyaiDB.characters)(row =>
              where(row.name === character.name)
              select(row)
            )

          val itemQuery =
            from(AyaiDB.inventory)(row =>
              where(row.playerId === characterQuery.single.id
                and row.itemId === item.id)
              select(row)
            )

          if(itemQuery.size == 0) {
            AyaiDB.inventory.insert(new InventoryRow(characterQuery.single.id, item.id, quantity, false))
          }
          else if (itemQuery.size == 1) {
            var inventoryRow = itemQuery.single
            val newRow = new InventoryRow(inventoryRow.playerId, inventoryRow.itemId, inventoryRow.quantity + quantity, inventoryRow.equipped)
            AyaiDB.inventory.update(newRow)
          }
          else {
            println("Error: duplicate inventory row.")
          }
        }

      case _ =>
        println("Can't get inventory table for some reason.")
    }
  }
}
