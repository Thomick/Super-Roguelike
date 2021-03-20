package game_entities

import items._
import map_objects._
import scala.collection._

trait HasInventory extends Character {
  val inventory = mutable.ArrayBuffer[AbstractItem]()

  def getInventoryItems(): Array[AbstractItem] = inventory.toArray

  // Add an item to the inventory
  def obtainItem(item: AbstractItem): Unit = {
    inventory += item
    writeLog(name + " obtains " + item.name)
  }

  // pick up an item from the ground at current position (if any)
  // returns true if an item has been picked up, else false
  def pickUpItem(): Boolean = {
    board.pickUpItem(pos, 0) match {
      case Some(item) => {
        obtainItem(item)
        true
      }
      case None => false
    }
  }

  // Drop the item of index (itemSlot) on the ground (if possible)
  // returns true if the item has been dropped
  def dropItem(itemSlot: Int): Boolean = {
    if (!isSlotEmpty(itemSlot)) {
      val item = inventory(itemSlot)
      if (board.addItem(new ItemEntity(pos, board, item), pos)) {
        inventory.remove(itemSlot)
        return true
      }
    }
    return false
  }

  // Test if an itemSlot is empty (ie. index out of bounds)
  def isSlotEmpty(itemSlot: Int, container: Seq[AbstractItem] = inventory): Boolean = {
    if (itemSlot < container.length)
      return false
    println("There is no item in this slot")
    return true
  }

  def canThrowItem(itemSlot : Int) : Boolean = (!isSlotEmpty(itemSlot) && inventory(itemSlot).isInstanceOf[Throwable])

  def canFireItem(itemSlot : Int) : Boolean = (!isSlotEmpty(itemSlot) && inventory(itemSlot).isInstanceOf[RangedWeapon])

  def itemRange(itemSlot : Int) : Int = inventory(itemSlot).asInstanceOf[RangedWeapon].range

  // Throw an item if the item is throwable, the effect depends on the item
  def throwItem(itemSlot: Int, pos : (Int,Int)): Unit = {
    val item = inventory(itemSlot)
    item.asInstanceOf[Throwable].throwItem(board,pos)
    inventory.remove(itemSlot)
    //writeLog(item.name + " can't be thrown")
  }

  // Consume an item if the item is consumable, effect depends on the item
  def consumeItem(itemSlot: Int): Boolean = {
    if (!isSlotEmpty(itemSlot)) {
      val item = inventory(itemSlot)
      if (item.isInstanceOf[Consumable]) {
        val effect = item.asInstanceOf[Consumable].consume(this)
        writeLog(effect)
        inventory.remove(itemSlot)
        return true
      }
      writeLog(item.name + " can't be consumed")
    }
    return false
  }
}

// This trait allows the character to equip an equipable item and apply the stats bonus
// Items can only be equiped if the body part on which they must be equiped is free
trait CanEquip extends Character with HasInventory {
  val equipedItems = mutable.ArrayBuffer[Equipable]()
  val equipedRangedWeapons = mutable.ArrayBuffer[RangedWeapon]()

  def getEquipedItems(): Array[AbstractItem] = equipedItems.toArray

  override def getDef(): Int =
    baseDef + equipedItems.foldLeft[Int](0)((s, item) => s + item.bonusDef)
  override def getAtt(): Int =
    baseAtt + equipedItems.foldLeft[Int](0)((s, item) => s + item.bonusAtt)
  override def getMaxHP(): Int =
    baseMaxHP + equipedItems.foldLeft[Int](0)((s, item) => s + item.bonusHP)

  def fire(mainWeapon: Boolean, itemSlot: Int, pos : (Int,Int)): Unit = {
    if (mainWeapon) {
      equipedRangedWeapons(0).shoot(board,pos)
    }
    else {
      inventory(itemSlot).asInstanceOf[RangedWeapon].shoot(board,pos)
    }
    //writeLog(item.name + " can't be thrown")
  }


  def equipItem(itemSlot: Int): Boolean = {
    if (!isSlotEmpty(itemSlot)) {
      if (inventory(itemSlot).isInstanceOf[Equipable]) {
        val item = inventory(itemSlot).asInstanceOf[Equipable]
        if (isBodyPartFree(item.part)) {
          equipedItems += item
          inventory.remove(itemSlot)
          if (item.isInstanceOf[RangedWeapon]) {
            equipedRangedWeapons += item.asInstanceOf[RangedWeapon]
          }
          writeLog("You equip " + item.name)
          return true
        } else {
          writeLog("You already equiped items on this body part")
        }
      } else {
        writeLog("You can not equip " + inventory(itemSlot).name)
      }
    }
    return false
  }

  def unequipItem(itemSlot: Int): Boolean = {
    if (!isSlotEmpty(itemSlot, equipedItems)) {
      val item = equipedItems(itemSlot)
      inventory += item
      equipedItems.remove(itemSlot)
      if (item.isInstanceOf[RangedWeapon]) {
        equipedRangedWeapons -= item.asInstanceOf[RangedWeapon]
      }
      writeLog("You unequip " + item.name)
      return true
    }
    return false
  }

  def isBodyPartFree(part: BodyPart.Value): Boolean
}

// Trait describing a body type, implements isBodyPartFree
trait Humanoid extends CanEquip {
  def isBodyPartFree(part: BodyPart.Value): Boolean = {
    val samePartCount = equipedItems.foldLeft[Int](0)((c, item) =>
      if (item.part == part)
        c + 1
      else
        c
    )
    part match {
      case BodyPart.Torso => samePartCount < 1
      case BodyPart.Arm   => samePartCount < 2
      case BodyPart.Head  => samePartCount < 1
      case BodyPart.Hand  => samePartCount < 2
      case BodyPart.Legs  => samePartCount < 1
      case BodyPart.Feet  => samePartCount < 1
      case BodyPart.Other => true
      case _              => false
    }
  }
}
