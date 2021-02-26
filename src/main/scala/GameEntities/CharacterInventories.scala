package GameEntities

import Items._
import map_objects._
import scala.collection._

trait HasInventory extends Character {
  val inventory = mutable.ArrayBuffer[AbstractItem]()

  def getInventoryItems(): Array[AbstractItem] = inventory.toArray

  def obtainItem(item: AbstractItem) = {
    inventory += item
    writeLog(name + " obtains " + item.name)
  }

  def destroyItem(item: AbstractItem) = {
    inventory -= item
  }

  def pickUpItem(): Boolean = {
    board.pickUpItem(pos, 0) match {
      case Some(item) => {
        obtainItem(item)
        true
      }
      case None => false
    }
  }

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

  def isSlotEmpty(itemSlot: Int, container: Seq[AbstractItem] = inventory): Boolean = {
    if (itemSlot < container.length)
      return false
    println("There is no item in this slot")
    return true
  }

  def throwItem(itemSlot: Int, dir: Direction.Value): Boolean = {
    if (!isSlotEmpty(itemSlot)) {
      val item = inventory(itemSlot)
      if (item.isInstanceOf[Throwable]) {
        val success = item.asInstanceOf[Throwable].throwItem(this, board, pos, dir)
        if (success) {
          inventory.remove(itemSlot)
          return true
        }
      }
      writeLog(item.name + " can't be thrown")
    }
    return false
  }

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

trait CanEquip extends Character with HasInventory {

  val equipedItems = mutable.ArrayBuffer[Equipable]()

  def getEquipedItems(): Array[AbstractItem] = equipedItems.toArray

  override def getDef(): Int =
    baseDef + equipedItems.foldLeft[Int](0)((s, item) => s + item.bonusDef)

  override def getAtt(): Int =
    baseAtt + equipedItems.foldLeft[Int](0)((s, item) => s + item.bonusAtt)

  override def getMaxHP(): Int =
    baseMaxHP + equipedItems.foldLeft[Int](0)((s, item) => s + item.bonusHP)

  def equipItem(itemSlot: Int): Boolean = {
    if (!isSlotEmpty(itemSlot)) {
      if (inventory(itemSlot).isInstanceOf[Equipable]) {
        val item = inventory(itemSlot).asInstanceOf[Equipable]
        if (isBodyPartFree(item.part)) {
          equipedItems += item
          inventory.remove(itemSlot)
          writeLog("You equip " + item.name)
          return true
        } else {
          writeLog("You can't carry anymore item on this body part.")
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
      writeLog("You unequip " + item.name)
      return true
    }
    return false
  }

  def isBodyPartFree(part: BodyPart.Value): Boolean
}

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
