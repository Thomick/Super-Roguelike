package GameEntities

import Items._
import map_objects._
import scala.collection._
import scala.math.min

abstract class Character(init_pos: (Int, Int), b: GameBoard)
    extends GameEntity(init_pos, b) {

  val baseMaxHP: Int = 10
  val baseDef: Int = 0
  val baseAtt: Int = 0
  var currentMaxHP: Int = 10
  var currentDef: Int = 0
  var currentAtt: Int = 0
  var currentHP: Int = 10
  def move(dir: Direction.Value): Unit = {
    var nextPos = Direction.nextPos(pos, dir)
    if (board.isFree(nextPos)) {
      board.entityMoved(this, nextPos)
      pos = nextPos
    }
  }

  def getDef(): Int = baseDef
  def getAtt(): Int = baseAtt
  def getMaxHP(): Int = baseMaxHP

  def updateMaxStat(): Unit = {
    currentAtt = getAtt()
    currentDef = getDef()
    currentMaxHP = getMaxHP()
    currentHP = min(currentHP, currentMaxHP)
  }

}

trait HasInventory extends Character {
  val inventory = mutable.ArrayBuffer[AbstractItem]()

  def obtainItem(item: AbstractItem) = {
    inventory += item
    println("Got Item")
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
    if (itemSlot < inventory.length) {
      val item = inventory(itemSlot)
      item.drop(this, board, pos)
      inventory.remove(itemSlot)
      true
    } else {
      println("There is no item in this slot")
      return false
    }
  }

  def throwItem(itemSlot: Int, dir: Direction.Value): Boolean = {
    if (itemSlot < inventory.length) {
      val item = inventory(itemSlot)
      if (item.isInstanceOf[Throwable]) {
        item.asInstanceOf[Throwable].throwItem(this, board, pos, dir)
        inventory.remove(itemSlot)
        return true
      } else {
        println("This item can't be thrown")
        return false
      }
    } else {
      println("There is no item in this slot")
      return false
    }
  }

  def consumeItem(itemSlot: Int): Boolean = {
    if (itemSlot < inventory.length) {
      val item = inventory(itemSlot)
      if (item.isInstanceOf[Consumable]) {
        item.asInstanceOf[Consumable].consume(this)
        inventory.remove(itemSlot)
        true
      } else {
        println("This item can't be consumed")
        return false
      }
    } else {
      println("There is no item in this slot")
      return false
    }
  }
}

trait CanEquip extends Character with HasInventory {

  val equipedItems = mutable.ArrayBuffer[Equipable]()
  override def getDef(): Int = {
    baseDef + equipedItems.foldLeft[Int](0)((s, item) =>
      if (item.isInstanceOf[Passive])
        s + item.asInstanceOf[Passive].bonusDef
      else
        s
    )
  }
  override def getAtt(): Int = {
    baseAtt + equipedItems.foldLeft[Int](0)((s, item) =>
      if (item.isInstanceOf[Passive])
        s + item.asInstanceOf[Passive].bonusAtt
      else
        s
    )
  }
  override def getMaxHP(): Int = {
    baseMaxHP + equipedItems.foldLeft[Int](0)((s, item) =>
      if (item.isInstanceOf[Passive])
        s + item.asInstanceOf[Passive].bonusHP
      else
        s
    )
  }
  def equipItem(itemSlot: Int): Boolean = {
    if (itemSlot < inventory.length) {
      if (inventory(itemSlot).isInstanceOf[Equipable]) {
        val item = inventory(itemSlot).asInstanceOf[Equipable]
        if (isBodyPartFree(item.part)) {
          equipedItems += item
          inventory.remove(itemSlot)
          updateMaxStat()
          return true
        }
      }
      return false
    } else {
      println("There is no item in this slot")
      return false
    }
  }

  def unequipItem(itemSlot: Int): Boolean = {
    if (itemSlot < equipedItems.length) {
      val item = equipedItems(itemSlot)
      inventory += item
      equipedItems.remove(itemSlot)
      updateMaxStat()
      return true
    } else {
      println("There is no equipment in this slot")
      return false
    }
  }

  def isBodyPartFree(part: BodyPart.Value): Boolean
}

trait Humanoid extends CanEquip {
  def isBodyPartFree(part: BodyPart.Value): Boolean = {
    val samePartCount = equipedItems.foldLeft[Int](0)((c, item) =>
      if (item.part == part) c + 1 else c
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