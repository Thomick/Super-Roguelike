package GameEntities

import Items._
import map_objects._
import scala.collection._
import scala.math.min
import scala.util.Random
import scala.math.{min, max}

abstract class Character(
    init_pos: (Int, Int),
    b: GameBoard,
    hasLogs: Boolean = false
) extends GameEntity(init_pos, b, hasLogs) {

  val baseMaxHP: Int = 10
  val baseDef: Int = 0
  val baseAtt: Int = 0
  var currentHP: Int = 10

  // Move the character(C1) to a new position on the board
  // If there another character(C2) at this position, triggers the the current character action on the other character(C2)
  def move(nextPos: (Int, Int)): Unit = {
    if (!(pos == nextPos))
      if (board.isFree(nextPos)) {
        board.entityMoved(this, nextPos)
        pos = nextPos
      } else if (board.hasCharacter(nextPos)) {
        action(board.getCharacter(nextPos))
      }
  }

  // Move the character to an adjacent square according to the specified direction
  def moveDir(dir: Direction.Value): Unit = move(Direction.nextPos(pos, dir))

  // Action of the current character on another character
  // (Usually triggered during collisions)
  def action(c: Character): Unit

  // Hand to hand attack based on the character stats
  def attack(c: Character): Unit = {
    val rnd = new Random
    val damage = max(0, (getAtt() * (1 + 3 * rnd.nextGaussian())).toInt)
    val (effectiveDamage, died) = c.take_damage(this, damage)
    writeLog(name + " deals " + effectiveDamage.toString + " damages to " + c.name)
    if (died)
      writeLog(name + " kills " + c.name)
  }

  // Compute effective damage based on defense stat and apply them to this character
  // Called by the opponent in order to effectively apply damages to its target
  // Return effective damage
  def take_damage(from: Character, dam: Int): (Int, Boolean) = {
    val effectiveDamage = max(0, dam - getDef())
    currentHP -= effectiveDamage
    writeLog(name + "receives " + effectiveDamage.toString + "damages from" + from.name)
    if (currentHP <= 0) {
      die()
      return (effectiveDamage, true) // The second value indicates if the attack killed the character or not
    }
    return (effectiveDamage, false)
  }

  def die(): Unit = {
    writeLog(name + " dies. Goodbye cruel world !")
    board.removeCharacter(pos)
  }

  def getDef(): Int = baseDef
  def getAtt(): Int = baseAtt
  def getMaxHP(): Int = baseMaxHP
}

trait AIControlled extends Character {
  var active = false
  def activate(): Unit = {
    active = true
  }
  def desactivate(): Unit = {
    active = false
  }
  def act(): Unit = ()
}

trait Enemy extends Character with AIControlled {
  def action(c: Character): Unit = {
    if (c.isInstanceOf[Player]) {
      attack(c)
    }
  }
}

trait MeleeEnemy extends Character with Enemy {

  def nextCell(): Option[(Int, Int)] = {
    val sPath = board.shortestPath(pos, board.playerEntity.pos)
    sPath match {
      case Some(path) => Some(path(1))
      case None       => None
    }
  }

  override def act(): Unit = {
    nextCell() match {
      case None       => ()
      case Some(cell) => move(cell)
    }
  }
}

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
