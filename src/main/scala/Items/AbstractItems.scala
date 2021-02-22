package Items

import map_objects._
import GameEntities._
import scala.collection.mutable.ArrayBuffer

abstract class AbstractItem() {
  val name: String
  val description: String
  val weight: Int
  val image: String = "src/main/resources/placeholder.png"
  val availableActions: ArrayBuffer[String] = new ArrayBuffer[String]
  availableActions += "D - Drop" //+= "U - Use (not implemented)"

  def drop(character: Character, board: GameBoard, pos: (Int, Int)): Boolean = {
    if (board.addItem(new ItemEntity(pos, board, this), pos)) {
      println("Item dropped")
      return true
    } else {
      println("This item can't be dropped here")
      return false
    }
  }

  def use(character: Character, board: GameBoard, pos: (Int, Int)): Boolean = {
    println("It does nothing")
    true
  }
}

trait Consumable extends AbstractItem {
  availableActions += "C - Consume"
  def consume(character: Character): String = "Nothing happened."
}

trait Throwable extends AbstractItem {
  availableActions += "T - Throw"

  def throwItem(
      character: Character,
      board: GameBoard,
      startingPos: (Int, Int),
      dir: Direction.Value
  ): Boolean = {
    val throwPos = Direction.nextPos(startingPos, dir)
    this.drop(character, board, throwPos)
  }
}

object BodyPart extends Enumeration {
  val Head, Arm, Legs, Torso, Feet, Hand, Other = Value
}

trait Equipable extends AbstractItem {
  availableActions += "R - Equip/Unequip"
  val part: BodyPart.Value
}

trait Passive extends AbstractItem {
  val bonusAtt: Int
  val bonusDef: Int
  val bonusHP: Int
}
