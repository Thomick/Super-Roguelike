package items

import map_objects._
import game_entities._
import scala.collection.mutable.ArrayBuffer

abstract class AbstractItem() {
  val name: String
  val description: String
  val weight: Int
  val image: String = "src/main/resources/placeholder.png"
  val availableActions: ArrayBuffer[String] = new ArrayBuffer[String]
  availableActions += "D - Drop" //+= "U - Use (not implemented)"

  // Activate the special ability of an item
  def use(character: Character, board: GameBoard, pos: (Int, Int)): Boolean = {
    println("It does nothing")
    true
  }
}

trait Throwable extends AbstractItem {
  val consumedWhenThrown : Boolean
  availableActions += "T - Throw"

  def effectWhenThrown(board: GameBoard, pos: (Int, Int)) : Unit
  def throwItem(
      board: GameBoard,
      throwPos: (Int, Int),
  ): Unit = {
    effectWhenThrown(board,throwPos)
    if (!consumedWhenThrown) {
      board.addItem(new ItemEntity(throwPos, board, this), throwPos)
    }
  }
}

// Enumeration of bodyparts used for equipment restrictions
object BodyPart extends Enumeration {
  val Head, Arm, Legs, Torso, Feet, Hand, Other = Value
}

trait Equipable extends AbstractItem {
  availableActions += "R - Equip/Unequip"
  // Bodypart on which the item is equiped
  val part: BodyPart.Value

  val bonusAtt: Int
  val bonusDef: Int
  val bonusHP: Int
}
