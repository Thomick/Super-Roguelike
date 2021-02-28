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

trait Consumable extends AbstractItem {
  availableActions += "C - Consume"

  // Apply consumption effect to the character and return action description (for the logs)
  def consume(character: Character): String = "Nothing happened."
}

trait Throwable extends AbstractItem {
  availableActions += "T - Throw"

  // Throw an object from startingPos in the direction dir
  // For now it only drop the item on the floor next to starting position
  // Mechanic might change in the future
  def throwItem(
      character: Character,
      board: GameBoard,
      startingPos: (Int, Int),
      dir: Direction.Value
  ): Boolean = {
    val throwPos = Direction.nextPos(startingPos, dir)
    board.addItem(new ItemEntity(throwPos, board, this), throwPos)
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
