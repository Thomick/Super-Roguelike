package items

import map_objects._
import game_entities._
import scala.collection.mutable.ArrayBuffer

@SerialVersionUID(105L)
abstract class AbstractItem() extends Serializable {
  var name: String
  val description: String
  val weight: Int
  val image: String = "src/main/resources/placeholder.png"
  val availableActions: ArrayBuffer[String] = new ArrayBuffer[String]
  availableActions += "D - Drop"
}

trait Throwable extends AbstractItem {
  val consumedWhenThrown: Boolean
  availableActions += "T - Throw"

  def effectWhenThrown(board: GameBoard, pos: (Int, Int)): Unit
  def throwItem(
      board: GameBoard,
      throwPos: (Int, Int)
  ): Unit = {
    effectWhenThrown(board, throwPos)
    if (!consumedWhenThrown) {
      board.addItem(new ItemEntity(throwPos, board, this), throwPos)
    }
  }
}

// Enumeration of bodyparts used for equipment restrictions
object BodyPart extends Enumeration {
  val Head, Arm, Legs, Torso, Feet, Hand, Other = Value
}
