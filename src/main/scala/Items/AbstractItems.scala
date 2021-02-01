package Items

import map_objects._
import GameEntities._

abstract class AbstractItem() {
  val name: String
  val description: String
  val weight: Int
  val availableActions: List[String]
}

trait Consumable {
  def consume: Boolean
}

trait Throwable {
  def throwItem(
      board: GameBoard,
      startingPos: (Int, Int),
      dir: Direction.Value
  ): Boolean
}
