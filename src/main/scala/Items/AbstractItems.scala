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
  this: AbstractItem =>
  def consume(character: Character): Boolean = {
    character.destroyItem(this)
    true
  }
}

trait Throwable {
  this: AbstractItem =>
  def throwItem(
      board: GameBoard,
      startingPos: (Int, Int),
      dir: Direction.Value
  ): Boolean = {
    val throwPos = Direction.nextPos(startingPos, dir)
    if (board.isFree(throwPos)) {
      board.entityMoved(new ItemEntity(throwPos, board, this), throwPos)
      return true
    } else {
      return false
    }
  }
}
