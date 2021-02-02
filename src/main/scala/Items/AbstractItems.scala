package Items

import map_objects._
import GameEntities._

abstract class AbstractItem() {
  val name: String
  val description: String
  val weight: Int
  val availableActions: List[String]

  def drop(character: Character, board: GameBoard, pos: (Int, Int)): Boolean = {
    if (board.addItem(new ItemEntity(pos, board, this), pos)) {
      character.destroyItem(this)
      println("Item dropped")
      return true
    } else {
      println("This item can't be dropped here")
      return false
    }
  }
}

trait Consumable {
  this: AbstractItem =>
  def consume(character: Character): Boolean = {
    character.destroyItem(this)
    println("Item consumed")
    true
  }
}

trait Throwable {
  this: AbstractItem =>
  def throwItem(
      character: Character,
      board: GameBoard,
      startingPos: (Int, Int),
      dir: Direction.Value
  ): Boolean = {
    val throwPos = Direction.nextPos(startingPos, dir)
    if (board.isFree(throwPos)) {
      board.entityMoved(new ItemEntity(throwPos, board, this), throwPos)
      character.destroyItem(this)
      println("Item thrown")
      return true
    } else {
      println("This item can't be thrown here")
      return false
    }
  }
}
