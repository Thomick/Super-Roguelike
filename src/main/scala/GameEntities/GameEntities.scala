package GameEntities

import map_objects._

object Direction extends Enumeration {
  val Up, Down, Left, Right, Nop = Value
  def nextPos(pos: (Int, Int), dir: Direction.Value): (Int, Int) = {
    dir match {
      case Direction.Left  => (pos._1 - 1, pos._2)
      case Direction.Right => (pos._1 + 1, pos._2)
      case Direction.Up    => (pos._1, pos._2 - 1)
      case Direction.Down  => (pos._1, pos._2 + 1)
      case Direction.Nop   => pos
    }
  }
}

abstract class GameEntity(init_pos: (Int, Int), b: GameBoard) {
  val name: String
  val description: String
  var pos: (Int, Int) = init_pos
  val board: GameBoard = b
  val image: String = "src/main/resources/placeholder.png"
}

class Rock(init_pos: (Int, Int), b: GameBoard) extends GameEntity(init_pos, b) {
  val name = "A Rock"
  val description = "A Big Rock"
}
