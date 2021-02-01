package GameEntities

import map_objects._
import java.awt.{Color => AWTColor}

object Direction extends Enumeration {
  val Up, Down, Left, Right, Nop = Value
}

abstract class GameEntity(init_pos: (Int, Int), b: GameBoard) {
  val name: String
  val description: String
  var pos: (Int, Int) = init_pos
  val color: AWTColor
  val board: GameBoard = b
  val image: String = "src/main/resources/placeholder.png"
}

abstract class Character(init_pos: (Int, Int), b: GameBoard)
    extends GameEntity(init_pos, b) {

  def move(dir: Direction.Value): Unit = {
    var nextPos = pos
    dir match {
      case Direction.Left  => nextPos = (pos._1 - 1, pos._2)
      case Direction.Right => nextPos = (pos._1 + 1, pos._2)
      case Direction.Up    => nextPos = (pos._1, pos._2 - 1)
      case Direction.Down  => nextPos = (pos._1, pos._2 + 1)
    }
    if (board.isFree(nextPos)) {
      board.entityMoved(this, nextPos)
      pos = nextPos
    }
  }
}

class Player(init_pos: (Int, Int), b: GameBoard)
    extends Character(init_pos, b) {
  val name = "Player"
  val description = "It's you !"
  val color = new AWTColor(100, 255, 100)
  override val image = "src/main/resources/hero.png"
}

class Rock(init_pos: (Int, Int), b: GameBoard) extends GameEntity(init_pos, b) {
  val name = "A Rock"
  val description = "A Big Rock"
  val color = new AWTColor(200, 200, 200)
}
