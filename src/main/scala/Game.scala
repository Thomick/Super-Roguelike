package game

import map_objects._
import logger._

@SerialVersionUID(7L)
class Game(logger: Logger) extends Serializable {
  var levels = Vector[GameBoard]()
  var actualLocation = 0
  newGame

  def currentLevel() : GameBoard = {
    return levels(actualLocation)
  }

  def newGame() : Unit = {
    val board = new GameBoard(30,30,logger)
    board.newMap(50,5,7,board.size_x,board.size_y)
    levels = levels :+ board
  }
}



