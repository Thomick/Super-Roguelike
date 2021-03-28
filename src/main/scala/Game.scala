package game

import java.io._

import map_objects._
import logger._
import input_handling._

@SerialVersionUID(7L)
class Game(val logger: Logger) extends Serializable {
  var levels = Vector[GameBoard]()
  var actualLocation = 0
  newLevel()

  def currentLevel(): GameBoard = {
    return levels(actualLocation)
  }

  def newLevel(): Unit = {
    val board = new GameBoard(30, 30, logger)
    board.newMap(50, 5, 7, board.size_x, board.size_y)
    levels = levels :+ board
  }
}
