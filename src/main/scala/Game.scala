package game

import java.io._

import map_objects._
import logger._
import input_handling._

@SerialVersionUID(7L)
class Game(val logger: Logger) extends Serializable {
  var levels = Vector[GameBoard]()
  var currentLocation = 0
  newLevel(false)

  def currentLevel(): GameBoard = {
    return levels(currentLocation)
  }

  def goUp(): Unit = {
    currentLevel.saveLastPosition
    levels(currentLocation - 1).updatePlayer(currentLevel.playerEntity)
    currentLocation -= 1
  }

  def goDown(): Unit = {
    if (currentLocation == levels.size - 1) { // The player is on the last generated level
      newLevel(currentLevel.activateElevator)
    }
    currentLevel.saveLastPosition
    levels(currentLocation + 1).updatePlayer(currentLevel.playerEntity)
    currentLocation += 1
  }

  def newLevel(elevatorOnStartingPostition : Boolean): Unit = {
    val board = new GameBoard(130, 130, logger)
    board.newMap(20, 20, board.size_x, board.size_y, 1, elevatorOnStartingPostition)
    levels = levels :+ board
  }
}
