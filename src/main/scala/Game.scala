package game

import java.io._

import map_objects._
import logger._
import input_handling._

@SerialVersionUID(7L)
class Game(val logger: Logger) extends Serializable {
  var levels = Vector[GameBoard]()
  var currentLocation = 0
  newLevel(false,1)

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
      newLevel(currentLevel.activateElevator, levels.size)
    }
    currentLevel.saveLastPosition
    levels(currentLocation + 1).updatePlayer(currentLevel.playerEntity)
    currentLocation += 1
  }

  def newLevel(elevatorOnStartingPostition : Boolean, depth : Int): Unit = {
    val board = new GameBoard(80, 80, logger)
    board.newMap(20, board.size_x, board.size_y, depth, elevatorOnStartingPostition)
    levels = levels :+ board
  }
}
