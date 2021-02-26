package GameEntities

import map_objects._

// Base class for game entities
// hasLog specifies if this entity can write to the gameboard logger
abstract class GameEntity(init_pos: (Int, Int), b: GameBoard, val hasLogs: Boolean = false) {
  val name: String
  val description: String
  var pos: (Int, Int) = init_pos
  val board: GameBoard = b
  val image: String = "src/main/resources/placeholder.png"

  // Write the input message to the gameboard logger
  def writeLog(message: String): Unit = {
    if (hasLogs)
      board.logger.writeLog(message)
  }
}
