package GameEntities

import map_objects._

abstract class GameEntity(
    init_pos: (Int, Int),
    b: GameBoard,
    val hasLogs: Boolean = false
) {
  val name: String
  val description: String
  var pos: (Int, Int) = init_pos
  val board: GameBoard = b
  val image: String = "src/main/resources/placeholder.png"

  // Write the input message to the logger of the gameboard
  def writeLog(message: String): Unit = {
    if (hasLogs)
      board.logger.writeLog(message)
  }
}
