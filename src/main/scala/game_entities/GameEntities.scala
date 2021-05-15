package game_entities

import map_objects._

// Base class for game entities
// hasLog specifies if this entity can write to the gameboard logger
@SerialVersionUID(104L)
abstract class GameEntity(init_pos: (Int, Int), b: GameBoard, val hasLogs: Boolean = false) extends Serializable {
  val name: String
  val description: String
  var pos: (Int, Int) = init_pos
  var board: GameBoard = b
  val image: String = "src/main/resources/other_sprites/placeholder.png"

  // Write the input message to the gameboard logger
  def writeLog(message: String): Unit = {
    if (hasLogs && message != "")
      board.logger.writeLog(message)
  }

  def interact(c: Character): Boolean = false
}
