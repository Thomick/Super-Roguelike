package cursor

import game_entities._
import map_objects._
import scala.math.{min, max}

@SerialVersionUID(101L)
class Cursor(board: GameBoard) extends Serializable {
  val image = "src/main/resources/other_sprites/cursor.png"
  val imageAoE = "src/main/resources/other_sprites/cursorbis.png"
  val highlightImage = "src/main/resources/other_sprites/highlight.png"
  var xpos = 0
  var ypos = 0
  var visible = false
  var highlightPath = false
  var highlightLength = 0
  var areaOfEffect = false
  var rangeOfEffect = 0
  def pos: (Int, Int) = (xpos, ypos)
  def backToPlayer: Unit = { // Move the cursor at the position of the player
    xpos = board.playerEntity.pos._1
    ypos = board.playerEntity.pos._2
  }
  def makeVisible: Unit = {
    visible = true
  }
  def makeInvisible: Unit = {
    visible = false
  }
  def activateAOE(range: Int): Unit = {
    areaOfEffect = true
    rangeOfEffect = range
  }
  def deactivateAOE: Unit = {
    areaOfEffect = false
  }
  def activateHighlight(length: Int): Unit = {
    highlightPath = true
    highlightLength = length
  }
  def deactivateHighlight: Unit = {
    highlightPath = false
  }
  def move(dir: Direction.Value): Unit = {
    val newpos = Direction.nextPos((xpos, ypos), dir)
    if (board.inGrid(newpos)) {
      xpos = newpos._1
      ypos = newpos._2
    }
  }
  def round(x: Double): Int = {
    if (Math.abs(x - x.toInt) >= 0.5) {
      if (x < 0) {
        return x.toInt - 1
      } else {
        return x.toInt + 1
      }
    } else {
      return x.toInt
    }
  }

  def highlightedCells
      : (Vector[(Int, Int)], Boolean) = { // Return the list of cells on the path from the player to the cursor when using a ranged weapon
    var hCells = Vector[(Int, Int)]()
    val dx: Double = xpos - board.playerEntity.pos._1
    val dy: Double = ypos - board.playerEntity.pos._2
    var steps: Double = max(Math.abs(dx), Math.abs(dy))
    val xIncrement: Double = dx / steps
    val yIncrement: Double = dy / steps
    var x: Double = 0 // We use relative coordinates to be able to round the values correctly
    var y: Double = 0
    var finish = false
    var accessible = false
    var i = 0
    while (!finish) {
      if (i >= steps) {
        finish = true
        accessible = true
      } else {
        x += xIncrement
        y += yIncrement
        if (
          !board.grid(board.playerEntity.pos._1 + round(x))(board.playerEntity.pos._2 + round(y)).explored
          || board.grid(board.playerEntity.pos._1 + round(x))(board.playerEntity.pos._2 + round(y)).blocking_sight
          || (board.distance((0, 0), (round(x), round(y))) > highlightLength)
        ) {
          finish = true // The cells is out of range or blocked
        } else {
          hCells = hCells :+ (board.playerEntity.pos._1 + round(x), board.playerEntity.pos._2 + round(y))
          i += 1
          if (!board.isFree(board.playerEntity.pos._1 + round(x), board.playerEntity.pos._2 + round(y))) { // When there is an enemy between the player and the cursor
            finish = true
            if (i >= steps) { // When the enemy is on the cursor
              accessible = true
            }
          }
        }
      }
    }
    return (hCells, accessible)
  }

}
