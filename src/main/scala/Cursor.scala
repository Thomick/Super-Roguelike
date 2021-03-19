package cursor

import game_entities._
import map_objects._

class Cursor (board : GameBoard) {
  val image = "src/main/resources/cursor.png"
  var xpos = 0
  var ypos = 0
  var visible = false
  var highlightPath = false
  def pos : (Int,Int) = (xpos,ypos)
  def backToPlayer : Unit = {
    xpos = board.playerEntity.pos._1
    ypos = board.playerEntity.pos._2
  }
  def makeVisible : Unit = {
    visible = true
  }
  def makeInvisible : Unit = {
    visible = false
  }
  def move (dir : Direction.Value): Unit = {
    val newpos = Direction.nextPos((xpos,ypos),dir)
    if (board.inGrid(newpos)) {
      xpos = newpos._1
      ypos = newpos._2
    }
  }
}
