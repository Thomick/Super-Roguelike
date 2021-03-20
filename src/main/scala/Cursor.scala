package cursor

import game_entities._
import map_objects._

class Cursor (board : GameBoard) {
  val image = "src/main/resources/cursor.png"
  val highlightImage = "src/main/resources/highlight.png"
  var xpos = 0
  var ypos = 0
  var visible = false
  var highlightPath = false
  var highlightLength = 0
  var areaOfEffect = false
  var rangeOfEffect = 0
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
  def activateAOE (range : Int) : Unit = {
    areaOfEffect = true
    rangeOfEffect = range
  }
  def deactivateAOE : Unit = {
    areaOfEffect = false
  }
  def activateHighlight (length : Int ) : Unit = {
    highlightPath = true
    highlightLength = length
  }
  def deactivateHighlight : Unit = {
    highlightPath = false
  }
  def move (dir : Direction.Value): Unit = {
    val newpos = Direction.nextPos((xpos,ypos),dir)
    if (board.inGrid(newpos)) {
      xpos = newpos._1
      ypos = newpos._2
    }
  }
  def highlightedCells : ... = {

}
