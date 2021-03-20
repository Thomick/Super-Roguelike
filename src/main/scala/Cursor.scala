package cursor

import game_entities._
import map_objects._
import scala.math.{min, max}

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
  def highlightedCells : Vector[(Int,Int)] = {
    var hCells = Vector[(Int,Int)]()
    val dx : Double = board.playerEntity.pos._1 - xpos
    val dy : Double = board.playerEntity.pos._2 - ypos
    var steps : Double = max(Math.abs(dx),Math.abs(dy))
    val xIncrement = dx / steps
    val yIncrement = dy / steps
    var x : Double = board.playerEntity.pos._1
    var y : Double = board.playerEntity.pos._2
    var finish = false
    while (!finish) {
      x += xIncrement
      y += yIncrement
      if ( 
        !board.grid(x.toInt)(y.toInt).explored 
        || board.grid(x.toInt)(y.toInt).blocking_sight 
        || (board.distance(board.playerEntity.pos, (x.toInt,y.toInt)) > highlightLength) 
      ) {
        finish = true
      } else {
        hCells = hCells :+ (x.toInt,y.toInt)
      }
    }
    return hCells
  }


}
