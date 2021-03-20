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
  def round (x : Double) : Int = {
    if (Math.abs(x-x.toInt) >= 0.5) {
      if (x<0) {
        return x.toInt -1
      }
      else {
        return x.toInt +1
      }
    } 
    else {
      return x.toInt
    }
  }

  def highlightedCells : (Vector[(Int,Int)],Boolean) = {
    var hCells = Vector[(Int,Int)]()
    val dx : Double = xpos - board.playerEntity.pos._1 
    val dy : Double = ypos - board.playerEntity.pos._2
    var steps : Double = max(Math.abs(dx),Math.abs(dy))
    val xIncrement : Double = dx / steps
    val yIncrement : Double = dy / steps
    var x : Double = board.playerEntity.pos._1
    var y : Double = board.playerEntity.pos._2
    var finish = false
    var accessible = true
    var i = 0
    while (!finish) {
      if (i>= steps){
        finish = true
        accessible = true
      }
      else {
        x += xIncrement
        y += yIncrement
        if ( 
          !board.grid(round(x))(round(y)).explored 
          || !board.isFree(round(x),round(y))
          || (board.distance(board.playerEntity.pos, (round(x),round(y))) > highlightLength) 
        ) {
          finish = true
        } else {
          hCells = hCells :+ (round(x),round(y))
          i+=1
        }
      }
    }
    return (hCells,accessible)
  }


}
