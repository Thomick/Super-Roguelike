package items

import scala.math.{pow, sqrt}
import map_objects._
import game_entities._

trait Throwable extends AbstractItem {
  val consumedWhenThrown: Boolean
  availableActions += "T - Throw"

  def effectWhenThrown(board: GameBoard, pos: (Int, Int)): Unit
  def throwItem(board: GameBoard, throwPos: (Int, Int)): Unit = {
    effectWhenThrown(board, throwPos)
    if (!consumedWhenThrown) {
      board.addItem(new ItemEntity(throwPos, board, this), throwPos)
    }
  }
}

trait ThrowableWithAoE extends Throwable {
  val effectRadius: Int

  override def throwItem(board: GameBoard, throwPos: (Int, Int)): Unit = {
    for {
      i <- throwPos._1 - effectRadius to throwPos._1 + effectRadius;
      j <- throwPos._2 - effectRadius to throwPos._2 + effectRadius
    } {
      if (sqrt(pow(throwPos._1 - i, 2) + pow(throwPos._2 - j, 2)) < effectRadius)
        effectWhenThrown(board, (i, j))
    }
    if (!consumedWhenThrown) {
      board.addItem(new ItemEntity(throwPos, board, this), throwPos)
    }
  }
}

class Grenade extends ThrowableWithAoE {
  var name: String = "Grenade"
  val description: String = "Kaboom !"
  val weight: Int = 300
  val effectRadius: Int = 3
  val consumedWhenThrown: Boolean = true
  def effectWhenThrown(board: GameBoard, pos: (Int, Int)): Unit = {
    if (board.hasCharacter(pos)) {
      board.getCharacter(pos).takeDamage(new ItemEntity(pos, board, this), 10)
    }
  }
}

class EMPGrenade extends ThrowableWithAoE {
  var name = "EMP Grenade"
  val description = "This grenade is able to release electro magnetic pulses and stun enemies"
  val weight: Int = 300
  val effectRadius: Int = 3
  val consumedWhenThrown: Boolean = true
  def effectWhenThrown(board: GameBoard, pos: (Int, Int)): Unit = {
    if (board.hasCharacter(pos)) {
      val c = board.getCharacter(pos)
      c.statusList += new StunnedStatus(10)
      c.writeLog("You got stunned by " + name)
    }
  }
}
