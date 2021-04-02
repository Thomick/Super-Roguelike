package items

import scala.math.{pow, sqrt}
import map_objects._
import game_entities._
import scala.collection._

// Base trait for throwable items
trait Throwable extends AbstractItem {
  val consumedWhenThrown: Boolean
  val hasAoE: Boolean = false
  val effectRadius: Int = 0
  availableActions += "T - Throw"

  // Defines the effect of the item on the board at position [pos]
  def effectWhenThrown(board: GameBoard, pos: (Int, Int)): String

  // Called when the item is thrown
  // Returns a string describing the effect (for logging)
  def throwItem(board: GameBoard, throwPos: (Int, Int)): String = {
    val effect: String = effectWhenThrown(board, throwPos)
    if (!consumedWhenThrown) {
      board.addItem(new ItemEntity(throwPos, board, this), throwPos)
    }
    return effect
  }
}

trait ThrowableWithAoE extends Throwable {
  override val hasAoE: Boolean = true
  override val effectRadius: Int = 2

  // Apply the item effect on a circular area of radius [effectRadius]
  override def throwItem(board: GameBoard, throwPos: (Int, Int)): String = {
    val targets = new mutable.ArrayBuffer[String]
    for {
      i <- throwPos._1 - effectRadius to throwPos._1 + effectRadius;
      j <- throwPos._2 - effectRadius to throwPos._2 + effectRadius
    } {
      if (sqrt(pow(throwPos._1 - i, 2) + pow(throwPos._2 - j, 2)) <= effectRadius) {
        // effectWhenThrown must only return the names of the objects affected
        val effect = effectWhenThrown(board, (i, j))
        if (effect != "")
          targets += effect
      }
    }
    if (!consumedWhenThrown) {
      board.addItem(new ItemEntity(throwPos, board, this), throwPos)
    }
    // Assembles the names of the objects affected to describe the effect
    if (targets.length == 0)
      return ""
    if (targets.length == 1)
      return targets(0) + " was hit by " + name

    val effect = new StringBuilder
    effect ++= targets(0)
    for (i <- 1 to targets.length - 2) {
      effect ++= ", " + targets(i)
    }
    effect ++= " and " + targets(targets.size - 1)
    return effect.toString + " were hit by " + name
  }
}

// Deals damage in an area around the position where it is thrown
class Grenade extends ThrowableWithAoE {
  var name: String = "Grenade"
  val description: String = "Kaboom !"
  val weight: Int = 300
  override val image: String = "src/main/resources/handgrenade.png"
  val consumedWhenThrown: Boolean = true
  def effectWhenThrown(board: GameBoard, pos: (Int, Int)): String = {
    if (board.hasCharacter(pos)) {
      val c = board.getCharacter(pos)
      c.takeDamage(new ItemEntity(pos, board, this), 10)
      return c.name
    }
    return ""
  }
}

// Stun grenade (Electro magnetic pulse grenade)
class EMPGrenade extends ThrowableWithAoE {
  var name = "EMP Grenade"
  val description = "This grenade is able to release electro magnetic pulses and stun enemies"
  val weight: Int = 300
  override val image: String = "src/main/resources/empgrenade.png"
  val consumedWhenThrown: Boolean = true
  def effectWhenThrown(board: GameBoard, pos: (Int, Int)): String = {
    if (board.hasCharacter(pos)) {
      val c = board.getCharacter(pos)
      c.statusList += new StunnedStatus(10)
      c.writeLog("You got stunned by " + name)
      return c.name
    }
    return ""
  }
}
