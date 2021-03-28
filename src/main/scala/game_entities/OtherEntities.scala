package game_entities

import map_objects._
import input_handling._
import items._

trait Hackable extends GameEntity {
  override def interact(c: Character): Boolean = {
    if (c.isInstanceOf[Player]) {
      println("1")
      UI.getSelectedItem(c.asInstanceOf[Player]) match {
        case Some(item) => {
          println("2")
          if (item.isInstanceOf[HackingTools]) {
            println("3")
            hack(c.asInstanceOf[Player])
            return true
          }
        }
        case None => ()
      }
    }
    return false
  }

  def hack(c: Player): Unit
}

class Computer(init_pos: (Int, Int), b: GameBoard) extends GameEntity(init_pos, b) with Hackable {
  val name = "Computer"
  val description = "A computer. It seems connected to the internal network."
  override val image: String = "src/main/resources/computer.png"

  override def interact(c: Character): Boolean = {
    if (!super[Hackable].interact(c)) {
      c.writeLog("\"Access denied !\" : You should look for something that will help you access this computer.")
      return false
    }
    return true
  }

  def hack(p: Player) = {
    board.grid.foreach(a => a.foreach(tile => if (!tile.blocking) tile.explored = true))
    p.writeLog("You access the computer and find a map of this floor.")
  }
}
