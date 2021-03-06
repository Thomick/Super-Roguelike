package game_entities

import map_objects._
import input_handling._
import items._

trait InteractWithItems extends GameEntity {
  override def interact(c: Character): Boolean = {
    if (c.isInstanceOf[Player]) {
      UI.getSelectedItem(c.asInstanceOf[Player]) match {
        case Some(item) => {
          val consumed = itemInteraction(item, c.asInstanceOf[Player])
          if (consumed) {
            //Remove the consumed item from inventory
            c.asInstanceOf[Player].inventory.remove(UI.getSelectedItemIndex(c.asInstanceOf[Player]))
            return true
          }
        }
        case None => ()
      }
    }
    return false
  }

  def itemInteraction(item: AbstractItem, player: Player): Boolean = false
}

// Adds an interaction with the item "Hacking tools"
trait Hackable extends GameEntity with InteractWithItems {
  override def itemInteraction(item: AbstractItem, player: Player): Boolean = {
    if (item.isInstanceOf[HackingTools]) {
      hack(player)
      return true
    }
    return false
  }

  def hack(c: Player): Unit
}

// An hackable entity that allows to reveal the map of the current level
class Computer(init_pos: (Int, Int), b: GameBoard) extends GameEntity(init_pos, b) with Hackable {
  val name = "Computer"
  val description = "A computer. It seems connected to the internal network."
  image = "src/main/resources/entity_sprites/computer.png"

  // Custom message if the interaction fails
  override def interact(c: Character): Boolean = {
    if (!super[Hackable].interact(c)) {
      c.writeLog("\"Access denied !\" : You should look for something that will help you access this computer.")
      return false
    }
    return true
  }

  // Reveals the floor tiles of the map
  def hack(p: Player) = {
    board.grid.foreach(a => a.foreach(tile => if (!tile.blocking) tile.explored = true))
    p.writeLog("You access the computer and find a map of this floor.")
  }
}

class Lock(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with Hackable {
  val name = "Lock"
  val description = "A lock. It prevents you from using the elevator."
  image = "src/main/resources/entity_sprites/lock.png"

  override def interact(c: Character): Boolean = {
    if (!super[Hackable].interact(c)) {
      c.writeLog("\"Access denied !\" : You should look for something that will help you unlock this lock.")
      return false
    }
    return true
  }

  def action(c: GameEntity) = ()

  def hack(p: Player) = {
    b.activateElevator = true
    die
    p.writeLog("You unlock safely the lock.")
  }
}

class Lever(init_pos: (Int, Int), b: GameBoard) extends GameEntity(init_pos, b) with map_objects.Activable {
  val name = "Lever"
  val description = "It should activate something."
  image = "src/main/resources/entity_sprites/leveroff.png"

  override def interact(c: Character): Boolean = {
    activated = true
    image = "src/main/resources/entity_sprites/leveron.png"
    true
  }
}
