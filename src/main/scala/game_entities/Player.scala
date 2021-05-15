package game_entities

import map_objects._
import items._
import scala.math.min

// Character controlled by the player
@SerialVersionUID(100L)
class Player(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b, true) with Humanoid with Serializable {
  val name = "Player"
  val description = "It's you !"
  baseMaxHP = 100
  currentHP = 100
  image = "src/main/resources/entity_sprites/robot.png"
  inventory += new ArmCannon
  var alive: Boolean = true

  var money: Int = 0
  var keyCount: Int = 3

  // Player action when encountering another character
  override def action(c: GameEntity): Unit = {
    if (c.isInstanceOf[Enemy]) {
      attack(c.asInstanceOf[Enemy])
    } else
      c.interact(this)
  }

  override def die(): Unit = {
    if (alive) {
      writeLog(
        "### Your body can not endure damage anymore. However your robot parts allow you to keep on exploring and fighting. ### (and test the game)"
      )
      alive = false
    }

  }

  // Automatically add money picked up to the money stat of the player
  override def pickUpItem(): Option[AbstractItem] = {
    val it = super.pickUpItem()
    it match {
      case Some(item) =>
        if (item.isInstanceOf[Money]) {
          money += item.asInstanceOf[Money].value
          inventory -= item
        } else if (item.isInstanceOf[Key]) {
          keyCount += 1
          inventory -= item
        }
      case None => ()
    }
    return it
  }

}
