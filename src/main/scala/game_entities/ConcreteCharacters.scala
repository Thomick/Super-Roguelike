package game_entities

import map_objects._
import items._

// Character controlled by the player
class Player(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b, true) with Humanoid {
  val name = "Player"
  val description = "It's you !"
  override val baseMaxHP: Int = 100
  currentHP = 100
  override val image = "src/main/resources/hero.png"

  var money: Int = 0

  // Player action when encountering another character
  override def action(c: GameEntity): Unit = {
    if (c.isInstanceOf[Enemy]) {
      attack(c.asInstanceOf[Enemy])
    } else
      c.interact(this)
  }

  override def die(): Unit = {
    writeLog(
      "### Your body can not endure damage anymore. However your robot parts allow you to keep on exploring and fighting. ###"
    )
  }

  override def pickUpItem(): Option[AbstractItem] = {
    val it = super.pickUpItem()
    it match {
      case Some(item) =>
        if (item.isInstanceOf[Money]) {
          money += item.asInstanceOf[Money].value
          inventory -= item
        }
      case None => ()
    }
    return it
  }
}

class Robot(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with MeleeEnemy with Humanoid {
  val name = "Robot"
  val description = "An angry robot"
  override val image = "src/main/resources/robot2.png"
}

class Dog(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with MeleeEnemy {
  val name = "Dog"
  val description = "An angry robot dog"
  override val image = "src/main/resources/dog1.png"
}
