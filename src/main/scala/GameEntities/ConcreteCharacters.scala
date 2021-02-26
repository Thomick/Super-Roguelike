package GameEntities

import map_objects._

// Character controlled by the player
class Player(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b, true) with Humanoid {
  val name = "Player"
  val description = "It's you !"
  override val baseAtt: Int = 1
  override val image = "src/main/resources/hero.png"

  // Player action when encountering another character
  override def action(c: Character): Unit = {
    if (c.isInstanceOf[Enemy]) {
      attack(c)
    }
  }
}

class Robot(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with MeleeEnemy {
  val name = "Robot"
  val description = "An angry robot"
  override val image = "src/main/resources/robot2.png"
}

class Dog(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with MeleeEnemy {
  val name = "Dog"
  val description = "An angry robot dog"
  override val image = "src/main/resources/doggo.png"
}
