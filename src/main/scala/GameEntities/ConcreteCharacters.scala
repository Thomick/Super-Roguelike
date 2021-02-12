package GameEntities

import map_objects._

class Player(init_pos: (Int, Int), b: GameBoard)
    extends Character(init_pos, b)
    with Humanoid {
  val name = "Player"
  val description = "It's you !"
  override val image = "src/main/resources/hero.png"
  override def action(c : Character): Unit = {
    if (c.isInstanceOf[Enemy]) {
      attack(c)
    }
  }
}
