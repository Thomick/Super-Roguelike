package GameEntities

import map_objects._

class Player(init_pos: (Int, Int), b: GameBoard)
    extends Character(init_pos, b, true)
    with Humanoid {
  val name = "Player"
  val description = "It's you !"
  override val baseAtt: Int = 1
  def moveDir(dir: Direction.Value): Unit = {
    move(Direction.nextPos(pos, dir))
  }
  override val image = "src/main/resources/hero.png"
  override def action(c: Character): Unit = {
    if (c.isInstanceOf[Enemy]) {
      attack(c)
    }
  }
}

class Robot(init_pos: (Int, Int), b: GameBoard)
    extends Character(init_pos, b)
    with Humanoid
    with MeleeEnemy {
  val name = "Robot"
  val description = "An angry robot"
  override val image = "src/main/resources/robot2.png"
}
