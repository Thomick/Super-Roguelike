package GameEntities

import java.awt.{Color => AWTColor}
import map_objects._

class Player(init_pos: (Int, Int), b: GameBoard)
    extends Character(init_pos, b)
    with Humanoid {
  val name = "Player"
  val description = "It's you !"
  val color = new AWTColor(100, 255, 100)
  override val image = "src/main/resources/hero.png"
}
