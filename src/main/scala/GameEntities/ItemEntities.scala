package GameEntities

import Items._
import map_objects._
import java.awt.Color

class ItemEntity(init_pos: (Int, Int), b: GameBoard, item: AbstractItem)
    extends GameEntity(init_pos, b) {
  val attachedItem = item
  val name = item.name
  val description = item.description
  val color = new Color(200, 120, 200)
  override val image = item.image
}
