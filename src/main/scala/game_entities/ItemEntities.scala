package game_entities

import items._
import map_objects._
import java.awt.Color

@SerialVersionUID(145L)
class ItemEntity(init_pos: (Int, Int), b: GameBoard, item: AbstractItem) extends GameEntity(init_pos, b) with Serializable {
  val associatedItem: AbstractItem = item
  val name = item.name
  val description = item.description
  image = item.image
}
