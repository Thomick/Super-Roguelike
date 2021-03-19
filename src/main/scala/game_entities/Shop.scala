package game_entities

import map_objects._

class Shopkeeper(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with AIControlled {
  val name = "Shopkeeper"
  val description = "He sells low quality goods at a high price"

  def action(c: Character) = ()
}
