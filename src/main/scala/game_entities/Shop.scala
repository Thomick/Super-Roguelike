package game_entities

import map_objects._
import scala.collection._
import rendering.Menu
import input_handling._
import items.AbstractItem

class Shopkeeper(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with AIControlled {
  val name = "Shopkeeper"
  val description = "He sells low quality goods at a high price"
  val forSale = new mutable.ArrayBuffer[(AbstractItem, Int)]
  val menu = new Menu {
    override val name = "Shop"
  }

  def action(c: Character) = ()

  override def interact(c: Character): Unit =
    if (c.isInstanceOf[Player])
      UI.menuStack.push(menu)
}
