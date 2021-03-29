package game_entities

import map_objects._
import scala.collection._
import input_handling._
import items._

class ShopMenu(shop: Shopkeeper, player: Player) extends Menu {
  override val name = "Shop"
  override def confirm(): Unit = {
    shop.sell(cursorIndex, player)
    items.clear()
    shop.forSale.foreach(a => items += ((a._1.name + " (" + a._2.toString() + "U)", "")))
  }
}

@SerialVersionUID(150L)
class Shopkeeper(init_pos: (Int, Int), b: GameBoard)
    extends Character(init_pos, b)
    with AIControlled
    with Serializable {
  val name = "Vending machine"
  val description = "It sells low quality goods at a high price"
  override val image: String = "src/main/resources/vendingmachine.png"
  val forSale = new mutable.ArrayBuffer[(AbstractItem, Int)]
  var lastPlayerMet: Option[Player] = None
  forSale += ((new LaserChainsaw, 10), (new IronHelmet, 1), (new Morphin, 3), (new Bandage, 5))
  forSale += ((new HackingTools, 5))

  def sell(itemIndex: Int, player: Player): Unit = {
    if (forSale.length > itemIndex && itemIndex >= 0)
      if (player.money >= forSale(itemIndex)._2) {
        player.money -= forSale(itemIndex)._2
        player.obtainItem(forSale(itemIndex)._1)
        forSale.remove(itemIndex)
      } else {
        player.writeLog("You don't have enough money to buy " + forSale(itemIndex)._1.name + ".")
      }
  }

  def action(c: GameEntity) = ()

  override def interact(c: Character): Boolean = {
    if (c.isInstanceOf[Player]) {
      UI.menuStack.push(new ShopMenu(this, c.asInstanceOf[Player]))
      lastPlayerMet = Some(c.asInstanceOf[Player])
      return true
    }
    return false
  }
}
