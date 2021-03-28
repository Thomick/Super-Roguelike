package game_entities

import map_objects._
import scala.collection._
import rendering.Menu
import input_handling._
import items._
import javax.swing.text.html.parser.Entity

class Shopkeeper(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with AIControlled {
  val name = "Shopkeeper"
  val description = "He sells low quality goods at a high price"
  override val image: String = "src/main/resources/vendingmachine.png"
  val forSale = new mutable.ArrayBuffer[(AbstractItem, Int)]
  var lastPlayerMet: Option[Player] = None
  val menu = new Menu {
    override val name = "Shop"
    override def confirm(): Unit = {
      sell(cursorIndex)
    }
  }
  forSale += ((new LaserChainsaw, 10), (new IronHelmet, 1), (new Morphin, 3), (new Bandage, 5))
  forSale += ((new HackingTools, 5))
  updateMenu()

  def sell(itemIndex: Int): Unit = {
    lastPlayerMet match {
      case Some(p) => {
        if (forSale.length > itemIndex && itemIndex >= 0)
          if (p.money >= forSale(itemIndex)._2) {
            p.money -= forSale(itemIndex)._2
            p.obtainItem(forSale(itemIndex)._1)
            forSale.remove(itemIndex)
          } else {
            p.writeLog("You don't have enough money to buy " + forSale(itemIndex)._1.name + ".")
          }
        updateMenu()
      }
      case None => ()
    }

  }

  def updateMenu(): Unit = {
    menu.items.clear()
    forSale.foreach(a => menu.items += ((a._1.name + " (" + a._2.toString() + "U)", "")))
  }

  def action(c: GameEntity) = ()

  override def interact(c: Character): Boolean = {
    if (c.isInstanceOf[Player]) {
      UI.menuStack.push(menu)
      lastPlayerMet = Some(c.asInstanceOf[Player])
      return true
    }
    return false
  }
}
