package items

import game_entities._
import scala.collection._
import map_objects._
import input_handling._

// An item to represent money (used to put money on the board)
// Can represent any amount of money
class Money(val value: Int) extends AbstractItem {
  var name = "Money"
  val description = "You are rich !"
  val weight = value * 5
  override val image: String = "src/main/resources/money.png"
}

// Item used to interact with Hackable entities
class HackingTools extends AbstractItem {
  var name = "Hacking Tools"
  val description = "Can be used to hack things"
  val weight = 100
}

// Item used to upgrade Upgradable pieces of equipment in the inventory
// Consumed when used
class ElectronicComponents extends AbstractItem with Consumable {
  var name = "Electronic components"
  val description = "Can be used to upgrade some equipment"
  val weight = 100
  override val image: String = "src/main/resources/circuitboard.png"

  val consumptionMessage = "You can choose an item to upgrade"

  // Open a menu that allows to upgrade an item
  override def consume(character: Character): String = {
    if (!character.isInstanceOf[Player])
      return "Only players can upgrade items"
    val player = character.asInstanceOf[Player]
    val upgradableItems = new mutable.ArrayBuffer[Upgradable]
    //Gathers all upgradable items from player inventory and equiped inventory
    player.equipedItems.foreach(item =>
      if (item.isInstanceOf[Upgradable])
        upgradableItems += item.asInstanceOf[Upgradable]
    )
    player.inventory.foreach(item =>
      if (item.isInstanceOf[Upgradable])
        upgradableItems += item.asInstanceOf[Upgradable]
    )
    // Menu used to show upgradable items in inventory
    val menu = new Menu {
      override val name = "Upgrade"
      if (upgradableItems.isEmpty)
        items += (("You do not have any upgradable item", ""))
      else
        upgradableItems.foreach(item => items += ((item.name, "")))

      override def confirm(): Unit = {
        var used = false
        if (cursorIndex < upgradableItems.size) {
          player.writeLog("You upgrade " + upgradableItems(cursorIndex).name)
          upgradableItems(cursorIndex).upgrade
          used = true
        }
        if (UI.menuStack.top == this)
          UI.menuStack.pop
        if (!used) {
          player.inventory += new ElectronicComponents
          println("remboursement")
        }
      }
    }
    UI.menuStack.push(menu)
    super.consume(character)
  }
}
