package items

import game_entities._
import scala.collection._
import map_objects._
import input_handling._

class Money(val value: Int) extends AbstractItem {
  var name = "Money"
  val description = "You are rich !"
  val weight = value * 5
  override val image: String = "src/main/resources/money.png"
}

class HackingTools extends AbstractItem {
  var name = "Hacking Tools"
  val description = "Can be used to hack things"
  val weight = 100
}

class ElectronicComponents extends AbstractItem with Consumable {
  var name = "Electronic components"
  val description = "Can be used to upgrade some equipment"
  val weight = 100
  override val image: String = "src/main/resources/circuitboard.png"

  val consumptionMessage = "You can choose an item to upgrade"

  override def consume(character: Character): String = {
    if (!character.isInstanceOf[Player])
      return "Only players can upgrade items"
    val player = character.asInstanceOf[Player]
    val upgradableItems = new mutable.ArrayBuffer[Upgradable]
    player.equipedItems.foreach(item =>
      if (item.isInstanceOf[Upgradable])
        upgradableItems += item.asInstanceOf[Upgradable]
    )
    player.inventory.foreach(item =>
      if (item.isInstanceOf[Upgradable])
        upgradableItems += item.asInstanceOf[Upgradable]
    )
    val menu = new Menu {
      override val name = "Upgrade"
      if (upgradableItems.isEmpty)
        items += (("You do not have any upgradable item", ""))
      else
        upgradableItems.foreach(item => items += ((item.name, "")))

      override def confirm(): Unit = {
        if (cursorIndex < upgradableItems.size) {
          player.writeLog("You upgrade " + upgradableItems(cursorIndex).name)
          upgradableItems(cursorIndex).upgrade
        }
        if (UI.menuStack.top == this)
          UI.menuStack.pop
      }
    }
    UI.menuStack.push(menu)
    super.consume(character)
  }
}
