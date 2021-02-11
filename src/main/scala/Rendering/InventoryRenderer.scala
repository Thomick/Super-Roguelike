package Rendering

import GameEntities._
import InputHandling.UI
import java.awt.{Graphics2D, Color}
import Items.AbstractItem

object InventoryRenderer {
  val selectedColor = new Color(255, 200, 200)

  def drawInventory(
      g: Graphics2D,
      origin: (Int, Int),
      player: Player,
      ui: UI
  ): Unit = {
    val equiped = player.getEquipedItems()
    val inventory = player.getInventoryItems()
    val menuBuffer = new StringBuilder()
    var currentIndex: Int = 0
    var yNext = origin._2

    def addToMenu(index: Int, item: AbstractItem): Unit = {
      val printedIndex =
        if (ui.inInventory) (index + 'a').toChar.toString else ""
      if (ui.selectedItem == index) {
        yNext = StringRenderer.drawString(
          g,
          menuBuffer.toString,
          (origin._1, yNext)
        )
        yNext = StringRenderer.drawString(
          g,
          ">> " + printedIndex + " - " + item.name + "\n",
          (origin._1, yNext),
          selectedColor
        )
        menuBuffer.clear()
      } else
        menuBuffer ++= " " + printedIndex + " - " + item.name + "\n"
    }

    menuBuffer ++= "Equiped items :\n"
    equiped.foreach(item => {
      addToMenu(currentIndex, item)
      currentIndex += 1
    })
    menuBuffer ++= "\nInventory :\n"
    inventory.foreach(item => {
      addToMenu(currentIndex, item)
      currentIndex += 1
    })

    StringRenderer.drawString(g, menuBuffer.toString, (origin._1, yNext))
  }
}
