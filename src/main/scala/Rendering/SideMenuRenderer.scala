package Rendering

import GameEntities._
import InputHandling.UI
import java.awt.{Graphics2D, Color, Toolkit}
import Items.AbstractItem
import map_objects.GameBoard

object SideMenuRenderer {
  val selectedColor = new Color(255, 200, 200)

  def drawVisibleEntitiesPanel(
      g: Graphics2D,
      origin: (Int, Int),
      drawnEntities: Array[GameEntity]
  ): Int = {
    val entitySize = g.getFont().getSize()
    var yNext =
      StringRenderer.drawString(g, "Visible : \n", origin) + entitySize
    drawnEntities.foreach(e => {
      g.drawImage(
        Toolkit.getDefaultToolkit().getImage(e.image),
        origin._1,
        yNext - entitySize,
        entitySize,
        entitySize,
        null
      )
      g.finalize()
      yNext = StringRenderer.drawString(
        g,
        e.name,
        (origin._1 + entitySize + 2, yNext)
      )
    })
    return yNext
  }

  def drawInventory(
      g: Graphics2D,
      origin: (Int, Int),
      player: Player,
      ui: UI
  ): Int = {
    val equiped = player.getEquipedItems()
    val inventory = player.getInventoryItems()
    val menuBuffer = new StringBuilder()
    var currentIndex: Int = 0
    var yNext = origin._2
    var selectedItem: Option[AbstractItem] = None

    menuBuffer ++= "HP : " + player.currentHP.toString + "/" + player.getMaxHP.toString + "\n"
    menuBuffer ++= "Attack power : " + player.getAtt.toString + "\n"
    menuBuffer ++= "Defense : " + player.getDef.toString + "\n\n"

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
        selectedItem = Some(item)
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

    selectedItem match {
      case None => ()
      case Some(item) =>
        menuBuffer ++= "\nAvailable actions :\n"
        item.availableActions.foreach(action => menuBuffer ++= action + "\n")
    }

    yNext =
      StringRenderer.drawString(g, menuBuffer.toString, (origin._1, yNext))
    return yNext
  }
}
