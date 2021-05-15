package rendering

import game_entities._
import input_handling.UI
import java.awt.{Graphics2D, Color, Toolkit}
import items.AbstractItem
import map_objects.GameBoard
import items.Money

object SideMenuRenderer {
  val selectedColor = new Color(255, 200, 200)

  // Show the name of visible entities
  def drawVisibleEntitiesPanel(
      g: Graphics2D,
      origin: (Int, Int),
      drawnEntities: Array[GameEntity]
  ): Int = {
    val entitySize = g.getFont().getSize()
    var yNext =
      StringRenderer.drawString(g, "Visible : \n", origin) + entitySize

    def getAdditionalInfo(e: GameEntity): String = {
      if (e.isInstanceOf[Character]) {
        val c = e.asInstanceOf[Character]
        val infos = new StringBuilder
        infos ++= " (" + c.currentHP + "/" + c.getMaxHP() + ")"
        c.statusList.foreach(s => infos ++= " (" + s.name + ":" + s.remainingTimeString() + ")")
        infos ++= s" Lvl. ${c.level}"
        return infos.toString()
      }
      if (e.isInstanceOf[ItemEntity]) {
        val item = e.asInstanceOf[ItemEntity].associatedItem
        if (item.isInstanceOf[Money])
          return s"(${item.asInstanceOf[Money].value} §)"
      }
      return ""
    }

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
        e.name + getAdditionalInfo(e),
        (origin._1 + entitySize + 2, yNext)
      )
    })
    return yNext
  }

  // Show player status, equiped items, items in the inventory and possible actions for the currently selected item
  def drawPlayerInfo(
      g: Graphics2D,
      origin: (Int, Int),
      player: Player
  ): Int = {
    val equiped = player.getEquipedItems()
    val inventory = player.getInventoryItems()
    val menuBuffer = new StringBuilder()
    var currentIndex: Int = 0 // Used for direct item selection
    var yNext = origin._2
    var selectedItem: Option[AbstractItem] = None

    menuBuffer ++= "HP : " + player.currentHP.toString + "/" + player.getMaxHP.toString + "\n"
    menuBuffer ++= s"Level : ${player.level}   Xp : ${player.xp}/${player.nextLevelCap}\n"
    menuBuffer ++= "Attack power : " + player.getAtt.toString + "\n"
    menuBuffer ++= "Defense : " + player.getDef.toString + "\n"
    menuBuffer ++= s"Money : ${player.money.toString}    Keys : ${player.keyCount}\n"
    menuBuffer ++= "Status : \n"
    player.statusList.foreach(s => menuBuffer ++= " - " + s.name + " : " + s.remainingTimeString() + "\n")
    menuBuffer ++= "\n"

    // Appends an item to the menu (with correct information and color)
    def addToMenu(index: Int, item: AbstractItem): Unit = {
      val printedIndex =
        if (UI.inInventory) (index + 'a').toChar.toString
        else "" // Information for direct item selection
      if (UI.selectedItem == index) {
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

    yNext = StringRenderer.drawString(g, menuBuffer.toString, (origin._1, yNext))
    return yNext
  }
}
