package Rendering

import GameEntities._
import java.awt.Graphics2D

object InventoryRenderer {
  def drawInventory(
      g: Graphics2D,
      origin: (Int, Int),
      player: Player,
      selected: Boolean
  ): Unit = {
    val equiped = player.getEquipedItems()
    val inventory = player.getInventoryItems()
    val menu = new StringBuilder()
    var currentChar: Char = 'a'
    menu ++= "Equiped items :\n"
    if (selected) {
      equiped.foreach(item => {
        menu ++= " " + currentChar + " - " + item.name + "\n"
        currentChar = (currentChar + 1).toChar
      })
    } else {
      equiped.foreach(item => menu ++= " - " + item.name + "\n")
    }
    menu ++= "\nInventory :\n"
    if (selected) {
      inventory.foreach(item => {
        menu ++= " " + currentChar + " - " + item.name + "\n"
        currentChar = (currentChar + 1).toChar
      })
    } else {
      inventory.foreach(item => menu ++= " - " + item.name + "\n")
    }

    StringRenderer.drawString(g, menu.toString, origin)
  }
}
