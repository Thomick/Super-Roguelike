package input_handling

import scala.collection._
import main._

class Menu {
  val name = "Menu"
  val items = new mutable.ArrayBuffer[(String, String)]
  val hasImage: Boolean = false
  var cursorIndex: Int = 0

  def selectNext(): Unit =
    if (items.size > 0)
      cursorIndex = (cursorIndex + 1) % items.size
    else
      cursorIndex = 0

  def selectPrev(): Unit =
    if (items.size > 0)
      cursorIndex = (cursorIndex - 1 + items.size) % items.size
    else
      cursorIndex = 0

  def confirm(): Unit = ()
}

class MainMenu extends Menu {
  override val name = "Main Menu"
  items += (("New Game", ""), ("Load Game", ""), ("Save Game", ""))

  override def confirm(): Unit = {
    cursorIndex match {
      case 0 => Main.newGame
      case 1 => Main.loadGame
      case 2 => Main.saveGame
      case _ => ()
    }
  }
}
