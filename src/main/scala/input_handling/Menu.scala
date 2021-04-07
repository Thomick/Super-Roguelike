package input_handling

import scala.collection._
import main._

// Parent class for a menu, handles cursor movement, [confirm] needs to be overwritten to add an effect when the player select an item
// Should be added on top of UI.menuStack in order to be drawn on the screen
class Menu {
  val name = "Menu"
  // [items] stores the couple (name,imageName)
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

  def onClosing(): Unit = ()
}

class MainMenu extends Menu {
  override val name = "Main Menu"
  items += (("New Game", ""), ("Load Game", ""), ("Demonstration of the shop", ""), ("Demonstration of the lock", ""), ("Demonstration of the computer", ""), ("Save Game", ""))

  override def confirm(): Unit = {
    cursorIndex match {
      case 0 => Main.newGame
      case 1 => Main.loadGame
      case 2 => Main.loadDemo1
      case 3 => Main.loadDemo2
      case 4 => Main.loadDemo3
      case 5 => Main.saveGame
      case _ => ()
    }
  }
}
