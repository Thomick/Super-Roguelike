package InputHandling

import swing._
import event._
import event.Key._
import GameEntities._

class AbstractUI {
  var lastKey: String = ""
  var lastIsMove: Boolean = false
  var lastDir: Direction.Value = Direction.Nop
  def newKeyPressed(keyCode: Value) = {
    keyCode match {
      case Up => {
        lastDir = Direction.Up
        lastIsMove = true
      }
      case Down => {
        lastDir = Direction.Down
        lastIsMove = true
      }
      case Left => {
        lastDir = Direction.Left
        lastIsMove = true
      }
      case Right => {
        lastDir = Direction.Right
        lastIsMove = true
      }
      case _ => {
        lastIsMove = false
      }
    }
    lastKey = keyCode.toString
  }
  def last: String = lastKey
}
