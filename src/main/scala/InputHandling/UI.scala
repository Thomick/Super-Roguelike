package InputHandling

import swing._
import event._
import event.Key._
import GameEntities._
import map_objects.GameBoard

class UI {
  var lastKey: String = ""
  var lastIsMove: Boolean = false
  var lastDir: Direction.Value = Direction.Nop
  var inInventory: Boolean = false
  var itemSlected: Int = 0
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

  def applyCommand(board: GameBoard) {
    if (lastIsMove) {
      board.playerEntity.move(lastDir)
    } else if (inInventory) {
      lastKey match {
        case "E"      => board.playerEntity.pickUpItem()
        case "D"      => board.playerEntity.dropItem(0)
        case "C"      => board.playerEntity.consumeItem(0)
        case "T"      => board.playerEntity.throwItem(0, lastDir)
        case "R"      => board.playerEntity.equipItem(0)
        case "F"      => board.playerEntity.unequipItem(0)
        case "Escape" => inInventory = false
        case _        => {}
      }
    } else {
      lastKey match {
        case "E" => board.playerEntity.pickUpItem()
        case "I" => inInventory = true
        case _   => {}
      }
    }
    board.update()
  }
}
