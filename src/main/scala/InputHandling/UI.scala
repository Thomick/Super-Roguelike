package InputHandling

import swing._
import event._
import event.Key._
import GameEntities._
import map_objects.GameBoard
import fov_functions._
import scala.math.max

class UI {
  var lastKey: String = ""
  var lastIsMove: Boolean = false
  var lastDir: Direction.Value = Direction.Nop
  var inInventory: Boolean = false
  var selectedItem: Int = 0
  def newKeyPressed(keyCode: Value) = {
    keyCode match {
      case Up | K => {
        lastDir = Direction.Up
        lastIsMove = true
      }
      case Down | J => {
        lastDir = Direction.Down
        lastIsMove = true
      }
      case Left | H => {
        lastDir = Direction.Left
        lastIsMove = true
      }
      case Right | L => {
        lastDir = Direction.Right
        lastIsMove = true
      }
      case Y => {
        lastDir = Direction.UpLeft
        lastIsMove = true
      }
      case U => {
        lastDir = Direction.UpRight
        lastIsMove = true
      }
      case B => {
        lastDir = Direction.DownLeft
        lastIsMove = true
      }
      case N => {
        lastDir = Direction.DownRight
        lastIsMove = true
      }
      case _ => {
        lastIsMove = false
      }
    }
    lastKey = keyCode.toString
  }

  def last: String = lastKey

  def applyCommand(board: GameBoard, lightMap : FovMap) {
    val player = board.playerEntity
    var doUpdate = true
    val isSelectedItemEquiped = selectedItem < player.equipedItems.length
    val currentIndex =
      if (isSelectedItemEquiped) selectedItem
      else selectedItem - player.equipedItems.length
    if (lastIsMove) {
      player.moveDir(lastDir)
    } else {
      lastKey match {
        case "E" => player.pickUpItem()
        case "D" => if (!isSelectedItemEquiped) player.dropItem(currentIndex)
        case "C" => if (!isSelectedItemEquiped) player.consumeItem(currentIndex)
        case "T" =>
          if (!isSelectedItemEquiped) player.throwItem(currentIndex, lastDir)
        case "R" =>
          if (isSelectedItemEquiped) player.unequipItem(currentIndex)
          else player.equipItem(currentIndex)
        case "F" => if (isSelectedItemEquiped) player.unequipItem(currentIndex)
        // Unused
        /*case "I" =>
          inInventory = !inInventory
          doUpdate = false*/
        case "Virgule" =>
          selectedItem -= 1
          doUpdate = false
        case "I" =>
          selectedItem += 1
          doUpdate = false
        case _ => doUpdate = false
      }
    }
    if (doUpdate)
      board.update(lightMap)
    selectedItem = Math.floorMod(
      selectedItem,
      max(1, player.inventory.length + player.equipedItems.length)
    )
  }
}
