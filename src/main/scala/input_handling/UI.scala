package input_handling

import swing._
import event._
import event.Key._
import game_entities._
import map_objects.GameBoard
import fov_functions._
import scala.math.max

class UI {
  var mode: String = "normal"
  var lastKey: String = ""
  var lastIsMove: Boolean = false
  var lastDir: Direction.Value = Direction.Nop
  var inInventory: Boolean = false
  var selectedItem: Int = 0
  def isNormalMode() : Boolean = (mode == "normal")
  def isCursorMode() : Boolean = (mode == "cursor")
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

  def applyCommand(board: GameBoard, lightMap: FovMap) {
    val player = board.playerEntity
    val cursor = board.cursor
    var doUpdate = true
    val isSelectedItemEquiped = selectedItem < player.equipedItems.length
    val currentIndex =
      if (isSelectedItemEquiped) selectedItem
      else selectedItem - player.equipedItems.length
    if (lastIsMove) {
      if (isNormalMode()) player.moveDir(lastDir)
      if (isCursorMode()) {
        cursor.move(lastDir)
        doUpdate = false
      }

    } else {
      lastKey match {
        case "E" => 
          if (isNormalMode()) player.pickUpItem()
          if (isCursorMode()) doUpdate = false
        case "D" => 
          if (!isSelectedItemEquiped && isNormalMode()) player.dropItem(currentIndex)
          if (isCursorMode()) doUpdate = false
        case "C" => 
          if (!isSelectedItemEquiped && isNormalMode()) player.consumeItem(currentIndex)
          if(isCursorMode()) doUpdate = false
        case "T" =>
          if (!isSelectedItemEquiped && isNormalMode()) player.throwItem(currentIndex, lastDir)
          if (isCursorMode()) doUpdate = false
        case "R" =>
          if (isSelectedItemEquiped && isNormalMode()) player.unequipItem(currentIndex)
          else if (isNormalMode()) player.equipItem(currentIndex)
          if (isCursorMode()) doUpdate = false
        case "F" => 
          if (isSelectedItemEquiped && isNormalMode()) player.unequipItem(currentIndex)
          if (isCursorMode()) doUpdate = false
        // Unused
        /*case "I" =>
          inInventory = !inInventory
          doUpdate = false*/
        case "O" =>
          if (isNormalMode()){
            selectedItem -= 1
          }
          doUpdate = false
        case "I" =>
          if (isNormalMode()){
            selectedItem += 1
          }
          doUpdate = false
        case "V" =>
          mode = "cursor"
          cursor.makeVisible
          cursor.backToPlayer
          doUpdate = false
        case "Echap" =>
          mode = "normal"
          cursor.makeInvisble
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
