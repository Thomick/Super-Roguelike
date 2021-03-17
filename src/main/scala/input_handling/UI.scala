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
  def isThrowMode() : Boolean = (mode == "throw")
  def isFireMode() : Boolean = (mode == "shoot")
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
    var doUpdate = false
    val isSelectedItemEquiped = selectedItem < player.equipedItems.length
    val currentIndex =
      if (isSelectedItemEquiped) selectedItem
      else selectedItem - player.equipedItems.length
    if (lastIsMove) {
      if (isNormalMode()) {
        player.moveDir(lastDir)
        doUpdate = true
      }
      if (isCursorMode() || isThrowMode() || isFireMode()) cursor.move(lastDir)
    } else {
      if (isNormalMode()) {
        doUpdate = true
        lastKey match {
          case "E" => 
            player.pickUpItem()
          case "D" => 
            if (!isSelectedItemEquiped) player.dropItem(currentIndex)
          case "C" => 
            if (!isSelectedItemEquiped) player.consumeItem(currentIndex)
          case "T" =>
            if (!isSelectedItemEquiped) {
              if (player.canThrowItem(currentIndex)) {
                mode = "throw"
                cursor.makeVisible
                cursor.backToPlayer
                doUpdate = false
              }
            }
          case "R" =>
            if (isSelectedItemEquiped) player.unequipItem(currentIndex)
            else player.equipItem(currentIndex)
          case "F" => 
            if (isSelectedItemEquiped) player.unequipItem(currentIndex)
          // Unused
          /*case "I" =>
            inInventory = !inInventory
            doUpdate = false*/
          case "O" =>
            selectedItem -= 1
            doUpdate = false
          case "I" =>
            selectedItem += 1
            doUpdate = false
          case "V" =>
            mode = "cursor"
            cursor.makeVisible
            cursor.backToPlayer
            doUpdate = false
          case _ => doUpdate = false
        }
      }
      else if (isCursorMode()) {
        lastKey match {
          case "Echap" || "Escape" =>
            mode = "normal"
            cursor.makeInvisible
          case _ => ()
        }
      }
      else if (isThrowMode()) {
        lastKey match {
          case "Echap" || "Escape" =>
            mode = "normal"
            cursor.makeInvisible
          case "T" =>
            if (lightMap.is_light(cursor.xpos,cursor.ypos)) {
              if(!board.grid(cursor.xpos)(cursor.ypos).blocking) {
                player.throwItem(currentIndex,cursor.pos)
                mode = "normal"
                cursor.makeInvisible
                doUpdate = true
              }
            }

          case _ => ()
        }
      }
      else if (isFireMode()) {
        lastKey match {
          case "Echap" || "Escape" =>
            mode = "normal"
            cursor.makeInvisible
          case _ => ()
        }
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
