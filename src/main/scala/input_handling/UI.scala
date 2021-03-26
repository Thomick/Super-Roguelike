package input_handling

import swing._
import event._
import event.Key._
import game_entities._
import map_objects.GameBoard
import fov_functions._
import scala.math.max
import scala.collection._
import rendering.Menu

object GameMode extends Enumeration {
  val Normal, Cursor, Throw, Shoot, Shop = Value
}

object UI {
  var mode: GameMode.Value = GameMode.Normal
  var lastKey: Key.Value = Up
  var lastIsMove: Boolean = false
  var lastDir: Direction.Value = Direction.Nop
  var inInventory: Boolean = false
  var selectedItem: Int = 0
  val menuStack = new mutable.Stack[Menu]

  def isNormalMode(): Boolean = (mode == GameMode.Normal)
  def isCursorMode(): Boolean = (mode == GameMode.Cursor)
  def isThrowMode(): Boolean = (mode == GameMode.Throw)
  def isFireMode(): Boolean = (mode == GameMode.Shoot)
  def newKeyPressed(keyCode: Key.Value) = {
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
    lastKey = keyCode
  }

  def last: String = lastKey.toString

  def applyCommand(board: GameBoard, lightMap: FovMap) {
    val player = board.playerEntity
    val cursor = board.cursor
    var doUpdate = false
    val isSelectedItemEquiped = selectedItem < player.equipedItems.length
    val currentIndex =
      if (isSelectedItemEquiped) selectedItem
      else selectedItem - player.equipedItems.length
    if (!menuStack.isEmpty) {
      lastKey match {
        case O =>
          menuStack.top.selectNext()
        case I =>
          menuStack.top.selectPrev()
        case Enter =>
          menuStack.top.confirm()
        case Escape =>
          menuStack.pop()
        case _ => ()
      }
    } else if (lastIsMove) {
      if (isNormalMode()) {
        player.updateStatus()
        player.moveDir(lastDir)
        doUpdate = true
      }
      if (isCursorMode() || isThrowMode() || isFireMode()) cursor.move(lastDir)
    } else {
      if (isNormalMode()) {
        doUpdate = true
        lastKey match {
          case E => {
            player.updateStatus()
            player.pickUpItem()
          }
          case D => {
            player.updateStatus()
            if (!isSelectedItemEquiped) player.dropItem(currentIndex)
          }
          case C => {
            player.updateStatus()
            if (!isSelectedItemEquiped) player.consumeItem(currentIndex)
          }
          case T => {
            if (!isSelectedItemEquiped) {
              if (player.canThrowItem(currentIndex)) {
                mode = GameMode.Throw
                cursor.makeVisible
                cursor.backToPlayer
              }
            }
            doUpdate = false
          }
          case R => {
            player.updateStatus()
            if (isSelectedItemEquiped) player.unequipItem(currentIndex)
            else player.equipItem(currentIndex)
          }
          // Unused
          /*case I =>
            inInventory = !inInventory
            doUpdate = false*/
          case O =>
            selectedItem -= 1
            doUpdate = false
          case I =>
            selectedItem += 1
            doUpdate = false
          case V =>
            mode = GameMode.Cursor
            cursor.makeVisible
            cursor.backToPlayer
            doUpdate = false
          case _ => doUpdate = false
        }
      } else if (isCursorMode()) {
        lastKey match {
          case Escape =>
            mode = GameMode.Normal
            cursor.makeInvisible
          case _ => ()
        }
      } else if (isThrowMode()) {
        lastKey match {
          case Escape =>
            mode = GameMode.Normal
            cursor.makeInvisible
          case T =>
            if (lightMap.is_light(cursor.xpos, cursor.ypos)) {
              if (!board.grid(cursor.xpos)(cursor.ypos).blocking) {
                player.updateStatus()
                player.throwItem(currentIndex, cursor.pos)
                mode = GameMode.Normal
                cursor.makeInvisible
                doUpdate = true
              }
            }

          case _ => ()
        }
      } else if (isFireMode()) {
        lastKey match {
          case Escape =>
            mode = GameMode.Normal
            cursor.makeInvisible
          case _ => ()
        }
      }
    }
    if (doUpdate) {
      board.update(lightMap)
    }
    selectedItem = Math.floorMod(
      selectedItem,
      max(1, player.inventory.length + player.equipedItems.length)
    )
  }
}
