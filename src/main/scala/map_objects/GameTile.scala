package map_objects

import java.awt.Color
import game.Game
import main._

@SerialVersionUID(10123L)
abstract class GameTile extends Serializable {
  var blocking: Boolean
  var blocking_sight: Boolean
  var explored = false
  var turnToCross = 1
  var lightColor: Color
  var darkColor: Color
  var hasImage: Boolean = false
  var image: String = "src/main/resources/other_sprites/placeholder.png"
}

trait InteractableTile extends GameTile {
  def interact(board: GameBoard, c: game_entities.Character): Unit
}

class FloorTile() extends GameTile {
  var blocking = false
  var blocking_sight = false
  var lightColor: Color = new Color(99, 150, 150)
  var darkColor: Color = new Color(79, 119, 119)
}

class WallTile() extends GameTile {
  var blocking = true
  var blocking_sight = true
  var lightColor: Color = new Color(168, 255, 255)
  var darkColor: Color = new Color(134, 204, 204)
}

class UpElevator() extends GameTile {
  var blocking = false
  var blocking_sight = false
  var lightColor: Color = new Color(100, 222, 30)
  var darkColor: Color = new Color(80, 202, 10)
}

class DownElevator() extends GameTile {
  var blocking = false
  var blocking_sight = false
  var lightColor: Color = new Color(185, 80, 181)
  var darkColor: Color = new Color(165, 60, 161)
}

class BrokenElevator() extends GameTile {
  var blocking = false
  var blocking_sight = false
  var lightColor: Color = new Color(135, 67, 8)
  var darkColor: Color = new Color(115, 47, 0)
}

class Door extends GameTile with InteractableTile {
  var blocking = true
  var blocking_sight = true
  var lightColor: Color = new Color(200, 200, 200)
  var darkColor: Color = new Color(150, 150, 150)
  var opened: Boolean = false
  var locked: Boolean = false

  // Opens the door, changes its color, and allows to see through
  def open(): Unit = {
    opened = true
    blocking = false
    blocking_sight = false
    lightColor = new Color(0, 200, 0)
    darkColor = new Color(0, 100, 0)
  }

  def close(): Unit = {
    opened = false
    blocking = true
    blocking_sight = true
    lightColor = new Color(200, 200, 200)
    darkColor = new Color(150, 150, 150)
  }

  def lock(): Unit = {
    close()
    locked = true
    lightColor = new Color(200, 0, 0)
    darkColor = new Color(100, 0, 0)
  }

  def unlock(): Unit = {
    close()
    locked = false
  }

  def interact(board: GameBoard, c: game_entities.Character): Unit = {
    if (!opened && !locked) {
      open()
      // An update of the field of view may be needed as the it is now possible to see what is behind the door
      Main.fovmap.update(board.grid)
    }
  }
}

class KeyLockedDoor extends Door {
  lock()
  // Open the door and remove one key from player inventory
  override def interact(board: GameBoard, c: game_entities.Character): Unit = {
    if (locked && c.isInstanceOf[game_entities.Player]) {
      val player = c.asInstanceOf[game_entities.Player]
      if (player.keyCount > 0) {
        player.keyCount -= 1
        unlock()
      }
    } else
      super.interact(board, c)
  }
}

// Door which can be opened using a Trigger
class TriggerableDoor extends Door with Triggerable {
  lock()
  def executeAction(): Unit = unlock
}
