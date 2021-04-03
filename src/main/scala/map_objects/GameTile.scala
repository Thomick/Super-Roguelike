package map_objects

import java.awt.Color

@SerialVersionUID(10123L)
abstract class GameTile() extends Serializable {
  val blocking: Boolean
  val blocking_sight: Boolean
  var explored = false
  val turnToCross = 1
  val lightColor: Color
  val darkColor: Color
}

case class FloorTile() extends GameTile {
  val blocking = false
  val blocking_sight = false
  val lightColor: Color = new Color(99, 150, 150)
  val darkColor: Color = new Color(79, 119, 119)
}

case class WallTile() extends GameTile {
  val blocking = true
  val blocking_sight = true
  val lightColor: Color = new Color(168, 255, 255)
  val darkColor: Color = new Color(134, 204, 204)
}

case class UpElevator() extends GameTile {
  val blocking = false
  val blocking_sight = false
  val lightColor: Color = new Color(100, 222, 30)
  val darkColor: Color = new Color(80, 202, 10)
}

case class DownElevator() extends GameTile {
  val blocking = false
  val blocking_sight = false
  val lightColor: Color = new Color(185, 80, 181)
  val darkColor: Color = new Color(165, 60, 161)
}

case class BrokenElevator() extends GameTile {
  val blocking = false
  val blocking_sight = false
  val lightColor: Color = new Color(135, 67, 8)
  val darkColor: Color = new Color(115, 47, 0)
}
