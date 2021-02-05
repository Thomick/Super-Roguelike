package Rendering

import swing._
import java.awt.Graphics2D
import java.awt.Color
import map_objects._
import fov_functions._

class Renderer {
  val bgColor = new Color(48, 99, 99)
  val errorColor = new Color(255, 0, 0)
  val gridOrigin = (10, 10)
  val BoardRenderer = new BoardRenderer(600, 30)

  def onPaint(
      g: Graphics2D,
      board: GameBoard,
      lastkey: String,
      screen_width: Int,
      screen_height: Int,
      fovmap: FovMap
  ) {
    fovmap.compute_fov(board.playerEntity.pos._1, board.playerEntity.pos._2)
    g setColor bgColor
    g fillRect (0, 0, screen_width, screen_height)
    val infos =
      "Last key pressed : " + lastkey + "\nImplemented command : \n" +
        " - Move with Arrows keys \n" + " - E to pick items up (You must face it) \n" +
        " - D to drop an item\n" + " - C to consume an item\n" + " - T to throw an item \n" +
        " - R to equip an item\n" + "F to unequip and item"
    StringRenderer.drawString(
      g,
      infos,
      (640, 20)
    )
    BoardRenderer.drawBoard(g, (20, 20), board, fovmap, (0, 0))
  }
}
