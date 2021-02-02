package render_functions

import swing._
import event._
import event.Key._
import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle, Toolkit}
import java.awt.{Color => AWTColor}
import map_objects._
import GameEntities._

object Render {
  val bgColor = new AWTColor(48, 99, 99)
  val writeColor = new AWTColor(200, 200, 200)
  val floorColor = new AWTColor(99, 150, 150)
  val wallColor = new AWTColor(255, 255, 255)
  val errorColor = new AWTColor(255, 0, 0)
  val tileSize = 20
  val tileOffset = 0
  val gridOrigin = (10, 10)
  def onPaint(
      g: Graphics2D,
      board: GameBoard,
      lastkey: String,
      screen_width: Int,
      screen_height: Int
  ) {
    g setColor bgColor
    g fillRect (0, 0, screen_width, screen_height)
    g setColor writeColor
    g drawString (lastkey, gridOrigin._1 + board.size_x * (tileOffset + tileSize) + 20, 20)

    def buildRect(pos: (Int, Int)): Rectangle =
      new Rectangle(
        gridOrigin._1 + pos._1 * (tileSize + tileOffset),
        gridOrigin._2 + pos._2 * (tileSize + tileOffset),
        tileSize,
        tileSize
      )
    def drawGrid() = {
      for {
        x <- 0 to board.size_x - 1
        y <- 0 to board.size_y - 1
        val pos = (x, y)
      } {
        board.grid(x)(y) match {
          case WallTile() => g.setColor(wallColor)
          case FloorTile() =>
            g.setColor(floorColor)
          case _ =>
            println("No match")
            g.setColor(errorColor)
        }
        g.fill(buildRect(pos))
      }
    }
    def drawEntity(e: GameEntity) = {
      val img = Toolkit.getDefaultToolkit().getImage(e.image)
      g.drawImage(
        img,
        e.pos._1 * (tileOffset + tileSize) + gridOrigin._1,
        e.pos._2 * (tileOffset + tileSize) + gridOrigin._2,
        tileSize,
        tileSize,
        null
      )
      g.finalize()
    }
    def drawEntities() = {
      val entities = board.getEntities()
      entities.foreach(e => drawEntity(e))
    }
    drawGrid()
    drawEntities()
  }
}
