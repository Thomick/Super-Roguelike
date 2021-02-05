package Rendering

import java.awt.{Color, Graphics2D, Rectangle, Toolkit}
import scala.math.min
import map_objects._
import GameEntities._
import fov_functions._

class BoardRenderer(boardSize: Int, nbTiles: Int) {
  val lightfloorColor = new Color(99, 150, 150)
  val lightwallColor = new Color(168, 255, 255)
  val darkfloorColor = new Color(79, 119, 119)
  val darkwallColor = new Color(134, 204, 204)
  val errorColor = new Color(255, 0, 0)
  val tileSize = boardSize / nbTiles
  val tileOffset = 0

  def drawBoard(
      g: Graphics2D,
      gridOrigin: (Int, Int),
      board: GameBoard,
      fovmap: FovMap,
      baseTile: (Int, Int)
  ): Unit = {

    def buildRect(pos: (Int, Int)): Rectangle =
      new Rectangle(
        gridOrigin._1 + pos._1 * (tileSize + tileOffset),
        gridOrigin._2 + pos._2 * (tileSize + tileOffset),
        tileSize,
        tileSize
      )
    def drawGrid() = {
      for {
        x <- baseTile._1 to min(baseTile._1 + nbTiles, board.size_x - 1)
        y <- baseTile._2 to min(baseTile._2 + nbTiles, board.size_y - 1)
        val pos = (x, y)
      } {
        if (fovmap.is_light(x, y))
          board.grid(x)(y).explored = true
        if (board.grid(x)(y).explored) {
          if (fovmap.is_light(x, y)) {
            board.grid(x)(y) match {
              case WallTile() => g.setColor(lightwallColor)
              case FloorTile() =>
                g.setColor(lightfloorColor)
              case _ =>
                println("No match")
                g.setColor(errorColor)
            }
          } else {
            board.grid(x)(y) match {
              case WallTile() => g.setColor(darkwallColor)
              case FloorTile() =>
                g.setColor(darkfloorColor)
              case _ =>
                println("No match")
                g.setColor(errorColor)
            }
          }
          g.fill(buildRect(pos))
        }
      }
    }
    def drawEntity(e: GameEntity) = {
      if (fovmap.is_light(e.pos._1, e.pos._2)) {
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
    }
    def drawEntities() = {
      val entities = board.getEntities()
      entities.foreach(e => drawEntity(e))
    }
    drawGrid()
    drawEntities()
  }
}
