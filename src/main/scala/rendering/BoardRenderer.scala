package rendering

import java.awt.{Color, Graphics2D, Rectangle, Toolkit}
import scala.math.{pow, sqrt, min}
import map_objects._
import game_entities._
import fov_functions._
import cursor._
import scala.collection._

object BoardRenderer {
  val errorColor = new Color(255, 0, 0)
  val tilePadding = 0

  def drawBoard(
      g: Graphics2D,
      gridOrigin: (Int, Int),
      board: GameBoard,
      fovmap: FovMap,
      baseTile: (Int, Int),
      boardSize: Int,
      nbTiles: Int
  ): Array[GameEntity] = {
    val tileSize = boardSize / (nbTiles + tilePadding)
    //drawnEntities is used to show information about visible entities
    var drawnEntities = new mutable.ArrayBuffer[GameEntity]

    def buildRect(pos: (Int, Int)): Rectangle = {
      new Rectangle(
        gridOrigin._1 + pos._1 * (tileSize + tilePadding),
        gridOrigin._2 + pos._2 * (tileSize + tilePadding),
        tileSize,
        tileSize
      )
    }

    def drawGrid() = {
      for {
        x <- baseTile._1 to min(baseTile._1 + nbTiles, board.size_x - 1)
        y <- baseTile._2 to min(baseTile._2 + nbTiles, board.size_y - 1)
        val pos = (x, y)
      } {
        // Select tile color depending on tile type and visibility
        if (fovmap.is_light(x, y))
          board.grid(x)(y).explored = true
        if (board.grid(x)(y).explored) {
          if (fovmap.is_light(x, y))
            g.setColor(board.grid(x)(y).lightColor)
          else
            g.setColor(board.grid(x)(y).darkColor)
          g.fill(buildRect(pos))
        }
      }
    }

    def drawImageOnBoard(imgName: String, pos: (Int, Int)): Unit = {
      val img = Toolkit.getDefaultToolkit().getImage(imgName)
      g.drawImage(
        img,
        pos._1 * (tilePadding + tileSize) + gridOrigin._1,
        pos._2 * (tilePadding + tileSize) + gridOrigin._2,
        tileSize,
        tileSize,
        null
      )
      g.finalize()
    }

    def drawEntities() = {
      val entities = board.getEntities()
      entities.foreach(e => {
        if (fovmap.is_light(e.pos._1, e.pos._2)) {
          drawnEntities += e
          drawImageOnBoard(e.image, e.pos)
        }
      })
    }

    def drawCursor(): Unit = {
      val c = board.cursor
      if (c.visible) {
        for {
          i <- c.xpos - c.rangeOfEffect to c.xpos + c.rangeOfEffect;
          j <- c.ypos - c.rangeOfEffect to c.ypos + c.rangeOfEffect
        } {
          if (sqrt(pow(c.xpos - i, 2) + pow(c.ypos - j, 2)) <= c.rangeOfEffect) {
            if (i == c.xpos && j == c.ypos)
              drawImageOnBoard(c.image, (i, j))
            else if (c.areaOfEffect)
              drawImageOnBoard(c.imageAoE, (i, j))
          }
        }
      }
      if (c.highlightPath && c.highlightedCells._2) {
        val imgH = Toolkit.getDefaultToolkit().getImage(c.highlightImage)
        val path = c.highlightedCells._1
        for (cell <- path) {
          if (fovmap.is_light(cell._1, cell._2)) {
            drawImageOnBoard(c.highlightImage, cell)
          }
        }
      }
    }

    drawGrid()
    drawEntities()
    drawCursor()
    return drawnEntities.toArray
  }
}
