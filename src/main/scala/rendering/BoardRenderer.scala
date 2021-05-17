package rendering

import java.awt.{Color, Graphics2D, Rectangle, Toolkit}
import scala.math.{pow, sqrt, min, max}
import map_objects._
import game_entities._
import fov_functions._
import cursor._
import scala.collection._
import input_handling._

object BoardRenderer {
  val errorColor = new Color(255, 0, 0)
  val tilePadding = 0

  def drawBoard(
      g: Graphics2D,
      gridOrigin: (Int, Int),
      board: GameBoard,
      fovmap: FovMap,
      boardSize: Int,
      nbTiles: Int
  ): Array[GameEntity] = {
    val tileSize = boardSize / (nbTiles + tilePadding)
    //drawnEntities is used to show information about visible entities
    var drawnEntities = new mutable.ArrayBuffer[GameEntity]
    // Compute the target position for the center of the drawn area wrt the current gamemode
    val center =
      if (!UI.isNormalMode)
        board.cursor.pos
      else
        board.playerEntity.pos
    // Compute the real position of the tile drawn in the top-left corner
    val baseTile = (
      max(0, min(center._1 - nbTiles / 2, board.size_x - nbTiles)),
      max(0, min(center._2 - nbTiles / 2, board.size_y - nbTiles))
    )

    def buildRect(pos: (Int, Int)): Rectangle = {
      new Rectangle(
        gridOrigin._1 + pos._1 * (tileSize + tilePadding),
        gridOrigin._2 + pos._2 * (tileSize + tilePadding),
        tileSize,
        tileSize
      )
    }

    // Compute the relative position of a drawn tile
    def mapPosToScren(pos: (Int, Int)) =
      (pos._1 - baseTile._1, pos._2 - baseTile._2)

    def drawGrid() = {
      for {
        x <- baseTile._1 to min(baseTile._1 + nbTiles - 1, board.size_x - 1)
        y <- baseTile._2 to min(baseTile._2 + nbTiles - 1, board.size_y - 1)
        val pos = mapPosToScren(x, y)
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

    def onScreen(pos: (Int, Int)): Boolean = {
      baseTile._1 <= pos._1 && pos._1 < baseTile._1 + nbTiles && baseTile._2 <= pos._2 && pos._2 < baseTile._2 + nbTiles
    }

    def drawEntities() = {
      val entities = board.getEntities()
      entities.foreach(e => {
        if (fovmap.is_light(e.pos._1, e.pos._2) && onScreen(e.pos)) {
          drawnEntities += e
          drawImageOnBoard(e.image, mapPosToScren(e.pos))
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
            if (onScreen(i, j))
              if (i == c.xpos && j == c.ypos)
                drawImageOnBoard(c.image, mapPosToScren(i, j))
              else if (c.areaOfEffect)
                drawImageOnBoard(c.imageAoE, mapPosToScren(i, j))
          }
        }
      }
      if (c.highlightPath && c.highlightedCells._2) {
        val imgH = Toolkit.getDefaultToolkit().getImage(c.highlightImage)
        val path = c.highlightedCells._1
        for (cell <- path) {
          if (fovmap.is_light(cell._1, cell._2) && onScreen(cell)) {
            drawImageOnBoard(c.highlightImage, mapPosToScren(cell))
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
