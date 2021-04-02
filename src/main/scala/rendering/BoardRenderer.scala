package rendering

import java.awt.{Color, Graphics2D, Rectangle, Toolkit}
import scala.math.min
import map_objects._
import game_entities._
import fov_functions._
import cursor._
import scala.collection._

object BoardRenderer {
  val lightfloorColor = new Color(99, 150, 150)
  val lightwallColor = new Color(168, 255, 255)
  val lightupelevatorColor = new Color(100,222,30)
  val lightdownelevatorColor = new Color(185,80,181)
  val lightbrokenelevatorColor = new Color(135,67,8)
  val darkfloorColor = new Color(79, 119, 119)
  val darkwallColor = new Color(134, 204, 204)
  val darkupelevatorColor = new Color(80,202,10)
  val darkdownelevatorColor = new Color(165,60,161)
  val darkbrokenelevatorColor = new Color(115,47,0)
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
          if (fovmap.is_light(x, y)) {
            board.grid(x)(y) match {
              case WallTile() => g.setColor(lightwallColor)
              case FloorTile() =>
                g.setColor(lightfloorColor)
              case UpElevator() =>
                g.setColor(lightupelevatorColor)
              case DownElevator() =>
                g.setColor(lightdownelevatorColor)
              case BrokenElevator() =>
                g.setColor(lightbrokenelevatorColor)
              case _ =>
                println("No match")
                g.setColor(errorColor)
            }
          } else {
            board.grid(x)(y) match {
              case WallTile() => g.setColor(darkwallColor)
              case FloorTile() =>
                g.setColor(darkfloorColor)
              case UpElevator() =>
                g.setColor(darkupelevatorColor)
              case DownElevator() =>
                g.setColor(darkdownelevatorColor)
              case BrokenElevator() =>
                g.setColor(darkbrokenelevatorColor)
              case _ =>
                println("No match")
                g.setColor(errorColor)
            }
          }
          g.fill(buildRect(pos))
        }
      }
    }

    // Draw an entity on the board (if visible)
    def drawEntity(e: GameEntity) = {
      val img = Toolkit.getDefaultToolkit().getImage(e.image)
      g.drawImage(
        img,
        e.pos._1 * (tilePadding + tileSize) + gridOrigin._1,
        e.pos._2 * (tilePadding + tileSize) + gridOrigin._2,
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
          drawEntity(e)
        }
      })
    }

    def drawCursor(): Unit = {
      if (board.cursor.visible) {
        val img = Toolkit.getDefaultToolkit().getImage(board.cursor.image)
        g.drawImage(
          img,
          board.cursor.xpos * (tilePadding + tileSize) + gridOrigin._1,
          board.cursor.ypos * (tilePadding + tileSize) + gridOrigin._2,
          tileSize,
          tileSize,
          null
        )
        g.finalize()
      }
      if (board.cursor.highlightPath && board.cursor.highlightedCells._2) {
        val imgH = Toolkit.getDefaultToolkit().getImage(board.cursor.highlightImage)
        val path = board.cursor.highlightedCells._1
        for (cell <- path) {
          if (fovmap.is_light(cell._1, cell._2)) {
            g.drawImage(
              imgH,
              cell._1 * (tilePadding + tileSize) + gridOrigin._1,
              cell._2 * (tilePadding + tileSize) + gridOrigin._2,
              tileSize,
              tileSize,
              null
            )
            g.finalize()
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
