package Rendering

import swing._
import java.awt.{Graphics2D, Dimension}
import java.awt.Color
import scala.math.{min, max}
import map_objects._
import fov_functions._
import InputHandling._

class Renderer {
  val bgColor = new Color(48, 99, 99)
  val errorColor = new Color(255, 0, 0)
  val gridOrigin = (10, 10)
  val rightPanelWidth = 200
  val bottomPanelHeight = 0
  val padding = 20

  def onPaint(
      g: Graphics2D,
      board: GameBoard,
      lastkey: String,
      screenSize: Dimension,
      fovmap: FovMap,
      ui: UI
  ) {
    fovmap.compute_fov(board.playerEntity.pos._1, board.playerEntity.pos._2)
    g.setColor(bgColor)
    g.fillRect(0, 0, screenSize.width, screenSize.height)
    val drawingAreaWidth = screenSize.width - 2 * padding
    val drawingAreaHeight = screenSize.height - 2 * padding
    val boardSize = max(
      0,
      min(
        drawingAreaWidth - rightPanelWidth - padding,
        drawingAreaHeight - bottomPanelHeight - padding
      )
    )

    val drawnEntities = BoardRenderer.drawBoard(
      g,
      (padding, padding),
      board,
      fovmap,
      (0, 0),
      boardSize,
      30
    )

    val infos = "Last key pressed : " + ui.lastKey +
      """|
         |Additional commands :
         |- Move with Arrows keys
         |- Move item selector with J:↑ and K:↓
         |- Pick up an item under you with E
         |""".stripMargin

    var yNext = SideMenuRenderer.drawInventory(
      g,
      (boardSize + 2 * padding, padding),
      board.playerEntity,
      ui
    )

    yNext = StringRenderer.drawString(
      g,
      infos,
      (boardSize + 2 * padding, yNext + padding)
    )

    yNext = SideMenuRenderer.drawVisibleEntitiesPanel(
      g,
      (boardSize + 2 * padding, yNext + padding),
      drawnEntities
    )
  }
}
