package Rendering

import swing._
import java.awt.{Graphics2D, Dimension}
import java.awt.Color
import scala.math.{min, max}
import map_objects._
import fov_functions._
import InputHandling._
import logger._

// Uses the different renderers to draw the main window.
object Renderer {
  val bgColor = new Color(48, 99, 99)
  val errorColor = new Color(255, 0, 0)
  val gridOrigin = (10, 10)
  val rightPanelWidth = 200
  val bottomPanelHeight = 50
  val padding = 10

  // Called on paint by the main panel
  def onPaint(
      g: Graphics2D,
      board: GameBoard,
      lastkey: String,
      screenSize: Dimension,
      fovmap: FovMap,
      ui: UI,
      logger: Logger
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

    var yNext = SideMenuRenderer.drawPlayerInfo(
      g,
      (boardSize + 2 * padding, 2 * padding),
      board.playerEntity,
      ui
    )

    yNext = SideMenuRenderer.drawVisibleEntitiesPanel(
      g,
      (boardSize + 2 * padding, yNext + padding),
      drawnEntities
    )

    val infos = "Last key pressed : " + ui.lastKey +
      """|
         |Additional commands :
         |- Move with Arrows keys
         |- Move item selector with J:↑ and K:↓
         |- Pick up an item under you with E
         |""".stripMargin

    yNext = StringRenderer.drawString(
      g,
      infos,
      (boardSize + 2 * padding, yNext + padding)
    )

    StringRenderer.drawIterable(
      g,
      logger.getLogs,
      (padding, boardSize + 3 * padding)
    )
  }
}
