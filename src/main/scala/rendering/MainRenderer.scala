package rendering

import swing._
import java.awt.{Graphics2D, Dimension}
import java.awt.Color
import scala.math.{min, max}
import map_objects._
import fov_functions._
import input_handling._
import logger._
import game._

// Uses the different renderers to draw the main window.
object Renderer {
  val bgColor = new Color(48, 99, 99)
  val errorColor = new Color(255, 0, 0)
  val gridOrigin = (10, 10)
  val rightPanelWidth = 370
  val bottomPanelHeight = 80
  val padding = 10

  // Called on paint by the main panel
  def onPaint(
      g: Graphics2D,
      board: GameBoard,
      screenSize: Dimension,
      fovmap: FovMap,
      logger: Logger
  ) {
    fovmap.compute_fov(board.playerEntity.pos._1, board.playerEntity.pos._2)
    g.setColor(bgColor)
    g.fillRect(0, 0, screenSize.width, screenSize.height)

    val drawingAreaWidth = screenSize.width - 2 * padding
    val drawingAreaHeight = screenSize.height - 2 * padding
    val boardSize =
      max(0, min(drawingAreaWidth - rightPanelWidth - padding, drawingAreaHeight - bottomPanelHeight - padding))

    val drawnEntities = BoardRenderer.drawBoard(g, (padding, padding), board, fovmap, boardSize, 20)

    var yNext = SideMenuRenderer.drawPlayerInfo(g, (boardSize + 2 * padding, 2 * padding), board.playerEntity)

    yNext = SideMenuRenderer.drawVisibleEntitiesPanel(g, (boardSize + 2 * padding, yNext + padding), drawnEntities)

    val infos = "Last key pressed : " + UI.last +
      """|
         |Additional commands :
         |- Move with Arrow keys or HJKL and YUBN for the diagonals
         |- When on elevators, move between floors with Q:↑ and W:↓
         |- Move item selector with I:↑ and O:↓
         |- Pick up an item under you with E
         |- Move toward objects/characters to interact with them
         |- Select an item if you want it to be used during an interaction
         |- When equiped with a ranged weapon, shoot with F
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

    if (!UI.menuStack.isEmpty)
      if (UI.menuStack.top.isInstanceOf[MainMenu])
        SplashScreenRenderer.drawMenu(g, (0, 0), (screenSize.width, screenSize.height), UI.menuStack.top, true)
      else
        SplashScreenRenderer.drawMenu(g, (padding, padding), (boardSize / 2, boardSize / 2), UI.menuStack.top)
  }
}
