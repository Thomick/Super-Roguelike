import swing._
import event._
import event.Key._
import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle, Toolkit}

import fov_functions._
import map_objects._
import GameEntities._
import InputHandling._
import Rendering._
import logger._

object Main extends SimpleSwingApplication {
  val ui = new UI
  val board = new GameBoard(30, 30)
  board.newMap(20, 5, 7, board.size_x, board.size_y)
  var fovmap = new FovMap(board.grid)
  val logger = new Logger

  def top = new MainFrame {
    title = "Super Roguelike"
    contents = mainPanel
  }
  def mainPanel = new Panel {
    preferredSize = new Dimension(1000, 750)
    focusable = true
    listenTo(keys)
    reactions += { case KeyPressed(_, key, _, _) =>
      ui.newKeyPressed(key)
      ui.applyCommand(board,fovmap)
      repaint
    }
    override def paint(g: Graphics2D) {
      Renderer.onPaint(g, board, ui.last, size, fovmap, ui, logger)
    }
  }
}
