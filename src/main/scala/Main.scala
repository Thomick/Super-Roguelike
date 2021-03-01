import swing._
import event._
import event.Key._
import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle, Toolkit}

import fov_functions._
import map_objects._
import game_entities._
import input_handling._
import rendering._
import logger._

object Main extends SimpleSwingApplication {
  val ui = new UI
  val logger = new Logger
  val board = new GameBoard(30, 30, logger)
  board.newMap(20, 5, 7, board.size_x, board.size_y)
  var fovmap = new FovMap(board.grid)

  def top = new MainFrame {
    title = "Super Roguelike"
    contents = mainPanel
  }
  def mainPanel = new Panel {
    preferredSize = new Dimension(1100, 700)
    focusable = true
    listenTo(keys)
    reactions += { case KeyPressed(_, key, _, _) =>
      ui.newKeyPressed(key)
      ui.applyCommand(board, fovmap)
      repaint
    }
    override def paint(g: Graphics2D) {
      Renderer.onPaint(g, board, ui.last, size, fovmap, ui, logger)
    }
  }
}
