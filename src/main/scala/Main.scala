import swing._
import event._
import event.Key._
import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle, Toolkit}
import java.awt.{Color => AWTColor}

import fov_functions._
import map_objects._
import GameEntities._
import render_functions._
import InputHandling._

object Main extends SimpleSwingApplication {

  val ui = new AbstractUI
  val board = new GameBoard(30, 30)
  val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
  board.newMap(20, 5, 7, board.size_x, board.size_y)
  var fovmap = new FovMap(board.grid)

  def top = new MainFrame {
    title = "Super Roguelike"
    contents = mainPanel
  }
  def mainPanel = new Panel {
    preferredSize = screenSize
    focusable = true
    listenTo(keys)
    reactions += { case KeyPressed(_, key, _, _) =>
      ui.newKeyPressed(key)
      ui.applyCommand(board)
      repaint
    }
    override def paint(g: Graphics2D) {
      Render.onPaint(g, board, ui.last, size.width, size.height, fovmap)
    }
  }
}
