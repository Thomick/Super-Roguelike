import swing._
import event._
import event.Key._
import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle}
import java.awt.{Color => AWTColor}

import map_objects._
import GameEntities._
import render_functions._
import InputHandling._

object Main extends SimpleSwingApplication {

  val ui = new AbstractUI
  val board = new GameBoard(30, 30)
  board.newMap(20, 5, 7, board.size_x, board.size_y)

  def update() {
    board.update(ui)
  }

  def top = new MainFrame {
    title = "Super Roguelike"
    contents = mainPanel
  }
  def mainPanel = new Panel {
    preferredSize = new Dimension(1000, 700)
    focusable = true
    listenTo(keys)
    reactions += { case KeyPressed(_, key, _, _) =>
      ui.newKeyPressed(key)
      update
      repaint
    }
    override def paint(g: Graphics2D) {
      Render.onPaint(g, board, ui.last, size.width, size.height)
    }
  }
}
