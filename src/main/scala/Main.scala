import swing._
import event._
import event.Key._
import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle}
import java.awt.{Color => AWTColor}

import map_objects._
import GameEntities._
import render_functions._

class AbstractUI {
  var lastKey: String = ""
  var lastIsMove: Boolean = false
  var lastDir: Direction.Value = Direction.Nop
  def newKeyPressed(keyCode: Value) = {
    keyCode match {
      case Up => {
        lastDir = Direction.Up
        lastIsMove = true
        lastKey = "Up"
      }
      case Down => {
        lastDir = Direction.Down
        lastIsMove = true
        lastKey = "Down"
      }
      case Left => {
        lastDir = Direction.Left
        lastIsMove = true
        lastKey = "Left"
      }
      case Right => {
        lastDir = Direction.Right
        lastIsMove = true
        lastKey = "Right"
      }
      case _ => {}
    }
    lastKey = keyCode.toString
  }
  def last: String = lastKey
}

object Main extends SimpleSwingApplication {

  val ui = new AbstractUI
  val board = new GameBoard(30, 30)
  board.newMap(20, 5, 7, board.size_x, board.size_y)

  def update() {
    if (ui.lastIsMove) {
      board.update(ui.lastDir)
    }
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
      Render.onPaint(g,board,ui.last,size.width,size.height)
    }
  }
}
