import swing._
import event._
import event.Key._
import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle, Toolkit}
import java.io._

import fov_functions._
import map_objects._
import game_entities._
import input_handling._
import rendering._
import logger._
import game._

object Main extends SimpleSwingApplication {
  val ui = new UI
  val logger = new Logger
  var game = new Game(logger)
  //val boardt = new GameBoard(30, 30, logger)
  //boardt.newMap(20, 5, 7, boardt.size_x, boardt.size_y)
  //val oos = new ObjectOutputStream(new FileOutputStream("src/main/resources/test.ser"))
  //oos.writeObject(boardt)
  //oos.close
  //val ois = new ObjectInputStream(new FileInputStream("src/main/resources/test.ser"))
  //val board = ois.readObject.asInstanceOf[GameBoard]
  //ois.close

  var fovmap = new FovMap(game.currentLevel.grid)

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
      ui.applyCommand(game.currentLevel, fovmap)
      repaint
    }
    override def paint(g: Graphics2D) {
      Renderer.onPaint(g, game.currentLevel, ui.last, size, fovmap, ui, logger)
    }
  }
}
