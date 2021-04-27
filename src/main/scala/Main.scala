package main

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
  var logger = new Logger
  var game = new Game(logger)
  var fovmap = new FovMap(game.currentLevel.grid)
  UI.menuStack.push(new MainMenu { items -= (("Save Game", "")) })

  def newGame: Unit = {
    logger = new Logger
    game = new Game(logger)
    fovmap.update(game.currentLevel.grid)
    UI.reset
  }

  def loadGame: Unit = {
    val save = new File("src/main/resources/save.ser")
    if (save.exists) {
      val ois = new ObjectInputStream(new FileInputStream("src/main/resources/save.ser"))
      game = ois.readObject.asInstanceOf[Game]
      ois.close
      logger = game.logger
      fovmap.update(game.currentLevel.grid)
      UI.reset
    }
  }

  def saveGame: Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream("src/main/resources/save.ser"))
    oos.writeObject(game)
    oos.close
  }

  def top = new MainFrame {
    title = "Super Roguelike"
    contents = mainPanel
  }

  val mainPanel = new Panel {
    preferredSize = new Dimension(1100, 700)
    focusable = true
    listenTo(keys)
    reactions += { case KeyPressed(_, key, _, _) =>
      UI.newKeyPressed(key)
      UI.applyCommand(game, fovmap)
      repaint
    }
    override def paint(g: Graphics2D) {
      Renderer.onPaint(g, game.currentLevel, size, fovmap, logger)
    }
  }
}
