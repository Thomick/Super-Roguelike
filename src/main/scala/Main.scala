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
  val logger = new Logger
  var game = new Game(logger)

  var fovmap = new FovMap(game.currentLevel.grid)

  def loadGame: Game = {
    val ois = new ObjectInputStream(new FileInputStream("src/main/resources/save.ser"))
    val gameLoaded = ois.readObject.asInstanceOf[Game]
    ois.close
    return gameLoaded
  }

  val newGameB = new Button("New Game")
  val loadGameB = new Button("Load Game")
  val gridPanel = new GridPanel(1, 2) {
    contents += newGameB
    contents += loadGameB
  }

  def top = new MainFrame {
    title = "Super Roguelike"
    contents = gridPanel
    preferredSize = new Dimension(1100, 700)
    listenTo(newGameB)
    listenTo(loadGameB)
    reactions += {
      case ButtonClicked(source) if source == newGameB =>
        game.saveGame
        contents = mainPanel
      case ButtonClicked(source) if source == loadGameB =>
        game = loadGame
        fovmap.update(game.currentLevel.grid)
        contents = mainPanel
    }
  }
  def mainPanel = new Panel {
    focusable = true
    listenTo(keys)
    reactions += { case KeyPressed(_, key, _, _) =>
      UI.newKeyPressed(key)
      UI.applyCommand(game.currentLevel, fovmap)
      repaint
    }
    override def paint(g: Graphics2D) {
      Renderer.onPaint(g, game.currentLevel, size, fovmap, logger)
    }
  }
}
