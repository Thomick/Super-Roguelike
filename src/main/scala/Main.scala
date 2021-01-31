import swing._
import event._
import event.Key._
import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle}
import java.awt.{Color => AWTColor}

object Direction extends Enumeration {
  val Up, Down, Left, Right = Value
}

abstract class GameEntity(init_pos: (Int, Int), b: GameBoard) {
  var name: String
  var description: String
  var pos: (Int, Int) = init_pos
  var color: AWTColor
  var board: GameBoard = b
}

abstract class Character(init_pos: (Int, Int), b: GameBoard)
    extends GameEntity(init_pos, b) {

  def move(dir: Direction.Value): Unit = {
    var nextPos = (0, 0)
    dir match {
      case Direction.Left  => nextPos = (pos._1 - 1, pos._2)
      case Direction.Right => nextPos = (pos._1 + 1, pos._2)
      case Direction.Up    => nextPos = (pos._1, pos._2 - 1)
      case Direction.Down  => nextPos = (pos._1, pos._2 + 1)
    }
    board.isFree(nextPos)
    pos = nextPos
    board.entityMoved(this)
  }
}

class Player(init_pos: (Int, Int), b: GameBoard)
    extends Character(init_pos, b) {
  var name = "Player"
  var description = "It's you !"
  var color = new AWTColor(100, 255, 100)

}

abstract class GameTile() {
  def blocking: Boolean
}
case class FloorTile() extends GameTile {
  def blocking = false
}
case class WallTile() extends GameTile {
  def blocking = true
}

class GameBoard(n: Int, m: Int) {
  val size_x = n
  val size_y = m
  var grid = Array.ofDim[GameTile](size_x, size_y)
  for {
    x <- 0 to size_x - 1
    y <- 0 to size_y - 1
    val pos = (x, y)
  } grid(x)(y) = new FloorTile()

  def isFree(pos: (Int, Int)): Boolean = {
    grid(pos._1)(pos._2) match {
      case FloorTile() => true
      case WallTile()  => false
    }
  }

  def entityMoved(e: GameEntity): Unit = {
    println("Nothing happen for now")
  }
}

class AbstractUI {
  var lastKey: String = ""
  def left() {
    lastKey = "left"
  }
  def right() {
    lastKey = "right"
  }
  def up() {
    lastKey = "up"
  }
  def down() {
    lastKey = "down"
  }
  def last: String = lastKey
}

object Main extends SimpleSwingApplication {

  val bgColor = new AWTColor(48, 99, 99)
  val writeColor = new AWTColor(200, 200, 200)
  val floorColor = new AWTColor(99, 150, 150)
  val wallColor = new AWTColor(255, 255, 255)
  val errorColor = new AWTColor(255, 0, 0)
  val ui = new AbstractUI
  val tileSize = 20
  val tileOffset = 1
  val gridOrigin = (10, 10)
  val board = new GameBoard(30, 30)

  def onKeyPress(keyCode: Value) = keyCode match {
    case Up    => ui.up()
    case Down  => ui.down()
    case Left  => ui.left()
    case Right => ui.right()
  }
  def onPaint(g: Graphics2D) {
    g setColor writeColor
    g drawString (ui.last, gridOrigin._1 + board.size_x * (tileOffset + tileSize) + 20, 20)

    def buildRect(pos: (Int, Int)): Rectangle =
      new Rectangle(
        gridOrigin._1 + pos._1 * (tileSize + tileOffset),
        gridOrigin._2 + pos._2 * (tileSize + tileOffset),
        tileSize,
        tileSize
      )

    def drawGrid() =
      for {
        x <- 0 to board.size_x - 1
        y <- 0 to board.size_y - 1
        val pos = (x, y)
      } {
        board.grid(x)(y) match {
          case WallTile() => g setColor wallColor
          case FloorTile() =>
            g setColor floorColor
          case _ =>
            println("No match")
            g setColor errorColor
        }
        g fill buildRect(pos)
      }
    drawGrid()
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
      onKeyPress(key)
      repaint
    }
    override def paint(g: Graphics2D) {
      g setColor bgColor
      g fillRect (0, 0, size.width, size.height)
      onPaint(g)
    }
  }
}
