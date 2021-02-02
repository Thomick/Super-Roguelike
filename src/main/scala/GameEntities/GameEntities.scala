package GameEntities

import map_objects._
import Items._

import java.awt.{Color => AWTColor}
import scala.collection._

object Direction extends Enumeration {
  val Up, Down, Left, Right, Nop = Value
  def nextPos(pos: (Int, Int), dir: Direction.Value): (Int, Int) = {
    dir match {
      case Direction.Left  => (pos._1 - 1, pos._2)
      case Direction.Right => (pos._1 + 1, pos._2)
      case Direction.Up    => (pos._1, pos._2 - 1)
      case Direction.Down  => (pos._1, pos._2 + 1)
      case Direction.Nop   => pos
    }
  }
}

abstract class GameEntity(init_pos: (Int, Int), b: GameBoard) {
  val name: String
  val description: String
  var pos: (Int, Int) = init_pos
  val color: AWTColor
  val board: GameBoard = b
  val image: String = "src/main/resources/placeholder.png"
}

abstract class Character(init_pos: (Int, Int), b: GameBoard)
    extends GameEntity(init_pos, b) {

  val inventory = mutable.ArrayBuffer[AbstractItem]()
  def move(dir: Direction.Value): Unit = {
    var nextPos = Direction.nextPos(pos, dir)
    if (board.isFree(nextPos)) {
      board.entityMoved(this, nextPos)
      pos = nextPos
    }
  }
  def obtainItem(item: AbstractItem) = {
    inventory += item
    println("Got Item")
  }
  def destroyItem(item: AbstractItem) = {
    inventory -= item
  }
}

class Player(init_pos: (Int, Int), b: GameBoard)
    extends Character(init_pos, b) {
  val name = "Player"
  val description = "It's you !"
  val color = new AWTColor(100, 255, 100)
  override val image = "src/main/resources/hero.png"

  def pickUp(dir: Direction.Value): Boolean = {
    val itemPos = Direction.nextPos(pos, dir)
    board.pickItem(itemPos) match {
      case None => {
        println("No item to pickup")
        false
      }
      case Some(item) => {
        obtainItem(item)
        true
      }
    }
  }

  def throwItem(itemSlot: Int, dir: Direction.Value): Boolean = {
    if (itemSlot < inventory.length) {
      val item = inventory(itemSlot)
      if (item.isInstanceOf[Throwable]) {
        item.asInstanceOf[Throwable].throwItem(this, board, pos, dir)
        return true
      } else {
        println("This item can't be thrown")
        return false
      }
    } else {
      println("There is no item in this slot")
      return false
    }
  }

  def consumeItem(itemSlot: Int): Boolean = {
    if (itemSlot < inventory.length) {
      val item = inventory(itemSlot)
      if (item.isInstanceOf[Consumable]) {
        item.asInstanceOf[Consumable].consume(this)
      } else {
        println("This item can't be consumed")
        return false
      }
    } else {
      println("There is no item in this slot")
      return false
    }
  }
}

class Rock(init_pos: (Int, Int), b: GameBoard) extends GameEntity(init_pos, b) {
  val name = "A Rock"
  val description = "A Big Rock"
  val color = new AWTColor(200, 200, 200)
}
