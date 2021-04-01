package map_objects

import game_entities._
import input_handling._
import items._
import fov_functions._
import logger._
import cursor._

import scala.collection._
import scala.math.{pow, sqrt}
import scala.collection.mutable.PriorityQueue
import scala.util.Random
import java.io._

@SerialVersionUID(10123L)
abstract class GameTile() extends Serializable {
  def blocking: Boolean
  def blocking_sight: Boolean
  var explored = false
  val turnToCross = 1
}

case class FloorTile() extends GameTile {
  def blocking = false
  def blocking_sight = false
}

case class WallTile() extends GameTile {
  def blocking = true
  def blocking_sight = true
}

@SerialVersionUID(123L)
class GameBoard(n: Int, m: Int, val logger: Logger) extends Serializable {
  var size_x = n
  var size_y = m
  val playerEntity = new Player((0, 0), this)
  val cursor = new Cursor(this)
  var itemEntities =
    new mutable.HashMap[(Int, Int), mutable.ArrayBuffer[ItemEntity]]
  var otherEntities = new mutable.HashMap[(Int, Int), GameEntity]
  var grid = MapGenerator.make_empty(size_x, size_y)

  def newMap(
      max_rooms: Int,
      room_min_size: Int,
      room_max_size: Int,
      map_width: Int,
      map_height: Int
  ) {
    val map = MapGenerator.make_map(
      max_rooms,
      room_min_size,
      room_max_size,
      map_width,
      map_height
    )
    grid = map._1
    size_x = map_width
    size_y = map_height
    playerEntity.pos = map._2(0)
    otherEntities = new mutable.HashMap[(Int, Int), GameEntity]
    itemEntities = new mutable.HashMap[(Int, Int), mutable.ArrayBuffer[ItemEntity]]

    // Setup of some entities in order to test the features
    val rnd = new Random
    for { x <- 1 to map._2.size - 1 } {
      rnd.nextInt(4) match {
        case 0 => otherEntities += (map._2(x) -> new Robot(map._2(x), this))
        case 1 => otherEntities += (map._2(x) -> new Dog(map._2(x), this))
        case 2 =>
          if (rnd.nextInt(2) == 1)
            otherEntities += ((map._2(x)._1, map._2(x)._2 + 1) -> new Shopkeeper(
              (map._2(x)._1, map._2(x)._2 + 1),
              this
            ))
          else
            otherEntities += ((map._2(x)._1, map._2(x)._2 + 1) -> new Computer(
              (map._2(x)._1, map._2(x)._2 + 1),
              this
            ))
        case 3 => ()
      }
      rnd.nextInt(6) match {
        case 0 =>
          addItem(new ItemEntity(map._2(x), this, new Morphin), map._2(x))
        case 1 =>
          addItem(new ItemEntity(map._2(x), this, new IronHelmet), map._2(x))
        case 2 =>
          addItem(new ItemEntity(map._2(x), this, new LaserChainsaw), map._2(x))
        case 3 =>
          addItem(new ItemEntity(map._2(x), this, new Bandage), map._2(x))
        case 4 =>
          addItem(new ItemEntity(map._2(x), this, new ArmCannon { upgrade() }), map._2(x))
        case 5 => ()
      }
    }
    // End of test
  }

  def inGrid(pos: (Int, Int)): Boolean =
    pos._1 >= 0 && pos._1 < size_x && pos._2 >= 0 && pos._2 < size_y

  def isFree(pos: (Int, Int)): Boolean =
    inGrid(pos) && (!(grid(pos._1)(pos._2).blocking || otherEntities.contains(pos) || playerEntity.pos == pos))

  def hasEntity(pos: (Int, Int)): Boolean =
    inGrid(pos) && (otherEntities.contains(pos) || playerEntity.pos == pos)

  def hasCharacter(pos: (Int, Int)): Boolean = hasEntity(pos) && getEntity(pos).isInstanceOf[Character]

  def entityMoved(e: GameEntity, newPos: (Int, Int)): Unit = {
    e match {
      case `playerEntity` => ()
      case entity => {
        otherEntities -= entity.pos
        otherEntities += (newPos -> entity)
      }
    }
  }

  def removeEntity(pos: (Int, Int)): Unit = {
    otherEntities -= pos
  }

  def addItem(entity: ItemEntity, pos: (Int, Int)): Boolean = {
    if (!inGrid(pos) || grid(pos._1)(pos._2).blocking)
      return false
    if (!itemEntities.contains(pos))
      itemEntities(pos) = new mutable.ArrayBuffer[ItemEntity]
    itemEntities(pos) += entity
    return true
  }

  def getEntities(): List[GameEntity] = {
    (itemEntities.foldLeft(mutable.MutableList.empty[GameEntity])(
      (
          buf: mutable.MutableList[GameEntity],
          keyAndValue: ((Int, Int), mutable.ArrayBuffer[ItemEntity])
      ) => buf ++= keyAndValue._2
    ) ++= otherEntities.values += playerEntity).toList
  }

  def getEntity(pos: (Int, Int)): GameEntity = {
    if (playerEntity.pos == pos) {
      playerEntity
    } else {
      otherEntities(pos)
    }
  }

  def getCharacter(pos: (Int, Int)): Character = getEntity(pos).asInstanceOf[Character]

  def pickUpItem(pos: (Int, Int), itemIndex: Int): Option[AbstractItem] = {
    if (itemEntities.contains(pos)) {
      if (itemEntities(pos).length > itemIndex) {
        val item = itemEntities(pos)(itemIndex).associatedItem
        itemEntities(pos).remove(itemIndex)
        if (itemEntities(pos).length == 0)
          itemEntities -= pos
        return Some(item)
      } else {
        println("Invalid item index")
        return None
      }
    } else {
      println("No item here")
      return None
    }
  }

  def distance(pos1: (Int, Int), pos2: (Int, Int)): Double = {
    sqrt(pow(pos1._1 - pos2._1, 2) + pow(pos1._2 - pos2._2, 2))
  }

  def shortestPath(s: (Int, Int), f: (Int, Int)): Option[Vector[(Int, Int)]] = { // Based on the A* algorithm
    def order(t: (Double, (Int, Int), (Int, Int))) = -t._1
    val open =
      new PriorityQueue[(Double, (Int, Int), (Int, Int))]()(Ordering.by(order))
    open.enqueue((distance(s, f), s, s))
    val closed = new mutable.HashMap[(Int, Int), (Double, (Int, Int))]
    while (!open.isEmpty) {
      val q = open.dequeue
      for (dir <- Direction.allDirections) {
        val successor = Direction.nextPos(q._2, dir)
        if (successor == f) {
          var path = Vector[(Int, Int)](q._2, f)
          if (q._2 == s) {
            return Some(path)
          }
          path = q._3 +: path
          while (path(0) != s) {
            path = closed(path(0))._2 +: path
          }
          return Some(path)
        }
        if (!grid(q._2._1)(q._2._2).blocking) {
          val w = q._1 + distance(successor, f) - distance(q._2, f) + grid(
            q._2._1
          )(q._2._2).turnToCross + (if (!isFree(successor)) 5 else 0)
          val bSuccessorOpena = open.find(
            _._2 == successor
          ) // We search if the successor is already in open
          var pass = false
          bSuccessorOpena match {
            case Some(b) => if (b._1 <= w) pass = true
            case None    => ()
          }
          if (!pass) {
            if ((!(closed contains successor)) || (closed(successor)._1 > w)) {
              open.enqueue((w, successor, q._2))
            }
          }
        }
      }
      closed += (q._2 -> (q._1, q._3))
    }
    None
  }

  def update(fovmap: FovMap) {
    val entities = getEntities()
    for (e <- entities) {
      if (e.isInstanceOf[AIControlled]) {
        if (fovmap.is_light(e.pos._1, e.pos._2)) {
          e.asInstanceOf[AIControlled].activate()
        }
        if (e.asInstanceOf[AIControlled].active) {
          e.asInstanceOf[AIControlled].act()
        }
      }
    }
    //println("Update all entities")
  }

}
