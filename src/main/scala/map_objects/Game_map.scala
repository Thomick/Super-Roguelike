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

@SerialVersionUID(123L)
class GameBoard(n: Int, m: Int, val logger: Logger) extends Serializable {
  var size_x = n
  var size_y = m
  var playerEntity = new Player((0, 0), this)
  val cursor = new Cursor(this)
  var itemEntities =
    new mutable.HashMap[(Int, Int), mutable.ArrayBuffer[ItemEntity]]
  var otherEntities = new mutable.HashMap[(Int, Int), GameEntity]
  var grid = MapGenerator.make_empty(size_x, size_y)
  var activateElevator = false
  var lastPosition = (0, 0)
  val triggers = new mutable.ArrayBuffer[Trigger]

  def saveLastPosition(): Unit = {
    lastPosition = playerEntity.pos
  }

  def updatePlayer(uPlayer: Player): Unit = {
    playerEntity = uPlayer
    playerEntity.board = this
    playerEntity.pos = lastPosition
  }

  def newMap(
      max_rooms: Int,
      room_min_size: Int,
      room_max_size: Int,
      map_width: Int,
      map_height: Int,
      elevatorOnStartingPosition: Boolean
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
    lastPosition = map._2(0)
    playerEntity.pos = map._2(0)
    if (elevatorOnStartingPosition) {
      grid(map._2(0)._1)(map._2(0)._2) = new UpElevator
    } else {
      grid(map._2(0)._1)(map._2(0)._2) = new BrokenElevator
    }
    otherEntities = new mutable.HashMap[(Int, Int), GameEntity]
    itemEntities = new mutable.HashMap[(Int, Int), mutable.ArrayBuffer[ItemEntity]]

    // Setup of some entities in order to test the features
    val rnd = new Random
    val elevator = rnd.nextInt(map._2.size - 1) + 1
    for { x <- 1 to map._2.size - 1 } {
      if (x == elevator) {
        grid(map._2(x)._1)(map._2(x)._2) = new DownElevator
        otherEntities += (map._2(x) -> new Lock(map._2(x), this))
      } else {
        rnd.nextInt(3) match {
          case 0 => otherEntities += (map._2(x) -> EnemyGenerator.generateEnemy("normal", 0, map._2(x), this))
          case 1 =>
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
          case 2 => ()
        }
        rnd.nextInt(12) match {
          case 0 =>
            addItem(new ItemEntity(map._2(x), this, new Morphin), map._2(x))
          case 1 =>
            addItem(new ItemEntity(map._2(x), this, new IronHelmet), map._2(x))
          case 2 =>
            addItem(new ItemEntity(map._2(x), this, new LaserChainsaw), map._2(x))
          case 3 =>
            addItem(new ItemEntity(map._2(x), this, new Bandage), map._2(x))
          case 4 =>
            addItem(new ItemEntity(map._2(x), this, new ArmCannon), map._2(x))
          case 5 =>
            addItem(new ItemEntity(map._2(x), this, new CowboyHat), map._2(x))
          case 6 =>
            addItem(new ItemEntity(map._2(x), this, new HeavyJacket), map._2(x))
          case 7 =>
            addItem(new ItemEntity(map._2(x), this, new Knuckles), map._2(x))
          case 8 =>
            addItem(new ItemEntity(map._2(x), this, new LaserEyes), map._2(x))
          case 9 =>
            addItem(new ItemEntity(map._2(x), this, new Grenade), map._2(x))
          case 10 =>
            addItem(new ItemEntity(map._2(x), this, new EMPGrenade), map._2(x))
          case 11 => ()
        }
      }
    }
    
    val exterminationTrigger = new DeathTrigger {
      actions += new LogAction("There are no more enemies on this floor.", logger)
    }
    for ((pos, e) <- otherEntities) {
      if (e.isInstanceOf[Enemy])
        exterminationTrigger.addCharacter(e.asInstanceOf[Enemy])
    }
    triggers += exterminationTrigger
    // End of test
  }

  def inGrid(pos: (Int, Int)): Boolean =
    pos._1 >= 0 && pos._1 < size_x && pos._2 >= 0 && pos._2 < size_y

  def isFree(pos: (Int, Int)): Boolean =
    inGrid(pos) && (!(grid(pos._1)(pos._2).blocking || otherEntities.contains(pos) || playerEntity.pos == pos))

  def hasEntity(pos: (Int, Int)): Boolean =
    inGrid(pos) && (otherEntities.contains(pos) || playerEntity.pos == pos)

  def hasCharacter(pos: (Int, Int)): Boolean = hasEntity(pos) && getEntity(pos).isInstanceOf[Character]

  def onUpElevator(pos: (Int, Int)): Boolean = grid(pos._1)(pos._2).isInstanceOf[UpElevator]

  def onDownElevator(pos: (Int, Int)): Boolean = grid(pos._1)(pos._2).isInstanceOf[DownElevator]

  def onBrokenElevator(pos: (Int, Int)): Boolean = grid(pos._1)(pos._2).isInstanceOf[BrokenElevator]

  def entityMoved(e: GameEntity, newPos: (Int, Int)): Unit = {
    if (!e.isInstanceOf[Player]) {
      otherEntities -= e.pos
      otherEntities += (newPos -> e)
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
      //println("No item here")
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

  def oppositeFreeCell(pos1: (Int, Int), pos2: (Int, Int)): Option[(Int, Int)] = {
    val opposite = (pos1._1 - pos2._1, pos1._2 - pos2._2)
    if (isFree(Direction.nextPos(pos1, Direction.giveDirection(opposite)))) { // If the opposite cells is free, we returns it
      return Some(Direction.nextPos(pos1, Direction.giveDirection(opposite)))
    }
    var clockwise = Direction.giveDirection(opposite)
    var counterClockwise = Direction.giveDirection(opposite)
    for (i <- 0 to 1) { // Else, we try to find the most close cell free
      clockwise = Direction.turnClockwise(clockwise)
      counterClockwise = Direction.turnCounterClockwise(counterClockwise)
      if (isFree(Direction.nextPos(pos1, clockwise))) {
        return Some(Direction.nextPos(pos1, clockwise))
      }
      if (isFree(Direction.nextPos(pos1, counterClockwise))) {
        return Some(Direction.nextPos(pos1, counterClockwise))
      }
    } // If there is no adjacent free cell, or if it's near the original cell, we dont return anything
    return None
  }

  def update(fovmap: FovMap) {
    val entities = getEntities()
    for (e <- entities) {
      if (e.isInstanceOf[AIControlled]) {
        if (fovmap.is_light(e.pos._1, e.pos._2) && e.isInstanceOf[Enemy]) {
          e.asInstanceOf[AIControlled].activate()
        }
        if (e.asInstanceOf[AIControlled].active) {
          e.asInstanceOf[AIControlled].act(fovmap.is_light(e.pos._1, e.pos._2))
        }
      }
    }
    for (t <- triggers)
      t.update()
    triggers --= triggers.filter(t => t.triggered)
    println("Update all entities")
  }

  def isTileInteractable(pos: (Int, Int)): Boolean = {
    grid(pos._1)(pos._2).isInstanceOf[InteractableTile]
  }

  def interactWithTile(pos: (Int, Int), c: Character): Unit = {
    grid(pos._1)(pos._2).asInstanceOf[InteractableTile].interact(this, c)
  }

}
