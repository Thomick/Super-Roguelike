package map_objects

import GameEntities._

import scala.collection._

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
  var size_x = n
  var size_y = m
  val playerEntity = new Player((0, 0), this)
  val otherEntities = new mutable.HashMap[(Int, Int), GameEntity]
  var grid = MapGenerator.make_empty(size_x, size_y)
  // For testing purpose
  def findEmpty(): (Int, Int) = {
    for {
      x <- 0 to size_x - 1
      y <- 0 to size_y - 1
    } {
      if (isFree(x, y)) {
        println("free")
        return (x, y)
      }
    }
    return (0, 0)
  }
  // End of testing

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
    playerEntity.pos = map._2
  }

  def inGrid(pos: (Int, Int)): Boolean = {
    pos._1 >= 0 && pos._1 < size_x && pos._2 >= 0 && pos._2 < size_y
  }

  def isFree(pos: (Int, Int)): Boolean = {
    if (inGrid(pos))
      !(grid(pos._1)(pos._2).blocking || otherEntities.contains(pos))
    else
      false
  }

  def entityMoved(e: GameEntity, newPos: (Int, Int)): Unit = {
    e match {
      case `playerEntity` => println("Player moved")
      case entity => {
        otherEntities -= entity.pos
        otherEntities += (newPos -> entity)
      }
    }
  }

  def getEntities(): List[GameEntity] = {
    (otherEntities.foldLeft(mutable.MutableList.empty[GameEntity])(
      (
          buf: mutable.MutableList[GameEntity],
          keyAndValue: ((Int, Int), GameEntity)
      ) => buf += keyAndValue._2
    ) += playerEntity).toList
  }

  def update(playerDir: Direction.Value) {
    playerEntity.move(playerDir)
  }
}
