package generator

import scala.math.{min, max}
import scala.util.Random
import items._
import game_entities._
import map_objects._

object MapGenerator {

  def make_empty(
      map_width: Int,
      map_height: Int,
      defaultTile: GameTile = new FloorTile
  ): Array[Array[GameTile]] = {
    val grid = Array.ofDim[GameTile](map_width, map_height)
    for {
      x <- 0 to map_width - 1
      y <- 0 to map_height - 1
    } defaultTile match {
      case _: FloorTile => grid(x)(y) = new FloorTile
      case _: WallTile  => grid(x)(y) = new WallTile
      case _            => grid(x)(y) = new WallTile
    }
    return grid
  }

  def make_empty_boolean(
      map_width: Int,
      map_height: Int,
  ): Array[Array[Boolean]] = {
    val grid = Array.ofDim[Boolean](map_width, map_height)
    for {
      x <- 0 to map_width - 1
      y <- 0 to map_height - 1
    } 
      grid(x)(y) = false
    return grid
  }

  def make_map(
      max_rooms: Int,
      min_rooms: Int,
      map_width: Int,
      map_height: Int,
      board: GameBoard,
      depth: Int,
      elevatorOnStartingPosition: Boolean
  ): Boolean = {
    val rnd = new Random
    var nbRooms = 1
    board.grid = make_empty(map_width, map_height, new WallTile)
    val modified = make_empty_boolean(map_width, map_height)
    var startingPos = (map_width /3 + rnd.nextInt(map_width/3), map_height /3 + rnd.nextInt(map_height/3))
    var unusedExit = Vector[(Direction.Value,(Int,Int))]()

    def addRoom(room: Room, entrance: (Int,Int)): Boolean = {

      for (p <- room.floorcells) {
        board.grid(entrance._1+p._1)(entrance._2+p._2) = new FloorTile
        modified(entrance._1+p._1)(entrance._2+p._2) = true
      }

      for (p <- room.wallcells) {
        modified(entrance._1+p._1)(entrance._2+p._2) = true
      }

      for ((f,p) <- room.entities) {
        board.grid(entrance._1+p._1)(entrance._2+p._2) = new FloorTile
        board.otherEntities += ((entrance._1+p._1,entrance._2+p._2) -> f((entrance._1+p._1,entrance._2+p._2),board))
        modified(entrance._1+p._1)(entrance._2+p._2) = true
      }

      for ((item,p) <- room.items) {
        board.grid(entrance._1+p._1)(entrance._2+p._2) = new FloorTile
        board.addItem(new ItemEntity((entrance._1+p._1,entrance._2+p._2),board, item), (entrance._1+p._1,entrance._2+p._2))
        modified(entrance._1+p._1)(entrance._2+p._2) = true
      }

      for ((d,p) <- room.possibleExits) {
        unusedExit = (d,(entrance._1+p._1,entrance._2+p._2)) +: unusedExit 
        modified(entrance._1+p._1)(entrance._2+p._2) = true
      }
      return true
    }
    board.lastPosition = startingPos
    board.playerEntity.pos = startingPos
    addRoom(RoomGenerator.generateRoom(depth,None,"arrival"),startingPos)
    if (elevatorOnStartingPosition) {
      board.grid(startingPos._1)(startingPos._2) = new UpElevator
    } else {
      board.grid(startingPos._1)(startingPos._2) = new BrokenElevator
    }
    while (nbRooms <= max_rooms || unusedExit.length != 0) {
      var valid = true
      val i = rnd.nextInt(unusedExit.length)
      val direction = unusedExit(i)._1
      val position = unusedExit(i)._2
      unusedExit = unusedExit.patch(i,Nil,1)
      val delta = direction match {
        case Direction.Up => (0,1)
        case Direction.Right => (1,0)
        case Direction.Down => (0,-1)
        case Direction.Left => (-1,0)
      }
      val newDir = direction match {
        case Direction.Up => Direction.Down
        case Direction.Right => Direction.Left
        case Direction.Down => Direction.Up
        case Direction.Left => Direction.Right
      }
      val newEntrance = (position._1+4*delta._1,position._2+4*delta._2)
      for (j <- 1 to 3) {
        if (modified(position._1+j*delta._1)(position._2+j*delta._2)) {
          valid = false
        }
      }
      if (valid && addRoom(RoomGenerator.generateRoom(depth,Some(newDir),"room"),newEntrance)) {
        board.grid(position._1)(position._2) = new Door
        board.grid(newEntrance._1)(newEntrance._2) = new Door
        for (j <- 1 to 3) {
          board.grid(position._1+j*delta._1)(position._2+j*delta._2) = new FloorTile
          modified(position._1+j*delta._1)(position._2+j*delta._2) = true
        }
        nbRooms = nbRooms + 1
      }
    }
    return (nbRooms >= min_rooms)
  }
}
