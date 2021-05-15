package generator

import scala.math.{min, max, pow, sqrt}
import scala.util.Random
import items._
import game_entities._
import map_objects._

object MapGenerator {

  def distance( pos1: (Int,Int), pos2: (Int, Int)): Double = {
    sqrt(pow(pos1._1 - pos2._1,2) + pow(pos1._2 - pos2._2,2))
  }

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

  def make_map_interior(
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
    var startingPos = ((map_width /3) + rnd.nextInt(map_width/3), (map_height /3) + rnd.nextInt(map_height/3))
    var unusedExit = Vector[(Direction.Value,(Int,Int))]()
    var levers = Vector[Lever]()

    def addLPath(pos1: (Int,Int), pos2: (Int,Int), dir1: Direction.Value, dir2: Direction.Value): Boolean = {
      val delta1 = Direction.giveVector(dir1)
      val delta2 = Direction.giveVector(dir2)
      val diff1 = {
        if (delta1._2 == 0) {
          pos2._1 - pos1._1
        } else {
          pos2._2 - pos1._2
        }
      }
      val diff2 = {
        if (delta1._2 == 0) {
          pos2._2 - pos1._2
        } else {
          pos2._1 - pos1._1
        }
      }
      for (i <- 1 to diff1) {
        if (0 > pos1._1+i*delta1._1 || pos1._1+i*delta1._1 >= map_width || 0 > pos1._2+i*delta1._2 || pos1._2+i*delta1._2 >= map_height || modified(pos1._1+i*delta1._1)(pos1._2+i*delta1._2)) {
          return false
        }
      }
      for (i <- 1 to diff2-1) {
        if (0 > pos2._1+i*delta2._1 || pos2._1+i*delta2._1 >= map_width || 0 > pos2._2+i*delta1._2 || pos2._2+i*delta2._2 >= map_height || modified(pos2._1+i*delta2._1)(pos2._2+i*delta2._2)) {
          return false
        }
      }
      for (i <- 1 to diff1) {
        board.grid(pos1._1+i*delta1._1)(pos1._2+i*delta1._2)= new FloorTile
        modified(pos1._1+i*delta1._1)(pos1._2+i*delta1._2) = true
      }
      for (i <- 1 to diff2-1) {
        board.grid(pos2._1+i*delta2._1)(pos2._2+i*delta2._2)= new FloorTile
        modified(pos2._1+i*delta2._1)(pos2._2+i*delta2._2) = true
      }
      return true
    }
      

    def addZPath(pos1: (Int,Int), pos2: (Int,Int), dir: Direction.Value, check: Boolean): Boolean = {
      val delta1 = Direction.giveVector(dir)
      val diffp = {
        if (delta1._2 == 0) {
          pos2._1 - pos1._1
        } else {
          pos2._2 - pos1._2
        }
      }
      val diffs = {
        if (delta1._2 == 0) {
          pos2._2 - pos1._2
        } else {
          pos2._1 - pos1._1
        }
      }
      val delta2 = {
        if (delta1._1 == 0) {
          if (diffs >= 0) {
            (1,0)
          } else {
            (-1,0)
          }
        } else {
          if (diffs >= 0) {
            (0,1)
          } else {
            (0,-1)
          }
        }
      }
      val midPath = Math.abs(diffp)/2
      for (i <- 1 to midPath) {
        if (check) {
          if (0 > pos1._1+i*delta1._1 || pos1._1+i*delta1._1 >= map_width || 0 > pos1._2+i*delta1._2 || pos1._2+i*delta1._2 >= map_height || modified(pos1._1+i*delta1._1)(pos1._2+i*delta1._2)) {
            return false
          }
        } else {
          board.grid(pos1._1+i*delta1._1)(pos1._2+i*delta1._2)= new FloorTile
          modified(pos1._1+i*delta1._1)(pos1._2+i*delta1._2) = true
        }
      }
      for (i <- 1 to Math.abs(diffs)) {
        if (check) {
          if (0 > pos1._1+midPath*delta1._1+i*delta2._1 || pos1._1+midPath*delta1._1+i*delta2._1 >= map_width || 0 > pos1._2+midPath*delta1._2+i*delta2._2 || pos1._2+midPath*delta1._2+i*delta2._2 >= map_height || modified(pos1._1+midPath*delta1._1+i*delta2._1)(pos1._2+midPath*delta1._2+i*delta2._2)) {
            return false
          }
        } else {
          board.grid(pos1._1+midPath*delta1._1+i*delta2._1)(pos1._2+midPath*delta1._2+i*delta2._2) = new FloorTile
          modified(pos1._1+midPath*delta1._1+i*delta2._1)(pos1._2+midPath*delta1._2+i*delta2._2) = true
        }
      }
      for (i <- midPath to Math.abs(diffp)-1) {
        if (check) {
          if (0 > pos1._1+i*delta1._1+Math.abs(diffs)*delta2._1 || pos1._1+i*delta1._1+Math.abs(diffs)*delta2._1 >= map_width || 0 > pos1._2+i*delta1._2+Math.abs(diffs)*delta2._2 || pos1._2+i*delta1._2+Math.abs(diffs)*delta2._2 >= map_height || modified(pos1._1+i*delta1._1+Math.abs(diffs)*delta2._1)(pos1._2+i*delta1._2+Math.abs(diffs)*delta2._2)) {
            return false
          }
        } else {
          board.grid(pos1._1+i*delta1._1+Math.abs(diffs)*delta2._1)(pos1._2+i*delta1._2+Math.abs(diffs)*delta2._2) = new FloorTile
          modified(pos1._1+i*delta1._1+Math.abs(diffs)*delta2._1)(pos1._2+i*delta1._2+Math.abs(diffs)*delta2._2) = true
        }
      }
      return true
    }

    def addRoom(room: Room, entrance: (Int,Int)): Boolean = {
      for (p <- room.floorcells) {
        if (0 > entrance._1+p._1 || entrance._1+p._1 >= map_width || 0 > entrance._2+p._2 || entrance._2+p._2 >= map_height || modified(entrance._1+p._1)(entrance._2+p._2)) {
          return false
        }
      }

      for (p <- room.wallcells) {
        if (0 > entrance._1+p._1 || entrance._1+p._1 >= map_width || 0 > entrance._2+p._2 || entrance._2+p._2 >= map_height || modified(entrance._1+p._1)(entrance._2+p._2)) {
          return false
        }
      }

      for (p <- room.lockedDoors) {
        if (0 > entrance._1+p._1 || entrance._1+p._1 >= map_width || 0 > entrance._2+p._2 || entrance._2+p._2 >= map_height || modified(entrance._1+p._1)(entrance._2+p._2)) {
          return false
        }
      }

      for (p <- room.levers) {
        if (0 > entrance._1+p._1 || entrance._1+p._1 >= map_width || 0 > entrance._2+p._2 || entrance._2+p._2 >= map_height || modified(entrance._1+p._1)(entrance._2+p._2)) {
          return false
        }
      }

      for (p <- room.locks) {
        if (0 > entrance._1+p._1 || entrance._1+p._1 >= map_width || 0 > entrance._2+p._2 || entrance._2+p._2 >= map_height || modified(entrance._1+p._1)(entrance._2+p._2)) {
          return false
        }
      }

      for (p <- room.elevator) {
        if (0 > entrance._1+p._1 || entrance._1+p._1 >= map_width || 0 > entrance._2+p._2 || entrance._2+p._2 >= map_height || modified(entrance._1+p._1)(entrance._2+p._2)) {
          return false
        }
      }

      for ((f,p) <- room.entities) {
        if (0 > entrance._1+p._1 || entrance._1+p._1 >= map_width || 0 > entrance._2+p._2 || entrance._2+p._2 >= map_height || modified(entrance._1+p._1)(entrance._2+p._2)) {
          return false
        }
      }

      for ((item,p) <- room.items) {
        if (0 > entrance._1+p._1 || entrance._1+p._1 >= map_width || 0 > entrance._2+p._2 || entrance._2+p._2 >= map_height || modified(entrance._1+p._1)(entrance._2+p._2)) {
          return false
        }
      }

      for ((d,p) <- room.possibleExits) {
        if (0 > entrance._1+p._1 || entrance._1+p._1 >= map_width || 0 > entrance._2+p._2 || entrance._2+p._2 >= map_height || modified(entrance._1+p._1)(entrance._2+p._2)) {
          return false
        }
      }

      for (p <- room.floorcells) {
        board.grid(entrance._1+p._1)(entrance._2+p._2) = new FloorTile
        modified(entrance._1+p._1)(entrance._2+p._2) = true
      }

      for (p <- room.wallcells) {
        modified(entrance._1+p._1)(entrance._2+p._2) = true
      }

      for (p <- room.levers) {
        val lever = new Lever((entrance._1+p._1,entrance._2+p._2),board)
        board.grid(entrance._1+p._1)(entrance._2+p._2) = new FloorTile
        board.otherEntities += ((entrance._1+p._1,entrance._2+p._2) -> lever)
        modified(entrance._1+p._1)(entrance._2+p._2) = true
        levers = lever +: levers
      }

      for (p <- room.lockedDoors) {
        val lockedDoor = new TriggerableDoor
        board.grid(entrance._1+p._1)(entrance._2+p._2) = lockedDoor
        modified(entrance._1+p._1)(entrance._2+p._2) = true
        board.triggers += new Trigger {
          actions += lockedDoor
          events += new ActivableWatcher (levers(0))
        }
        levers = levers.patch(0,Nil,1)

      }

      for (p <- room.elevators) {
        board.grid(entrance._1+p._1)(entrance._2+p._2) = new DownElevator
        modified(entrance._1+p._1)(entrance._2+p._2) = true
      }

      for (p <- room.locks) {
        board.grid(entrance._1+p._1)(entrance._2+p._2) = new DownElevator
        board.otherEntities += ((entrance._1+p._1,entrance._2+p._2) -> new Lock((entrance._1+p._1,entrance._2+p._2),board)
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
      }
      return true
    }
    board.lastPosition = startingPos
    board.playerEntity.pos = startingPos
    addRoom(RoomGenerator.generateRoom(depth,Direction.Nop,"arrival"),startingPos)
    if (elevatorOnStartingPosition) {
      board.grid(startingPos._1)(startingPos._2) = new UpElevator
    } else {
      board.grid(startingPos._1)(startingPos._2) = new BrokenElevator
    }
    while (nbRooms <= max_rooms && unusedExit.length != 0) {
      var valid = true
      val i = rnd.nextInt(unusedExit.length)
      val direction = unusedExit(i)._1
      val position = unusedExit(i)._2
      unusedExit = unusedExit.patch(i,Nil,1)
      val delta1 = Direction.giveVector(direction)
      val delta2 = Direction.giveVector(Direction.turnClockwise(Direction.turnClockwise(direction)))
      val newDir = Direction.oppositeDirection(direction)
      var file = "room"
      rnd.nextInt(5) match {
        case 0 => file = "lever"
        case 1 => if (levers.size != 0) {
          file = "treasureRooms"
        }
        case _ => ()
      }
      val newEntrance = (position._1+(1+rnd.nextInt(6))*delta1._1+(4-rnd.nextInt(9))*delta2._1,position._2+(1+rnd.nextInt(6))*delta1._2+(4-rnd.nextInt(9))*delta2._2)
      valid = addZPath(position,newEntrance,direction,true)
      if (valid && addRoom(RoomGenerator.generateRoom(depth,newDir,file),newEntrance)) {
        board.grid(position._1)(position._2) = new Door
        board.grid(newEntrance._1)(newEntrance._2) = new Door
        addZPath(position,newEntrance,direction,false)
        nbRooms = nbRooms + 1
      } else {
        var exitPosition = (0,0)
        var exitDir = Direction.Nop
        var exitIndex = 0
        for ( ((d,p),index) <- unusedExit.zipWithIndex ) {
          if ( distance(p,position) < 5 && d != direction) {
            exitIndex = index
            exitPosition = p
            exitDir = d
          }
        }
        if (exitDir != Direction.Nop) {
          if (exitDir == Direction.oppositeDirection(direction)) {
            valid = addZPath(position,exitPosition,direction,true)
            if (valid) addZPath(position,exitPosition,direction,false)
          } else {
            valid = addLPath(position,exitPosition,direction,exitDir)
          }
          if (valid) {
            board.grid(position._1)(position._2) = new Door
            board.grid(exitPosition._1)(exitPosition._2) = new Door
            unusedExit = unusedExit.patch(exitIndex,Nil,1)
          }
        }
      }
    }
    return (nbRooms >= min_rooms)
  }
}
