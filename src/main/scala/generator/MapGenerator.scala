package generator

import scala.math.{min, max, pow, sqrt, abs, acos}
import scala.util.Random
import items._
import game_entities._
import map_objects._

object MapGenerator {

  def distance(pos1: (Int, Int), pos2: (Int, Int)): Double = {
    sqrt(pow(pos1._1 - pos2._1, 2) + pow(pos1._2 - pos2._2, 2))
  }

  def checkAngle(pos1: (Int,Int), pos2: (Int,Int), dir: Direction.Value): Boolean = {// Checks if the angle pos2 pos1 pos3 is inferior to pi/2, where pos3 is pos1 moved by one in the direction of dir.
    val delta = Direction.giveVector(dir)
    val pos3 = (pos1._1 + delta._1, pos1._2 + delta._2)
    val p12 = distance(pos1,pos2) 
    val p23 = distance(pos2,pos3) 
    val p13 = distance(pos1,pos3) 
    return (abs(acos((p12*p12+p13*p13-p23*p23)/(2*p12*p13))) < 1.571)
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
      map_height: Int
  ): Array[Array[Boolean]] = {
    val grid = Array.ofDim[Boolean](map_width, map_height)
    for {
      x <- 0 to map_width - 1
      y <- 0 to map_height - 1
    } grid(x)(y) = false
    return grid
  }

  def make_map_interior(
      nbRooms: Int,
      map_width: Int,
      map_height: Int,
      board: GameBoard,
      depth: Int,
      elevatorOnStartingPosition: Boolean
  ): Boolean = {
    val rnd = new Random
    var countRooms = 1
    board.grid = make_empty(map_width, map_height, new WallTile)
    val modified = make_empty_boolean(map_width, map_height)
    var startingPos = ((map_width / 3) + rnd.nextInt(map_width / 3), (map_height / 3) + rnd.nextInt(map_height / 3))
    var unusedExit = Vector[(Direction.Value, (Int, Int))]()
    var levers = Vector[Lever]()

    def addLPath(pos1: (Int, Int), pos2: (Int, Int), dir1: Direction.Value, dir2: Direction.Value): Boolean = {
      val delta1 = Direction.giveVector(dir1)
      val delta2 = Direction.giveVector(dir2)
      val diff1 = {
        if (delta1._2 == 0) {
          abs(pos2._1 - pos1._1)
        } else {
          abs(pos2._2 - pos1._2)
        }
      }
      val diff2 = {
        if (delta1._2 == 0) {
          abs(pos2._2 - pos1._2)
        } else {
          abs(pos2._1 - pos1._1)
        }
      }
      for (i <- 1 to diff1) {
        if (0 > pos1._1 + i * delta1._1 || pos1._1 + i * delta1._1 >= map_width || 0 > pos1._2 + i * delta1._2 || pos1._2 + i * delta1._2 >= map_height || modified(pos1._1 + i * delta1._1)(pos1._2 + i * delta1._2)) {
          return false
        }
      }
      for (i <- 1 to diff2 - 1) {
        if (0 > pos2._1 + i * delta2._1 || pos2._1 + i * delta2._1 >= map_width || 0 > pos2._2 + i * delta1._2 || pos2._2 + i * delta2._2 >= map_height || modified(pos2._1 + i * delta2._1)(pos2._2 + i * delta2._2)) {
          return false
        }
      }
      for (i <- 1 to diff1) {
        board.grid(pos1._1 + i * delta1._1)(pos1._2 + i * delta1._2) = new FloorTile
        modified(pos1._1 + i * delta1._1)(pos1._2 + i * delta1._2) = true
      }
      for (i <- 1 to diff2 - 1) {
        board.grid(pos2._1 + i * delta2._1)(pos2._2 + i * delta2._2) = new FloorTile
        modified(pos2._1 + i * delta2._1)(pos2._2 + i * delta2._2) = true
      }
      return true
    }

    def addZPath(pos1: (Int, Int), pos2: (Int, Int), dir: Direction.Value, check: Boolean): Boolean = {
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
            (1, 0)
          } else {
            (-1, 0)
          }
        } else {
          if (diffs >= 0) {
            (0, 1)
          } else {
            (0, -1)
          }
        }
      }
      val midPath = Math.abs(diffp) / 2
      for (i <- 1 to midPath) {
        if (check) {
          if (0 > pos1._1 + i * delta1._1 || pos1._1 + i * delta1._1 >= map_width || 0 > pos1._2 + i * delta1._2 || pos1._2 + i * delta1._2 >= map_height || modified(pos1._1 + i * delta1._1)(pos1._2 + i * delta1._2)) {
            return false
          }
        } else {
          board.grid(pos1._1 + i * delta1._1)(pos1._2 + i * delta1._2) = new FloorTile
          modified(pos1._1 + i * delta1._1)(pos1._2 + i * delta1._2) = true
        }
      }
      for (i <- 1 to Math.abs(diffs)) {
        if (check) {
          if (0 > pos1._1 + midPath * delta1._1 + i * delta2._1 || pos1._1 + midPath * delta1._1 + i * delta2._1 >= map_width || 0 > pos1._2 + midPath * delta1._2 + i * delta2._2 || pos1._2 + midPath * delta1._2 + i * delta2._2 >= map_height || modified(pos1._1 + midPath * delta1._1 + i * delta2._1)(pos1._2 + midPath * delta1._2 + i * delta2._2)) {
            return false
          }
        } else {
          board.grid(pos1._1 + midPath * delta1._1 + i * delta2._1)(pos1._2 + midPath * delta1._2 + i * delta2._2) = new FloorTile
          modified(pos1._1 + midPath * delta1._1 + i * delta2._1)(pos1._2 + midPath * delta1._2 + i * delta2._2) = true
        }
      }
      for (i <- midPath to Math.abs(diffp) - 1) {
        if (check) {
          if (0 > pos1._1 + i * delta1._1 + Math.abs(diffs) * delta2._1 || pos1._1 + i * delta1._1 + Math.abs(diffs) * delta2._1 >= map_width || 0 > pos1._2 + i * delta1._2 + Math.abs(diffs) * delta2._2 || pos1._2 + i * delta1._2 + Math.abs(diffs) * delta2._2 >= map_height || modified(pos1._1 + i * delta1._1 + Math.abs(diffs) * delta2._1)(pos1._2 + i * delta1._2 + Math.abs(diffs) * delta2._2)) {
            return false
          }
        } else {
          board.grid(pos1._1 + i * delta1._1 + Math.abs(diffs) * delta2._1)(pos1._2 + i * delta1._2 + Math.abs(diffs) * delta2._2) = new FloorTile
          modified(pos1._1 + i * delta1._1 + Math.abs(diffs) * delta2._1)(pos1._2 + i * delta1._2 + Math.abs(diffs) * delta2._2) = true
        }
      }
      return true
    }

    def addRoom(room: Room, entrance: (Int, Int)): Boolean = {
      def checkInGrid(p: (Int, Int)): Boolean = {
        0 <= entrance._1 + p._1 && entrance._1 + p._1 < map_width && 0 <= entrance._2 + p._2 && entrance._2 + p._2 < map_height && !modified(entrance._1 + p._1)(entrance._2 + p._2)
      }

      def checkInGridArr(arr: Vector[(Int, Int)]): Boolean = {
        for (p <- arr)
          if (!checkInGrid(p))
            return false
        return true
      }
      def checkInGridArr2(arr: Vector[(Any, (Int, Int))]): Boolean = {
        for ((_, p) <- arr)
          if (!checkInGrid(p))
            return false
        return true
      }

      if (!(checkInGridArr(room.floorcells) && checkInGridArr(room.wallcells) && checkInGridArr(room.lockedDoors) && checkInGridArr(room.levers) && checkInGridArr(room.locks) && checkInGridArr(room.elevators)))
        return false

      if (!(checkInGridArr2(room.entities) && checkInGridArr2(room.items) && checkInGridArr2(room.possibleExits)))
        return false

      def changeTile(p: (Int, Int), newTile: GameTile): Unit = {
        board.grid(entrance._1 + p._1)(entrance._2 + p._2) = newTile
        modified(entrance._1 + p._1)(entrance._2 + p._2) = true
      }

      for (p <- room.floorcells)
        changeTile(p, new FloorTile)

      for (p <- room.wallcells)
        modified(entrance._1 + p._1)(entrance._2 + p._2) = true

      for (p <- room.levers) {
        val lever = new Lever((entrance._1 + p._1, entrance._2 + p._2), board)
        changeTile(p, new FloorTile)
        board.otherEntities += ((entrance._1 + p._1, entrance._2 + p._2) -> lever)
        levers = lever +: levers
      }

      if (room.bosses.size > 0) {
        var bosses = Vector[GameEntity]()
        for ((f, p) <- room.bosses) {
          val boss = f((entrance._1 + p._1, entrance._2 + p._2), board)
          changeTile(p, new FloorTile)
          board.otherEntities += ((entrance._1 + p._1, entrance._2 + p._2) -> boss)
          bosses = boss +: bosses
        }
        for (p <- room.lockedDoors) {
          val lockedDoor = new TriggerableDoor
          changeTile(p, lockedDoor)
          board.triggers += new Trigger {
            actions += lockedDoor
            actions += new LogAction("You have defeated the boss ! The door is now unlocked.", board.logger)
            for (boss <- bosses) {
              events += new DeathWatcher(boss.asInstanceOf[Enemy])
            }
          }
        }
      } else {
        for (p <- room.lockedDoors) {
          val lockedDoor = new TriggerableDoor
          changeTile(p, lockedDoor)
          if (levers.size > 0) {
            board.triggers += new Trigger {
              actions += lockedDoor
              actions += new LogAction("The door is now unlocked.", board.logger)
              events += new ActivableWatcher(levers(0))
            }
            levers = levers.patch(0, Nil, 1)
          } else {
            board.triggers += new Trigger {
              actions += lockedDoor
              actions += new LogAction("The door is now unlocked.", board.logger)
              for ((pos, e) <- board.otherEntities) {
                if (e.isInstanceOf[Enemy]) {
                  events += new DeathWatcher(e.asInstanceOf[Enemy])
                }
              }
            }
          }
        }
      }

      for (p <- room.elevators)
        changeTile(p, new DownElevator)

      for (p <- room.locks) {
        changeTile(p, new DownElevator)
        board.otherEntities += ((entrance._1 + p._1, entrance._2 + p._2) -> new Lock((entrance._1 + p._1, entrance._2 + p._2), board))
      }

      for ((f, p) <- room.entities) {
        changeTile(p, new FloorTile)
        board.otherEntities += ((entrance._1 + p._1, entrance._2 + p._2) -> f((entrance._1 + p._1, entrance._2 + p._2), board))
      }

      for ((item, p) <- room.items) {
        changeTile(p, new FloorTile)
        board.addItem(new ItemEntity((entrance._1 + p._1, entrance._2 + p._2), board, item), (entrance._1 + p._1, entrance._2 + p._2))
      }

      for ((d, p) <- room.possibleExits) {
        unusedExit = (d, (entrance._1 + p._1, entrance._2 + p._2)) +: unusedExit
      }
      return true
    }
    board.lastPosition = startingPos
    board.playerEntity.pos = startingPos
    addRoom(RoomGenerator.generateRoom(depth, Direction.Nop, "arrival"), startingPos)
    if (elevatorOnStartingPosition) {
      board.grid(startingPos._1)(startingPos._2) = new UpElevator
    } else {
      board.grid(startingPos._1)(startingPos._2) = new BrokenElevator
    }
    while (countRooms <= nbRooms && unusedExit.length != 0) {
      var valid = true
      val i = rnd.nextInt(unusedExit.length)
      val direction = unusedExit(i)._1
      val position = unusedExit(i)._2
      unusedExit = unusedExit.patch(i, Nil, 1)
      val delta1 = Direction.giveVector(direction)
      val delta2 = Direction.giveVector(Direction.turnClockwise(Direction.turnClockwise(direction)))
      val newDir = Direction.oppositeDirection(direction)
      val file = if (countRooms == nbRooms) {
        rnd.nextInt(4) match {
          //case 0 => "boss"
          case 1 => "exitLock"
          case _ => "exitDoor"
        }
      } else {
        rnd.nextInt(5) match {
          case 0 => "lever"
          case 1 =>
            if (levers.size != 0) {
              "treasureRooms"
            } else {
              "room"
            }
          case _ => "room"
        }
      }
      val newEntrance = (position._1 + (1 + rnd.nextInt(6)) * delta1._1 + (4 - rnd.nextInt(9)) * delta2._1, position._2 + (1 + rnd.nextInt(6)) * delta1._2 + (4 - rnd.nextInt(9)) * delta2._2)
      valid = addZPath(position, newEntrance, direction, true)
      if (valid && addRoom(RoomGenerator.generateRoom(depth, newDir, file), newEntrance)) {
        board.grid(position._1)(position._2) = new Door
        board.grid(newEntrance._1)(newEntrance._2) = new Door
        addZPath(position, newEntrance, direction, false)
        countRooms += 1
      } else {
        var exitPosition = (0, 0)
        var exitDir = Direction.Nop
        var exitIndex = 0
        for (((d, p), index) <- unusedExit.zipWithIndex) {
          if (distance(p, position) < 5 && d != direction && checkAngle(position, p, direction)) {
            exitIndex = index
            exitPosition = p
            exitDir = d
          }
        }
        if (exitDir != Direction.Nop) {
          if (exitDir == Direction.oppositeDirection(direction)) {
            valid = addZPath(position, exitPosition, direction, true)
            if (valid) addZPath(position, exitPosition, direction, false)
          } else {
            valid = addLPath(position, exitPosition, direction, exitDir)
          }
          if (valid) {
            board.grid(position._1)(position._2) = new Door
            board.grid(exitPosition._1)(exitPosition._2) = new Door
            unusedExit = unusedExit.patch(exitIndex, Nil, 1)
          }
        }
      }
    }
    return (countRooms >= nbRooms)
  }
}
