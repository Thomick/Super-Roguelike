package map_objects

import scala.math.{min, max}
import scala.util.Random
import items._
import game_entities._

class Rect(x: Int, y: Int, w: Int, h: Int) {
  val x1 = x
  val y1 = y
  val x2 = x + w
  val y2 = y + h

  def center() = {
    val center_x = (x1 + x2) / 2
    val center_y = (y1 + y2) / 2
    (center_x, center_y)
  }

  def intersect(rect2: Rect) = {
    (x1 <= rect2.x2 && x2 >= rect2.x1 && y1 <= rect2.y2 && y2 >= rect2.y1)
  }
}

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

  def make_map(
      max_rooms: Int,
      room_min_size: Int,
      room_max_size: Int,
      map_width: Int,
      map_height: Int
  ): (Array[Array[GameTile]], Vector[(Int, Int)]) = {
    val grid = make_empty(map_width, map_height, new WallTile)
    var startingPos = (0, 0)

    def create_room(room: Rect) {
      for {
        x <- room.x1 + 1 to room.x2 - 1
        y <- room.y1 + 1 to room.y2 - 1
      } grid(x)(y) = new FloorTile
    }

    def create_h_tunnel(x1: Int, x2: Int, y: Int) {
      for (x <- min(x1, x2) to max(x1, x2)) grid(x)(y) = new FloorTile
    }

    def create_v_tunnel(x: Int, y1: Int, y2: Int) {
      for (y <- min(y1, y2) to max(y1, y2)) grid(x)(y) = new FloorTile
    }

    // Add doors based on a pattern (must be called after the generation of doors and walls)
    def addDoors() {
      def matchDoorPattern(posArray: Array[(Int, Int)]): Boolean = {
        if (posArray.length != 9) {
          return false
        }
        for (i <- 0 to 5)
          if (!grid(posArray(i)._1)(posArray(i)._2).isInstanceOf[FloorTile]) {
            return false
          }
        return grid(posArray(6)._1)(posArray(6)._2).isInstanceOf[WallTile] && grid(posArray(7)._1)(posArray(7)._2)
          .isInstanceOf[FloorTile] && grid(posArray(8)._1)(posArray(8)._2).isInstanceOf[WallTile]
      }
      for {
        x <- 0 to map_width - 3
        y <- 0 to map_height - 3
      } {
        // Pattern matching with rotations
        var a1 = new Array[(Int, Int)](9)
        var a2 = new Array[(Int, Int)](9)
        var a3 = new Array[(Int, Int)](9)
        var a4 = new Array[(Int, Int)](9)
        for {
          i <- 0 to 2
          j <- 0 to 2
          val ind = 3 * i + j
        } {
          a1(ind) = (x + i, y + j)
          a2(ind) = (x + 2 - i, y + j)
          a3(ind) = (x + j, y + i)
          a4(ind) = (x + j, y + 2 - i)
        }
        if (matchDoorPattern(a1))
          grid(x + 2)(y + 1) = new Door
        else if (matchDoorPattern(a2))
          grid(x)(y + 1) = new Door
        else if (matchDoorPattern(a3))
          grid(x + 1)(y + 2) = new Door
        else if (matchDoorPattern(a4))
          grid(x + 1)(y) = new Door
      }
    }

    var num_room = 0
    var rooms = Vector[Rect]()
    var centersRoom = Vector[(Int, Int)]() // Stocks the center of every rooms
    val rnd = new Random
    for (r <- 0 to max_rooms - 1) {
      val w = room_min_size + rnd.nextInt((room_max_size - room_min_size) + 1)
      val h = room_min_size + rnd.nextInt((room_max_size - room_min_size) + 1)
      val x = rnd.nextInt(map_width - w)
      val y = rnd.nextInt(map_height - h)
      val new_room = new Rect(x, y, w, h)
      val new_center = new_room.center
      var no_intersection = true
      if (num_room > 0) { // Checks if there is an intersection with a previous room
        for (other_room <- rooms)
          no_intersection &= !(new_room.intersect(other_room))
      }
      if (no_intersection) {
        create_room(new_room)
        if (num_room != 0) {
          val prev_center = rooms(rooms.size - 1).center
          if (rnd.nextInt(2) == 1) {
            create_h_tunnel(prev_center._1, new_center._1, prev_center._2)
            create_v_tunnel(new_center._1, prev_center._2, new_center._2)
          } else {
            create_v_tunnel(prev_center._1, new_center._2, prev_center._2)
            create_h_tunnel(prev_center._1, new_center._1, new_center._2)
          }
        }
        rooms = new_room +: rooms
        centersRoom = centersRoom :+ new_center
        num_room += 1
      }
    }
    addDoors()
    return (grid, centersRoom)
  }
}
