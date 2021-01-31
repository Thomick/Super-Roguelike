package map_objects

import scala.math.{min,max}
import scala.util.Random

abstract class GameTile() {
  def blocking: Boolean
}
case class FloorTile() extends GameTile {
  def blocking = false
}
case class WallTile() extends GameTile {
  def blocking = true
}

class Rect(x:Int,y:Int,w:Int,h:Int) {
  val x1 = x
  val y1 = y
  val x2 = x+w
  val y2 = y+h
  def center() ={
    val center_x = (x1+x2)/2
    val center_y = (y1+y2)/2
    (center_x,center_y)
  }
  def intersect(rect2 : Rect) = {
    (x1 <= rect2.x2 && x2 >= rect2.x1 && y1 <= rect2.y2 && y2 >= rect2.y2)
  }
}

class GameBoard(n: Int, m: Int) {
  val size_x = n
  val size_y = m
  var grid = Array.ofDim[GameTile](size_x, size_y)
  for {
    x <- 0 to size_x - 1
    y <- 0 to size_y - 1
    val pos = (x, y)
  } grid(x)(y) = WallTile()

  def create_room(room : Rect){
    for {
      x <- room.x1 + 1 to room.x2 -1
      y <- room.y1 +1 to room.y2 -1
    } grid(x)(y) = FloorTile()
  }
  def create_h_tunnel(x1:Int, x2:Int, y:Int){
    for (x <- min(x1,x2) to max(x1,x2)) grid(x)(y) = FloorTile()
  }
  def create_v_tunnel(x:Int, y1:Int, y2:Int){
    for (y <- min(y1,y2) to max(y1,y2)) grid(x)(y) = FloorTile()
  }

  def make_map(max_rooms : Int, room_min_size : Int, room_max_size : Int, map_width : Int, map_height : Int){
    var num_room = 0
    var rooms = Vector[Rect]()
    val rnd = new Random
    for ( r <- 0 to max_rooms-1){
      val w=room_min_size+rnd.nextInt((room_max_size - room_min_size)+1)
      val h=room_min_size+rnd.nextInt((room_max_size - room_min_size)+1)
      val x = rnd.nextInt(map_width - w)
      val y = rnd.nextInt(map_height - h)
      val new_room = new Rect(x,y,w,h)
      val new_center = new_room.center
      var no_intersection = true
      if(num_room > 0){
        for(other_room <- rooms) no_intersection &= !(new_room.intersect(other_room))
      }
      if(no_intersection){
        create_room(new_room)
        if(num_room >0){
          val prev_center = rooms(rooms.size -1).center
          if(rnd.nextInt(2)==1){
            create_h_tunnel(prev_center._1,new_center._1,prev_center._2)
            create_v_tunnel(new_center._1, prev_center._2, new_center._2)
          } else {
            create_v_tunnel(prev_center._1,new_center._2,prev_center._2)
            create_h_tunnel(prev_center._1,new_center._1,new_center._2)
          }
        }
        rooms = new_room +: rooms 
        num_room +=1
      }
    }
  }


}

