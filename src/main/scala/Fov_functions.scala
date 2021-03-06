package fov_functions

import scala.math.{pow, sqrt}
import map_objects._

class FovMap(board: Array[Array[GameTile]]) {
  var resistance_map =
    board.map(_.map(_.blocking_sight)) //Apply a mask on the board to keep only the boolean blocking_sight
  var light_map = resistance_map.map(_.map(x => false))
  var width = resistance_map.size
  var height = resistance_map(0).size
  val radius = 10

  def norm(x: Double, y: Double) = sqrt(pow(x, 2) + pow(y, 2))
  def is_light(x: Int, y: Int) = light_map(x)(y)

  def update(newGrid: Array[Array[GameTile]]): Unit = {
    resistance_map =
      newGrid.map(_.map(_.blocking_sight)) //Apply a mask on the board to keep only the boolean blocking_sight
    light_map = resistance_map.map(_.map(x => false))
    width = resistance_map.size
    height = resistance_map(0).size
  }

  def compute_fov(startx: Int, starty: Int) = {
    light_map = resistance_map.map(_.map(x => false))
    light_map(startx)(starty) = true
    for { x <- 0 to 1; y <- 0 to 1 } { //Launch the algorithm on each octant
      castLight(1, 1.0, 0.0, 0, (2 * x) - 1, (2 * y) - 1, 0, startx, starty)
      castLight(1, 1.0, 0.0, (2 * x) - 1, 0, 0, (2 * y) - 1, startx, starty)
    }
    light_map
  }

  def castLight(row: Int, start: Double, end: Double, xx: Int, xy: Int, yx: Int, yy: Int, startx: Int, starty: Int) {
    var startvar = start
    var newStart = 0.0
    if (startvar >= end) {
      var blocked = false
      var distance = row
      while (distance <= radius && !blocked) {
        val deltaY = -distance
        var deltaX = -distance
        while (deltaX <= 0) {
          val currentX = startx + deltaX * xx + deltaY * xy
          val currentY = starty + deltaX * yx + deltaY * yy
          val leftSlope = (deltaX - 0.5) / (deltaY + 0.5)
          val rightSlope = (deltaX + 0.5) / (deltaY - 0.5)
          val centerSlope = (deltaX + 0.0) / (deltaY + 0.0)
          if (
            !(currentX >= 0 && currentY >= 0 && currentX < width && currentY < height) || startvar < rightSlope //Test if the cell is on the board
          ) {} else if (end > leftSlope) {
            deltaX = 1 // Breaks the loop
          } else {
            if (norm(deltaX, deltaY) <= radius) {
              if (
                resistance_map(currentX)(
                  currentY
                ) || ((centerSlope <= startvar) && (centerSlope >= end)) // The cell is visible only if it's a wall or the center of the cell is visible
              ) {
                light_map(currentX)(currentY) = true
              }
            }
            if (blocked) {
              if (resistance_map(currentX)(currentY)) {
                newStart = rightSlope
              } else {
                blocked = false
                startvar = newStart
              }
            } else if (resistance_map(currentX)(currentY) && distance < radius) {
              blocked = true
              castLight(distance + 1, startvar, leftSlope, xx, xy, yx, yy, startx, starty)
              newStart = rightSlope
            }

          }
          deltaX += 1
        }
        distance += 1
      }
    }
  }

}
