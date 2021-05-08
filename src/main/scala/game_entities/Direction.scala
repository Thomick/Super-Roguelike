package game_entities

object Direction extends Enumeration {
  val Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight, Nop = Value

  val allDirections =
    List(Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight)

  // Transforms a element of {-1,0,1}^2 into a direction
  def giveDirection(distance: (Int, Int)): Direction.Value = {
    distance match {
      case (-1,0) => Direction.Left
      case (1,0) => Direction.Right
      case (0,-1) => Direction.Up
      case (0,1) => Direction.Down
      case (-1,-1) => Direction.UpLeft
      case (1,-1) => Direction.UpRight
      case (-1,1) => Direction.DownLeft
      case (1,1) => Direction.DownRight
      case _ => Direction.Nop
    }
  }

  def giveVector(direction: Direction.Value): (Int,Int) = {
    direction match {
      case Direction.Nop => (0,0)
      case Direction.Up => (0,1)
      case Direction.Right => (1,0)
      case Direction.Down => (0,-1)
      case Direction.Left => (-1,0)
      case Direction.UpLeft => (-1,-1)
      case Direction.UpRight => (1,-1)
      case Direction.DownLeft => (-1,1) 
      case Direction.DownRight => (1,1)
    }
  }

  def turnClockwise(dir: Direction.Value): Direction.Value = {
    dir match {
      case Direction.Nop       => Direction.Nop
      case Direction.Left      => Direction.UpLeft
      case Direction.Right     => Direction.DownRight
      case Direction.Up        => Direction.UpRight
      case Direction.Down      => Direction.DownLeft
      case Direction.UpLeft    => Direction.Up
      case Direction.UpRight   => Direction.Right
      case Direction.DownLeft  => Direction.Left
      case Direction.DownRight => Direction.Down
    }
  }

  def turnCounterClockwise(dir: Direction.Value): Direction.Value = {
    dir match {
      case Direction.Nop       => Direction.Nop
      case Direction.Left      => Direction.DownLeft
      case Direction.Right     => Direction.UpRight
      case Direction.Up        => Direction.UpLeft
      case Direction.Down      => Direction.DownRight
      case Direction.UpLeft    => Direction.Left
      case Direction.UpRight   => Direction.Up
      case Direction.DownLeft  => Direction.Down
      case Direction.DownRight => Direction.Right
    }
  }

  def oppositeDirection(dir: Direction.Value): Direction.Value = {
    dir match {
      case Direction.Nop       => Direction.Nop
      case Direction.Left      => Direction.Right
      case Direction.Right     => Direction.Left
      case Direction.Up        => Direction.Down
      case Direction.Down      => Direction.Up
      case Direction.UpLeft    => Direction.DownRight
      case Direction.UpRight   => Direction.DownLeft
      case Direction.DownLeft  => Direction.UpRight
      case Direction.DownRight => Direction.UpLeft
    }
  }

  def nextPos(pos: (Int, Int), dir: Direction.Value): (Int, Int) = {
    dir match {
      case Direction.Left      => (pos._1 - 1, pos._2)
      case Direction.Right     => (pos._1 + 1, pos._2)
      case Direction.Up        => (pos._1, pos._2 - 1)
      case Direction.Down      => (pos._1, pos._2 + 1)
      case Direction.UpLeft    => (pos._1 - 1, pos._2 - 1)
      case Direction.UpRight   => (pos._1 + 1, pos._2 - 1)
      case Direction.DownLeft  => (pos._1 - 1, pos._2 + 1)
      case Direction.DownRight => (pos._1 + 1, pos._2 + 1)
      case Direction.Nop       => pos
    }
  }
}
