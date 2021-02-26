package GameEntities

object Direction extends Enumeration {
  val Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight, Nop = Value

  val allDirections =
    List(Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight)

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
