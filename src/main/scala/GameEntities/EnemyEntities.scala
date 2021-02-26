package GameEntities

trait Enemy extends Character with AIControlled {
  def action(c: Character): Unit = {
    if (c.isInstanceOf[Player]) {
      attack(c)
    }
  }
}

trait MeleeEnemy extends Character with Enemy {

  def nextCell(): Option[(Int, Int)] = {
    val sPath = board.shortestPath(pos, board.playerEntity.pos)
    sPath match {
      case Some(path) => Some(path(1))
      case None       => None
    }
  }

  override def act(): Unit = {
    nextCell() match {
      case None       => ()
      case Some(cell) => move(cell)
    }
  }
}
