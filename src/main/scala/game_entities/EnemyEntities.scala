package game_entities

trait Enemy extends Character with AIControlled {
  def action(c: Character): Unit =
    if (c.isInstanceOf[Player])
      attack(c)
}

// Melee enemy behaviour : walk toward the player and tries to hit it
trait MeleeEnemy extends Character with Enemy {
  // Get the next move toward the player
  def nextCell(): Option[(Int, Int)] = {
    val sPath = board.shortestPath(pos, board.playerEntity.pos)
    sPath match {
      case Some(path) => Some(path(1))
      case None       => None
    }
  }

  override def act(): Unit =
    nextCell match {
      case None       => ()
      case Some(cell) => move(cell)
    }
  super.act()
}
