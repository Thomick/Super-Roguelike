package game_entities

import scala.util.Random
import items.Money

trait Enemy extends Character with AIControlled {
  def action(c: GameEntity): Unit =
    if (c.isInstanceOf[Player])
      attack(c.asInstanceOf[Player])

  override def die: Unit = {
    super.die()
    board.addItem(new ItemEntity(pos, board, new Money((new Random).nextInt(2) + 1)), pos)
  }
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

  override def act(): Unit = {
    updateStatus()
    nextCell match {
      case None       => ()
      case Some(cell) => move(cell)
    }
  }
}
