package game_entities

import scala.util.Random
import items.Money
import scala.math.{pow, sqrt}
import map_objects._

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

  override def act(visible: Boolean): Unit = {
    updateStatus()
    nextCellTowards match {
      case None       => ()
      case Some(cell) => move(cell)
    }
  }
}

trait RangedEnemy extends Character with Enemy {
  val range: Int
  override def act(visible: Boolean): Unit = {
    updateStatus()
    if (
      visible && sqrt(pow(pos._1 - board.playerEntity.pos._1, 2) + pow(pos._2 - board.playerEntity.pos._2, 2)) < range
    )
      attack(board.playerEntity.asInstanceOf[Player])
  }
}

class Robot(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with MeleeEnemy with Humanoid {
  val name = "Robot"
  val description = "An angry robot"
  override val image = "src/main/resources/robot2.png"
}

class Dog(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with MeleeEnemy {
  val name = "Dog"
  val description = "An angry robot dog"
  override val image = "src/main/resources/dog1.png"
}

class Turret(init_pos: (Int, Int), b: GameBoard) extends Character(init_pos, b) with RangedEnemy {
  val name = "Turret"
  val description = ""
  val range = 5
}
