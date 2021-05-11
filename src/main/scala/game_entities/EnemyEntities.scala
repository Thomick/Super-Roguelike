package game_entities

import scala.util.Random
import items.Money
import scala.math.{pow, sqrt}
import map_objects._
import items.AbstractItem
import scala.collection.mutable._

abstract class Enemy(init_pos: (Int, Int), b: GameBoard, init_name: String = "Unnamed")
    extends Character(init_pos, b)
    with AIControlled {
  val name = init_name

  // If the attack is successful then the function [effect] is called with a probability [effectProb] (for example to aplly a status to the target)
  var effect: Character => Unit = c => ()
  var effectProb: Double = 0.5
  var lootableItems = new ArrayBuffer[(AbstractItem, Double)]

  private val rnd = new Random

  def action(c: GameEntity): Unit =
    if (c.isInstanceOf[Player])
      attack(c.asInstanceOf[Player])

  override def die: Unit = {
    super.die()
    board.addItem(new ItemEntity(pos, board, new Money((new Random).nextInt(2) + 1)), pos)
    val rnd = new Random
    val s = rnd.nextDouble()
    var i = 0
    var acc: Double = 0
    var looted = false
    while (lootableItems.length > i && !looted) {
      acc += lootableItems(i)._2
      if (acc > s) {
        board.addItem(new ItemEntity(pos, board, lootableItems(i)._1), pos)
        looted = true
      }
      i += 1
    }
  }

  override def attack(c: Character): Boolean = {
    if (super.attack(c)) {
      if (rnd.nextDouble() < effectProb)
        effect(c)
      return true
    }
    return false
  }

}

// Melee enemy behaviour : walk toward the player and tries to hit it
trait MeleeEnemy extends Enemy {

  override def act(visible: Boolean): Unit = {
    updateStatus()
    moveTowards()
  }
}

trait RangedEnemy extends Enemy {
  val range: Int
  override def act(visible: Boolean): Unit = {
    updateStatus()
    if (
      visible && sqrt(pow(pos._1 - board.playerEntity.pos._1, 2) + pow(pos._2 - board.playerEntity.pos._2, 2)) < range
    )
      attack(board.playerEntity.asInstanceOf[Player])
  }
}

trait MovingRangedEnemy extends RangedEnemy {
  val minRange: Int
  override def act(visible: Boolean): Unit = {
    updateStatus()
    val dst = sqrt(pow(pos._1 - board.playerEntity.pos._1, 2) + pow(pos._2 - board.playerEntity.pos._2, 2))

    if (dst <= minRange)
      moveAway()
    else if (visible && dst < range)
      attack(board.playerEntity.asInstanceOf[Player])
    else
      moveTowards()
  }
}

class Robot(init_pos: (Int, Int), b: GameBoard, name: String = "Robot")
    extends Enemy(init_pos, b, name)
    with MovingRangedEnemy
    with Humanoid {
  val description = "An angry robot"
  override val image = "src/main/resources/robot2.png"
  val minRange: Int = 2
  val range: Int = 5
}

class Dog(init_pos: (Int, Int), b: GameBoard, name: String = "Dog") extends Enemy(init_pos, b, name) with MeleeEnemy {
  val description = "An angry robot dog"
  override val image = "src/main/resources/dog1.png"
}

class Turret(init_pos: (Int, Int), b: GameBoard, name: String = "Turret")
    extends Enemy(init_pos, b, name)
    with RangedEnemy {
  val description = ""
  val range = 5
  override val image = "src/main/resources/turret.png"
}
