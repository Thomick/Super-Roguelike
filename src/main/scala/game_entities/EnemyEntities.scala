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
  val description: String = ""

  // If the attack is successful then the function [effect] is called with a probability [effectProb] (for example to aplly a status to the target)
  var effects = new ArrayBuffer[(String, Int, Double)]
  var lootableItems = new ArrayBuffer[(AbstractItem, Double)]
  var reward = 0

  private val rnd = new Random

  def action(c: GameEntity): Unit =
    if (c.isInstanceOf[Player])
      attack(c.asInstanceOf[Player])

  override def die: Unit = {
    super.die()
    board.addItem(new ItemEntity(pos, board, new Money(reward)), pos)
    val rnd = new Random
    val s = rnd.nextDouble()
    var acc: Double = 0
    for ((item, prob) <- lootableItems) {
      acc += prob
      if (acc > s) {
        board.addItem(new ItemEntity(pos, board, item), pos)
        return
      }
    }
  }

  def appliesEffect(c: Character): Unit = {
    val s = rnd.nextDouble()
    var totProb: Double = 0
    for ((et, duration, prob) <- effects) {
      totProb += prob
      if (totProb > s) {
        et match {
          case "burning"      => c.statusList += new BurningStatus(duration)
          case "regeneration" => c.statusList += new RegenerationStatus(duration)
          case "stunned"      => c.statusList += new StunnedStatus(duration)
          case "bleeding"     => c.statusList += new BleedingStatus(duration)
          case _              => ()
        }
        return
      }
    }
  }

  override def attack(c: Character): Boolean = {
    if (super.attack(c)) {
      appliesEffect(c)
      return true
    }
    return false
  }

}

// Melee enemy behaviour : walk toward the player and tries to hit it
class MeleeEnemy(init_pos: (Int, Int), b: GameBoard, name: String = "Dog") extends Enemy(init_pos, b, name) {
  image = "src/main/resources/enemy_sprites/dog1.png"
  override def act(visible: Boolean): Unit = {
    updateStatus()
    moveTowards()
  }
}

class RangedEnemy(init_pos: (Int, Int), b: GameBoard, name: String = "Turret") extends Enemy(init_pos, b, name) {
  var range = 5
  image = "src/main/resources/enemy_sprites/turret.png"
  override def act(visible: Boolean): Unit = {
    updateStatus()
    if (
      visible && sqrt(pow(pos._1 - board.playerEntity.pos._1, 2) + pow(pos._2 - board.playerEntity.pos._2, 2)) < range
    )
      attack(board.playerEntity.asInstanceOf[Player])
  }
}

class MovingRangedEnemy(init_pos: (Int, Int), b: GameBoard, name: String = "Robot")
    extends RangedEnemy(init_pos, b, name) {
  image = "src/main/resources/enemy_sprites/robot2.png"
  var minRange: Int = 2
  range = 5
  override def act(visible: Boolean): Unit = {
    updateStatus()
    val dst = sqrt(pow(pos._1 - board.playerEntity.pos._1, 2) + pow(pos._2 - board.playerEntity.pos._2, 2))
    val oldPos = pos
    if (dst <= minRange)
      moveAway()
    else if (!visible || dst > range)
      moveTowards()
    if (oldPos == pos && visible && dst <= range)
      attack(board.playerEntity.asInstanceOf[Player])
  }
}
