package items

import map_objects._
import scala.util.Random
import scala.math.{min, max}

abstract class Armor() extends AbstractItem with Equipable {
  val bonusAtt: Int = 0
  val bonusHP: Int = 0
}

abstract class Weapon() extends AbstractItem with Equipable {
  val part = BodyPart.Hand
  val bonusDef: Int = 0
  val bonusHP: Int = 0
}

abstract class RangedWeapon() extends Weapon {
  val range: Int = 10
  val att: Int
  def shoot(shooter: game_entities.CanEquip, board: GameBoard, pos: (Int,Int)) : Unit = {
    if (board.hasCharacter(pos)) {
      val target = board.getCharacter(pos)
      val rnd = new Random
      val damage = max(0, (att * (1 + 3 * rnd.nextGaussian())).toInt)
      shooter.giveDamage(damage,target)
    }
  }
}

class ArmCannon() extends RangedWeapon {
  val name: String = "Arm-cannon"
  val description: String = "It is a arm-cannon. Wow, just like in Megaman."
  val weight = 1500
  val bonusAtt = 0
  val att = 5
}

class IronHelmet() extends Armor {
  val part: BodyPart.Value = BodyPart.Head
  val name: String = "Iron helmet"
  val description: String = "It is a slightly rusty iron helmet"
  override val image = "src/main/resources/ironhelmet.png"
  val bonusDef: Int = 5
  val weight = 500
}

class LaserChainsaw() extends Weapon {
  val name: String = "Laser chainsaw"
  val description: String = "It is a laser chainsaw. It seems really powerful."
  override val image = "src/main/resources/laserchainsaw.png"
  val bonusAtt: Int = 15
  val weight = 1500
}
