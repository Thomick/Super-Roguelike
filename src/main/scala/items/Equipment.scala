package items

import map_objects._
import scala.util.Random
import scala.math.{min, max}

trait Equipable extends AbstractItem {
  availableActions += "R - Equip/Unequip"
  // Bodypart on which the item is equiped
  val part: BodyPart.Value

  var bonusAtt: Int
  var bonusDef: Int
  var bonusHP: Int
}

trait Upgradable extends Equipable {
  var level: Int = 0
  val upgradeValue: Int
  var baseName = ""

  def upgrade(): Unit = {
    if (level == 0)
      baseName = name
    level += 1
    updateName()
  }
  def upgrade(n: Int): Unit = {
    for (i <- 1 to n)
      upgrade()
  }
  def updateName(): Unit =
    if (level != 0)
      name = baseName + " (+" + level + ")"
    else
      name = baseName
}

abstract class Armor() extends AbstractItem with Equipable {
  var bonusAtt: Int = 0
  var bonusHP: Int = 0
}

abstract class Weapon() extends AbstractItem with Equipable {
  val part = BodyPart.Hand
  var bonusDef: Int = 0
  var bonusHP: Int = 0
}

abstract class RangedWeapon() extends Weapon with Upgradable {
  val range: Int = 10
  var att: Int
  def shoot(shooter: game_entities.CanEquip, board: GameBoard, pos: (Int, Int)): Unit = {
    if (board.hasCharacter(pos)) {
      val target = board.getCharacter(pos)
      val rnd = new Random
      val damage = max(0, (att * (1 + 3 * rnd.nextGaussian())).toInt)
      shooter.giveDamage(damage, target)
    }
  }

  override def upgrade(): Unit = {
    super.upgrade()
    att += upgradeValue
  }
}

class ArmCannon() extends RangedWeapon {
  var name: String = "Arm-cannon"
  val description: String = "It is an arm-cannon. Wow, just like in Megaman."
  val weight = 1500
  var bonusAtt = 0
  var att = 1
  override val image = "src/main/resources/armcannon.png"

  val upgradeValue = 1
}

class IronHelmet() extends Armor {
  val part: BodyPart.Value = BodyPart.Head
  var name: String = "Iron helmet"
  val description: String = "It is a slightly rusty iron helmet"
  override val image = "src/main/resources/ironhelmet.png"
  var bonusDef: Int = 5
  val weight = 500
}

class LaserChainsaw() extends Weapon with Upgradable {
  var name: String = "Laser chainsaw"
  val description: String = "It is a laser chainsaw. It seems really powerful."
  override val image = "src/main/resources/laserchainsaw.png"
  var bonusAtt: Int = 10
  val weight = 1500

  val upgradeValue = 5
  override def upgrade(): Unit = {
    super.upgrade()
    bonusAtt += upgradeValue
  }
}
