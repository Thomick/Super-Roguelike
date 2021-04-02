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

// Makes a piece of equipment upgradable
trait Upgradable extends Equipable {
  var level: Int = 0
  // Upgrade value is used when computing the new stats of an item
  val upgradeValue: Int
  var baseName = ""

  //Upgrades by 1 level and update item name
  def upgrade(): Unit = {
    if (level == 0)
      baseName = name
    level += 1
    updateName()
  }

  // Upgrades the item by [n] levels
  def upgrade(n: Int): Unit = {
    for (i <- 1 to n)
      upgrade()
  }

  // Appends (+level) at the end of the item name
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

abstract class Weapon() extends AbstractItem with Equipable with Upgradable {
  val part = BodyPart.Hand
  var bonusDef: Int = 0
  var bonusHP: Int = 0

  override def upgrade(): Unit = {
    super.upgrade()
    bonusAtt += upgradeValue
  }
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

  // Upgrade by giving a flat boost to [att] stat
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
  var att = 2
  override val image = "src/main/resources/armcannon.png"

  val upgradeValue = 2
}

class LaserEyes extends RangedWeapon {
  override val part = BodyPart.Head
  var name: String = "Laser eyes"
  val description: String = ""
  val weight = 100
  var bonusAtt = 0
  var att = 1
  override val image = "src/main/resources/lasereyes.png"

  val upgradeValue = 1
}

class IronHelmet() extends Armor with Upgradable {
  val part: BodyPart.Value = BodyPart.Head
  var name: String = "Iron helmet"
  val description: String = "It is a slightly rusty iron helmet"
  override val image = "src/main/resources/ironhelmet.png"
  var bonusDef: Int = 5
  val weight = 500

  val upgradeValue = 5
  override def upgrade(): Unit = {
    super.upgrade()
    bonusDef += upgradeValue
  }
}

class CowboyHat extends Armor {
  val part: BodyPart.Value = BodyPart.Head
  var name: String = "Cowboy Hat"
  val description: String = ""
  override val image = "src/main/resources/cowboyhat.png"
  var bonusDef: Int = 2
  val weight = 100
}

class HeavyJacket extends Armor {
  val part: BodyPart.Value = BodyPart.Head
  var name: String = "Heavy Jacket"
  val description: String = ""
  override val image = "src/main/resources/jacket.png"
  var bonusDef: Int = 7
  val weight = 1000
}

class LaserChainsaw() extends Weapon with Upgradable {
  var name: String = "Laser chainsaw"
  val description: String = "It is a laser chainsaw. It seems really powerful."
  override val image = "src/main/resources/laserchainsaw.png"
  var bonusAtt: Int = 20
  val weight = 1500

  val upgradeValue = 5
}

class Knuckles() extends Weapon with Upgradable {
  var name: String = "Brass Knuckles"
  val description: String = ""
  override val image = "src/main/resources/knuckles.png"
  var bonusAtt: Int = 3
  val weight = 500

  val upgradeValue = 1
}

class PoweredHammer extends Weapon with Upgradable {
  var name: String = "PoweredHammer"
  val description: String = ""
  override val image = "src/main/resources/hammer.png"
  var bonusAtt: Int = 10
  val weight = 1500

  val upgradeValue = 5
}
