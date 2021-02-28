package items

abstract class Armor() extends AbstractItem with Equipable {
  val bonusAtt: Int = 0
  val bonusHP: Int = 0
}

abstract class Weapon() extends AbstractItem with Equipable {
  val part = BodyPart.Hand
  val bonusDef: Int = 0
  val bonusHP: Int = 0
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
