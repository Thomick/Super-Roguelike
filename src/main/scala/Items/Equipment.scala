package Items

abstract class Armor() extends AbstractItem with Equipable with Passive {
  val bonusAtt: Int = 0
  val bonusHP: Int = 0
}

abstract class BasicWeapon() extends AbstractItem with Equipable with Passive {
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
