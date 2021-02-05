package Items

abstract class Armor() extends AbstractItem with Equipable with Passive {
  val bonusDef: Int = 0
  val bonusHP: Int = 0
}

abstract class BasicWeapon() extends AbstractItem with Equipable with Passive {
  val part = BodyPart.Hand
  val bonusAtt: Int = 0
  val bonusHP: Int = 0
}
