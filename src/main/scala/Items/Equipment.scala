package Items

abstract class Armor() extends AbstractItem with Equipable with Passive {}

abstract class BasicWeapon() extends AbstractItem with Equipable with Passive {
  val part = BodyPart.Hand
}
