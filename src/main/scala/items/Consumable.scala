package items

import game_entities._
import map_objects._

trait Consumable extends AbstractItem {
  availableActions += "C - Consume"
  val consumptionMessage: String

  // Apply consumption effect to the character and return action description (for the logs)
  def consume(character: Character): String = return consumptionMessage
}

abstract class Food extends AbstractItem with Consumable with Throwable {
  availableActions -= "C - Consume"
  availableActions += "C - Eat"
  val consumedWhenThrown = false
  def effectWhenThrown(board : GameBoard, pos : (Int,Int)): Unit = ()
}

class Morphin extends AbstractItem with Consumable with Throwable {
  val name = "Morphin"
  val description = "A cute little needle to feel a little better"
  val weight = 50
  val consumptionMessage = "It stings. You feel a little better."
  val consumedWhenThrown = false
  def effectWhenThrown(board : GameBoard, pos : (Int,Int)): Unit = ()

  override def consume(character: Character): String = {
    character.addToHP(5)
    if (character.isInstanceOf[HasInventory])
      character.asInstanceOf[HasInventory].obtainItem(new Syringe)
    super.consume(character)
  }
}

class Syringe extends AbstractItem {
  val name = "Empty syringe"
  val description = "An empty syringe. Fill it or drop it"
  val weight = 30
}

class Bandage() extends AbstractItem with Consumable {
  val name = "Bandage"
  val description = "A cute little needle to feel a little better"
  val weight = 50
  val consumptionMessage = "It hurts but at least you stop bleeding"

  override def consume(character: Character): String = {
    //character.removeStatus(Bleeding) // TODO
    super.consume(character)
  }
}
