package items

import game_entities._
import map_objects._

trait Consumable extends AbstractItem {
  availableActions += "Z - Use"
  val consumptionMessage: String

  // Apply consumption effect to the character and return action description (for the logs)
  def consume(character: Character): String = return consumptionMessage
}

// Parent class for food
abstract class Food extends AbstractItem with Consumable with Throwable {
  availableActions -= "Z - Use"
  availableActions += "Z - Eat"
  val consumedWhenThrown = false
  def effectWhenThrown(board: GameBoard, pos: (Int, Int)): String = ""
}

// Parent class for syringe. Throwable, stun on hit, destroyed when thrown
abstract class Syringe extends AbstractItem with Throwable {
  val consumedWhenThrown = false
  def effectWhenThrown(board: GameBoard, pos: (Int, Int)): String = {
    if (board.hasCharacter(pos)) {
      val target = board.getCharacter(pos)
      target.statusList += new StunnedStatus(3)
      return "The needle hit " + target.name + " and stun it for a short time."
    }
    return ""
  }
}

// Healing item, stuns a character for 1 turn when thrown
class Morphin extends Syringe with Consumable {
  var name = "Morphin"
  val description = "A cute little needle to feel a little better"
  val weight = 50
  override val image: String = "src/main/resources/morphin.png"
  val consumptionMessage = "It stings. You feel a little better."

  override def consume(character: Character): String = {
    character.addToHP(5)
    if (character.isInstanceOf[HasInventory])
      character.asInstanceOf[HasInventory].obtainItem(new EmptySyringe)
    super.consume(character)
  }
}

// Item obtained after having used a full syringe (eg. Morphin)
// Don't have any use for now
class EmptySyringe extends Syringe {
  var name = "Empty syringe"
  val description = "An empty syringe. Fill it or drop it"
  val weight = 30
  override val image: String = "src/main/resources/emptysyringe.png"
}

// Consumable item that can remove bleeding effect
class Bandage() extends AbstractItem with Consumable {
  var name = "Bandage"
  val description = "A cute little needle to feel a little better"
  val weight = 50
  override val image: String = "src/main/resources/bandage.png"
  val consumptionMessage = "It hurts but at least you stop bleeding"

  override def consume(character: Character): String = {
    character.removeStatus(_.isInstanceOf[BleedingStatus])
    super.consume(character)
  }
}
