package items

import game_entities._

abstract class Food() extends AbstractItem() with Consumable with Throwable {
  availableActions -= "C - Consume"
  availableActions += "C - Eat"
}

class Apple() extends Food() {
  val name = "Apple"
  val description = "A red apple"
  val weight = 200
  override val image = "src/main/resources/apple.png"

  override def consume(character: Character): String = {
    character.addToHP(5)
    return "You eat this apple. You feel ready to continue your quest."
  }
}
