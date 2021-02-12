package Items

abstract class Food() extends AbstractItem() with Consumable with Throwable {
  availableActions -= "C - Consume"
  availableActions += "C - Eat"
}

class Apple() extends Food() {
  val name = "Apple"
  val description = "A red apple"
  val weight = 200
  override val image = "src/main/resources/apple.png"
}
