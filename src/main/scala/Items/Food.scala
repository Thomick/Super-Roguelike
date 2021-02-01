package Items

abstract class Food() extends AbstractItem() with Consumable with Throwable {
  val availableActions = List("Eat", "Throw")
}

class Apple() extends Food() {
  val name = "Apple"
  val description = "A red apple"
  val weight = 200
}
