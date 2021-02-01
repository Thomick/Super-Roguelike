package Items

abstract class Food() extends AbstractItem() with Consumable with Throwable {
  val availableActions = List("Eat", "Throw")
}
