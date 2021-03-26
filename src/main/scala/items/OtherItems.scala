package items

class Money(val value: Int) extends AbstractItem {
  val name = "Coins"
  val description = "You are rich !"
  val weight = value * 5
}
