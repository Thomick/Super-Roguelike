package items

class Money(val value: Int) extends AbstractItem {
  var name = "Coins"
  val description = "You are rich !"
  val weight = value * 5
}

class HackingTools extends AbstractItem {
  var name = "Hacking Tools"
  val description = "Can be used to hack things"
  val weight = 100
}
