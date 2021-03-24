package game_entities

@SerialVersionUID(106L)
class StatusResults extends Serializable {
  var canMove: Boolean = true
  var healthModifier: Int = 0

  def reset(): Unit = {
    canMove = true
    healthModifier = 0
  }
}

abstract class Status(duration: Int) {
  val name: String
  val effectDescription: String
  var remainingTime = duration

  // Get remaining time as a string
  def remainingTimeString(): String =
    if (remainingTime < 0)
      return "Forever"
    else
      return remainingTime.toString()

  def applyEffect(results: StatusResults): Unit = {
    if (remainingTime > 0)
      remainingTime -= 1
    println(remainingTime)
  }
}

class BleedingStatus(duration: Int) extends Status(duration) {
  val name = "Bleeding"
  val effectDescription = "You lose health each turn"

  override def applyEffect(results: StatusResults): Unit = {
    results.healthModifier -= 3
    super.applyEffect(results)
  }
}

class RegenerationStatus(duration: Int) extends Status(duration) {
  val name = "Regeneration"
  val effectDescription = "You gain health each turn"

  override def applyEffect(results: StatusResults): Unit = {
    results.healthModifier += 5
    super.applyEffect(results)
  }
}

class StunnedStatus(duration: Int) extends Status(duration) {
  val name = "Stunned"
  val effectDescription = "You can't move for the duration of the status"

  override def applyEffect(results: StatusResults): Unit = {
    results.canMove = false
    super.applyEffect(results)
  }
}
