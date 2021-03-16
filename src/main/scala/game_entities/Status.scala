package game_entities

object StatusHandler {
  // Gather the effects of the status in a StatusResults and return it.
  def getEffects(statusCollection: Iterable[Status]): StatusResults = {
    val results = new StatusResults
    statusCollection.foreach(status => status.applyEffect(results))
    return results
  }
}

class StatusResults {
  var canMove: Boolean = true
  var healthModifier: Int = 0
}

abstract class Status(var remainingTime: Int) {
  val name: String
  val effectDescription: String

  // Get remaining time as a string
  def remainingTimeString(): String =
    if (remainingTime < 0)
      return "Forever"
    else
      return remainingTime.toString()

  def applyEffect(results: StatusResults): Unit = {
    if (remainingTime >= 0)
      remainingTime -= 1
  }
}

class BleedingStatus(remainingTime: Int) extends Status(remainingTime) {
  val name = "Bleeding"
  val effectDescription = "You lose health each turn"

  override def applyEffect(results: StatusResults): Unit = {
    results.healthModifier -= 3
    super.applyEffect(results)
  }
}

class RegenerationStatus(remainingTime: Int) extends Status(remainingTime) {
  val name = "Regeneration"
  val effectDescription = "You gain health each turn"

  override def applyEffect(results: StatusResults): Unit = {
    results.healthModifier += 5
    super.applyEffect(results)
  }
}

class StunnedStatus(remainingTime: Int) extends Status(remainingTime) {
  val name = "Stunned"
  val effectDescription = "You can move for the duration of the status"

  override def applyEffect(results: StatusResults): Unit = {
    results.canMove = false
    super.applyEffect(results)
  }
}
