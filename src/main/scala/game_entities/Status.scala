package game_entities

// Class used to store the effects of the current status effects on a character
@SerialVersionUID(106L)
class StatusResults extends Serializable {
  var canMove: Boolean = true
  var healthModifier: Int = 0

  def reset(): Unit = {
    canMove = true
    healthModifier = 0
  }
}

// Parent class for status, handles the remaining time of a status
abstract class Status(duration: Int) {
  val name: String
  val effectDescription: String
  var remainingTime = duration

  // Get remaining time as a string
  def remainingTimeString(): String =
    if (remainingTime < 0)
      return "Forever"
    else if (remainingTime == 1)
      return remainingTime.toString() + " turn"
    else
      return remainingTime.toString() + " turns"

  def applyEffect(results: StatusResults): Unit = {
    if (remainingTime > 0)
      remainingTime -= 1
  }
}

// Effect : Lose some health points each turn
class BleedingStatus(duration: Int) extends Status(duration) {
  val name = "Bleeding"
  val effectDescription = "You lose health each turn"

  override def applyEffect(results: StatusResults): Unit = {
    results.healthModifier -= 3
    super.applyEffect(results)
  }
}

// Effect : Gain some health points each turn
class RegenerationStatus(duration: Int) extends Status(duration) {
  val name = "Regeneration"
  val effectDescription = "You gain health each turn"

  override def applyEffect(results: StatusResults): Unit = {
    results.healthModifier += 5
    super.applyEffect(results)
  }
}

// Effect : The character can not move
class StunnedStatus(duration: Int) extends Status(duration) {
  val name = "Stunned"
  val effectDescription = "You can't move for the duration of the status"

  override def applyEffect(results: StatusResults): Unit = {
    results.canMove = false
    super.applyEffect(results)
  }
}
