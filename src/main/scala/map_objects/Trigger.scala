package map_objects

import scala.collection.mutable._
import game_entities._
import logger.Logger

@SerialVersionUID(100002L)
abstract class EventWatcher extends Serializable {
  def checkEvent(): Boolean
}

trait Activable {
  var activated = false
}

class ActivableWatcher(obj: Activable) extends EventWatcher {
  def checkEvent(): Boolean = obj.activated
}

trait Triggerable {
  def executeAction(): Unit
}

@SerialVersionUID(100003L)
class Trigger extends Serializable {
  val events = new ArrayBuffer[EventWatcher]
  val actions = new ArrayBuffer[Triggerable]
  var triggered = false

  def update(): Unit = {
    if (triggered)
      return
    events --= events.filter(_.checkEvent())
    if (events.isEmpty) {
      for (a <- actions)
        a.executeAction()
      triggered = true
    }
  }
}

@SerialVersionUID(100004L)
class LogAction(message: String, logger: Logger) extends Triggerable with Serializable {
  def executeAction(): Unit = logger.writeLog(message)
}

class DeathWatcher(character: Character) extends EventWatcher {
  def checkEvent(): Boolean = character.currentHP <= 0
}

class DeathTrigger(characters: List[Character] = List.empty[Character]) extends Trigger {
  for (c <- characters)
    addCharacter(c)

  def addCharacter(c: Character) =
    events += new DeathWatcher(c)
}
