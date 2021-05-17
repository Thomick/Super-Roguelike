package map_objects

import scala.collection.mutable._
import game_entities._
import logger.Logger

// Verifies if some event happened during this turn
@SerialVersionUID(100002L)
abstract class EventWatcher extends Serializable {
  def checkEvent(): Boolean
}

// Trait used to add a boolean state to a class
trait Activable {
  var activated = false
}

// Watcher for the event : the object end the turn in the state [activated]
class ActivableWatcher(obj: Activable) extends EventWatcher {
  def checkEvent(): Boolean = obj.activated
}

// Trait used to add an action which can be called by a trigger
trait Triggerable {
  def executeAction(): Unit
}

@SerialVersionUID(100003L)
class Trigger extends Serializable {
  val events = new ArrayBuffer[EventWatcher]
  val actions = new ArrayBuffer[Triggerable]
  private[this] var _triggered = false
  def triggered: Boolean = _triggered

  def update(): Unit = {
    if (_triggered)
      return // Actions have already been done
    events --= events.filter(_.checkEvent()) // Remove events that already happened
    if (events.isEmpty) { // No more watched event
      for (a <- actions)
        a.executeAction()
      _triggered = true
    }
  }
}

// Print a message to the logger when triggered
@SerialVersionUID(100004L)
class LogAction(message: String, logger: Logger) extends Triggerable with Serializable {
  def executeAction(): Unit = logger.writeLog(message)
}

// Activated when [character] dies
class DeathWatcher(character: Character) extends EventWatcher {
  def checkEvent(): Boolean = character.currentHP <= 0
}

// Trigger where events are the death of a group of characters
class DeathTrigger(characters: List[Character] = List.empty[Character]) extends Trigger {
  for (c <- characters)
    addCharacter(c)

  def addCharacter(c: Character) =
    events += new DeathWatcher(c)
}
