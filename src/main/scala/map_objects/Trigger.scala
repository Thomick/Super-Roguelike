package map_objects

import scala.collection.mutable._
import game_entities._
import logger.Logger

abstract class EventWatcher {
  def checkEvent(): Boolean
}

trait Triggerable {
  def executeAction(): Unit
}

class Trigger {
  val events = new ArrayBuffer[EventWatcher]
  val actions = new ArrayBuffer[Triggerable]
  var triggered = false

  def update(): Unit = {
    if (triggered)
      return
    for (e <- events) {
      if (e.checkEvent())
        events -= e
    }
    if (events.isEmpty) {
      for (a <- actions)
        a.executeAction()
      triggered = true
    }
  }
}

class LogAction(message: String, logger: Logger) extends Triggerable {
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
