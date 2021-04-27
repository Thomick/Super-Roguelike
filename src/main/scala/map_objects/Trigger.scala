package map_objects

import scala.collection.mutable._

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
    for (e <- events)
      if (e.checkEvent())
        events -= e
    if (events.isEmpty) {
      for (a <- actions)
        a.executeAction()
      triggered = true
    }
  }
}
