package logger

import scala.collection._

class Logger {
  var nbBufferedLines = 5
  val buffer = new mutable.Queue[String]

  def writeLog(message: String): Unit = {
    message
      .split("\n")
      .foreach(line => {
        while (buffer.length >= nbBufferedLines) {
          buffer.dequeue()
        }
        buffer.enqueue(line)
      })
  }

  def getLogs = buffer.toList
}
