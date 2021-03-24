package logger

import scala.collection._

@SerialVersionUID(103L)
class Logger extends Serializable {
  var nbBufferedLines = 7
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
