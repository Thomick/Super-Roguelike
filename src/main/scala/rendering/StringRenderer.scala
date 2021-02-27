package rendering

import java.awt.{Color, Graphics2D}

object StringRenderer {
  val defaultWriteColor = new Color(200, 200, 200)

  // draw multilined strings and return the y-coordinate for the line just after what was written.
  def drawString(
      g: Graphics2D,
      text: String,
      origin: (Int, Int),
      writeColor: Color = defaultWriteColor
  ): Int = {
    return drawIterable(g, text.split("\n"), origin, writeColor)
  }

  def drawIterable(
      g: Graphics2D,
      lines: Iterable[String],
      origin: (Int, Int),
      writeColor: Color = defaultWriteColor
  ): Int = {
    g.setColor(writeColor)
    val size = g.getFont().getSize()
    var y = origin._2
    lines
      .foreach(line => {
        g.drawString(line, origin._1, y)
        y += size
      })
    return y
  }
}
