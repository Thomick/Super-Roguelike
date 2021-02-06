package Rendering

import java.awt.{Color, Graphics2D}

object StringRenderer {
  val defaultWriteColor = new Color(200, 200, 200)

  def drawString(
      g: Graphics2D,
      text: String,
      origin: (Int, Int),
      writeColor: Color = defaultWriteColor
  ) {
    g.setColor(writeColor)
    val size = g.getFont().getSize()
    var y = origin._2
    text
      .split("\n")
      .foreach(line => {
        g.drawString(line, origin._1, y)
        y += size
      })
  }
}
