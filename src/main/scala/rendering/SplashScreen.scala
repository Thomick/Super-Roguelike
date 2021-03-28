package rendering

import scala.collection._
import java.awt.{Graphics2D, Dimension}
import java.awt.Color
import input_handling._
import scala.collection.mutable.ArrayBuffer

object SplashScreenRenderer {
  val selectedColor = new Color(255, 200, 200)
  val backgroundColor = new Color(44, 95, 95)
  val borderColor = new Color(30, 80, 80)
  val padding = 10

  def drawMenu(
      g: Graphics2D,
      origin: (Int, Int),
      size: (Int, Int),
      menu: Menu
  ) {
    g.setColor(backgroundColor)
    g.fillRect(origin._1, origin._2, size._1, size._2)
    g.setColor(borderColor)
    g.drawRect(origin._1, origin._2, size._1, size._2)
    var yNext = origin._2 + 2 * padding
    var currentIndex = 0
    yNext = StringRenderer.drawString(g, menu.name + "\n", (origin._1 + (size._1 - menu.name.size * 5) / 2, yNext))
    yNext += padding
    menu.items.foreach(s =>
      if (yNext < origin._2 + size._2) {
        if (currentIndex == menu.cursorIndex)
          yNext = StringRenderer.drawString(g, ">> " + s._1 + "\n", (origin._1 + padding, yNext), selectedColor)
        else
          yNext = StringRenderer.drawString(g, s._1 + "\n", (origin._1 + padding, yNext))
        currentIndex += 1
      }
    )
  }
}
