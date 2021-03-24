package rendering

import scala.collection._
import java.awt.{Graphics2D, Dimension}
import java.awt.Color
import input_handling._
import scala.collection.mutable.ArrayBuffer

abstract class Menu {
  val items = new mutable.ArrayBuffer[(String, String)]
  val hasImage: Boolean = false
  var cursorIndex: Int = 0
  val 
}

class SplashScreen(val menu: Menu, var color: Color = new Color(255, 168, 100)) {
  val selectedColor = new Color(255, 200, 200)

  def onPaint(
      g: Graphics2D,
      origin: (Int, Int),
      size: (Int, Int),
      ui: UI
  ) {
    g.setColor(color)
    g.fillRect(origin._1, origin._2, size._1, size._2)
    var yNext = origin._2
    var currentIndex = 0
    menu.items.foreach(s =>
      if (yNext < origin._2 + size._2) {
        if (currentIndex == menu.cursorIndex)
          yNext = StringRenderer.drawString(g, ">> " + s._1 + "\n", (origin._1, yNext), selectedColor)
        else
          yNext = StringRenderer.drawString(g, s._1 + "\n", (origin._1, yNext))
      }
    )
  }
}
