package com.unclebobsdevblog.pattern

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

object Patterns extends Canvas {

  val FILL_COLOR_DARK = Color.web("#222")
  val FILL_COLOR_LIGHT = Color.web("#ddd")
  val STROKE_COLOR = Color.web("#000")
  val STROKE_OPACITY = 0.02
  val OPACITY_MIN = 0.02
  val OPACITY_MAX = 0.15

  private def fillColor(v: Int) =
    if (v % 2 == 0) FILL_COLOR_LIGHT else FILL_COLOR_DARK

  private def fillOpacity(v: Int): Double =
    rescale(v, (0, 15), (OPACITY_MIN, OPACITY_MAX))

  //Re-maps a value in one range to another
  private def rescale(v: Double, vRange: (Double, Double), rRange: (Double, Double)): Double =
    (v - vRange._1) * (rRange._2 - rRange._1) / (vRange._2 - vRange._1) + rRange._1


  trait Pattern extends Canvas {
    //TODO check the string is actually a hash
    val hash: String
    val baseColor: Color

    private val hueOffset = rescale(hexVal(14, 3), (0, 4095), (0, 359))
    private val satOffset = hexVal(17)

    val backgroudColor = Color.hsb(
      //hue
      (((baseColor.hue * 360 - hueOffset) + 360) % 360) / 360,
      //saturation
      if (satOffset % 2 == 0)
        Math.min(1, ((baseColor.saturation * 100) + satOffset) / 100)
      else
        Math.max(1, ((baseColor.saturation * 100) - satOffset) / 100)
      //brightness
      , baseColor.brightness)

    //Extract a substring from a hex string and parse it as an integer
    final def hexVal(index: Int, len: Int = 1): Int = {
      val substr = hash.substring(index, index + len).toUpperCase
      Integer.parseInt(substr, 16)
    }


    final def drawBackground(): Unit = {
      graphicsContext2D.setFill(backgroudColor)
      graphicsContext2D.fillRect(0d, 0d, width.doubleValue, height.doubleValue)
    }

    def drawPattern(): Unit

    final def draw(): Unit = {
      drawBackground()
      drawPattern()
    }
  }

  def apply(hash: String = "f3da29ce23e96dc8b38df6ab3b6aaf7995cc581a",
            baseColor: Color = Color.web("#933c3c")): Pattern = {
    new Hexagons(hash, baseColor)

  }

  private class Hexagons(val hash: String, val baseColor: Color) extends Pattern {
    val scale = hexVal(0)
    val sideLength = rescale(scale, (0, 15), (8, 60))
    val hexHeight = sideLength * Math.sqrt(3)
    val hexWidth = sideLength * 2

    width = hexWidth * 3 + sideLength * 3
    height = hexHeight * 6

    val c: Double = sideLength
    val a: Double = c / 2
    val b: Double = Math.sin(60 * Math.PI / 180) * c

    val hexTemplate = Seq(
      (0d, b),
      (a, 0d),
      (a + c, 0d),
      (2 * c, b),
      (a + c, 2 * b),
      (a, 2 * b),
      (0d, b)
    )

    def drawPattern() = {
      graphicsContext2D.setStroke(STROKE_COLOR.opacity(STROKE_OPACITY))

      for (y <- 0 to 7) {
        for (x <- 0 to 7) {
          val index = (y * 6 + x % 6) % 36 + 1
          val value = hexVal(index)
          graphicsContext2D.setFill(fillColor(value).opacity(fillOpacity(value)))

          val dy = if (x % 2 == 0) (y - 0.5) * hexHeight else (y - 0.5) * hexHeight + hexHeight / 2
          val dx = x * sideLength * 1.5 - hexWidth / 2
          val hex = hexTemplate.map(point => (point._1 + dx, point._2 + dy))
          graphicsContext2D.fillPolygon(hex)
          graphicsContext2D.strokePolygon(hex)
          graphicsContext2D.setFill(Color.Black)
          //graphicsContext2D.fillText(index.toString, dx + c, dy + c)
        }
      }
    }
  }
}