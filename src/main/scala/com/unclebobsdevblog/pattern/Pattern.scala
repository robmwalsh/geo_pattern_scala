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

    //only need to draw when the canvas is getting bigger
    width.onChange((_, oldVal, newVal) => if (newVal.intValue > oldVal.intValue) draw())
    height.onChange((_, oldVal, newVal) => if (newVal.intValue > oldVal.intValue) draw())
  }

  def apply(hash: String = "f3da29ce23e96dc8b38df6ab3b6aaf7995cc581a",
            baseColor: Color = Color.web("#933c3c")): Pattern = {
    new PlusSigns(hash, baseColor)

  }

  private class Hexagons(val hash: String, val baseColor: Color) extends Pattern {
    val scale = hexVal(0)
    val sideLength = rescale(scale, (0, 15), (8, 60))
    val hexHeight = sideLength * Math.sqrt(3)
    val hexWidth = sideLength * 2

    val c: Double = sideLength
    val a: Double = c / 2
    val b: Double = Math.sin(60 * Math.PI / 180) * c

    val hexTemplate: Seq[(Double, Double)] = Seq(
      (0, b),
      (a, 0),
      (a + c, 0),
      (2 * c, b),
      (a + c, 2 * b),
      (a, 2 * b),
      (0, b)
    )

    def drawPattern() = {
      graphicsContext2D.setStroke(STROKE_COLOR.opacity(STROKE_OPACITY))
      val xMax = (width / (hexWidth + sideLength) * 2).intValue() + 1
      val yMax = (height / hexHeight).intValue() + 1
      for (y <- 0 to yMax) {
        for (x <- 0 to xMax) {
          val index = (y * 6 + x % 6) % 36 + 1
          val value = hexVal(index)
          graphicsContext2D.setFill(fillColor(value).opacity(fillOpacity(value)))
          val dy = if (x % 2 == 0) (y - 0.5) * hexHeight else (y - 0.5) * hexHeight + hexHeight / 2
          val dx = x * sideLength * 1.5 - hexWidth / 2
          val hex = hexTemplate.map(point => (point._1 + dx, point._2 + dy))
          graphicsContext2D.fillPolygon(hex)
          graphicsContext2D.strokePolygon(hex)
        }
      }
    }
  }

  private class Chevrons(val hash: String, val baseColor: Color) extends Pattern {
    val chevronWidth = rescale(hexVal(0), (0, 15), (30, 80))
    val chevronHeight = chevronWidth
    val e = chevronHeight * 0.66
    val chevronTemplate: Seq[(Double, Double)] = Seq(
      (chevronWidth / 2, chevronHeight - e),
      (chevronWidth, 0),
      (chevronWidth, e),
      (chevronWidth / 2, chevronHeight),
      (0, e),
      (0, 0),
      (chevronWidth / 2, chevronHeight - e)
    )

    def drawPattern() = {
      graphicsContext2D.setStroke(STROKE_COLOR.opacity(STROKE_OPACITY))
      val xMax = (width / chevronWidth).intValue() + 1
      val yMax = (height / e).intValue() + 1
      for (y <- 0 to yMax) {
        for (x <- 0 to xMax) {
          val index = (y * 6 + x % 6) % 36 + 1
          val value = hexVal(index)
          graphicsContext2D.setFill(fillColor(value).opacity(fillOpacity(value)))
          val dy = (y - 0.5) * e
          val dx = x * chevronWidth
          val chevron = chevronTemplate.map(point => (point._1 + dx, point._2 + dy))
          graphicsContext2D.fillPolygon(chevron)
          graphicsContext2D.strokePolygon((chevronWidth / 2 + dx, chevronHeight + dy) +: chevron)
        }
      }
    }
  }

  private class PlusSigns(val hash: String, val baseColor: Color) extends Pattern {
    val squareSize = rescale(hexVal(0), (0, 15), (10, 25))
    val plusSize = squareSize * 3
    val plusTemplate: Seq[(Double, Double)] = Seq(
      (1 * squareSize, 0),
      (2 * squareSize, 0),
      (2 * squareSize, 1 * squareSize),
      (3 * squareSize, 1 * squareSize),
      (3 * squareSize, 2 * squareSize),
      (2 * squareSize, 2 * squareSize),
      (2 * squareSize, 3 * squareSize),
      (1 * squareSize, 3 * squareSize),
      (1 * squareSize, 2 * squareSize),
      (0 * squareSize, 2 * squareSize),
      (0 * squareSize, 1 * squareSize),
      (1 * squareSize, 1 * squareSize),
      (1 * squareSize, 0 * squareSize),
    )

    def drawPattern() = {
      graphicsContext2D.setStroke(STROKE_COLOR.opacity(STROKE_OPACITY))
      val xMax = (width / squareSize * 2).intValue() + 1
      val yMax = (height / squareSize * 2).intValue() + 1
      for (y <- 0 to yMax) {
        for (x <- 0 to xMax) {
          val index = (y * 6 + x % 6) % 36 + 1
          val value = hexVal(index)
          graphicsContext2D.setFill(fillColor(value).opacity(fillOpacity(value)))
          val dy = y * plusSize - y * squareSize - plusSize / 2
          val dx = x * plusSize - x * squareSize + (if (y % 2 == 0) 0 else squareSize) - squareSize
          val plus = plusTemplate.map(point => (point._1 + dx, point._2 + dy))
          graphicsContext2D.fillPolygon(plus)
          graphicsContext2D.strokePolygon(plus)
        }
      }
    }
  }

}