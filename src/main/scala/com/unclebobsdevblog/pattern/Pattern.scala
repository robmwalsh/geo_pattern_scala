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

  trait BasicPattern extends Pattern {
    val template: Seq[(Double, Double)]

    def translate(x: Double, y: Double): (Double, Double)

    def drawPattern(): Unit = {
      graphicsContext2D.setStroke(STROKE_COLOR.opacity(STROKE_OPACITY))
      for (y <- 0 to yMax) {
        for (x <- 0 to xMax) {
          val index = (y * 6 + x % 6) % 36 + 1
          val value = hexVal(index)
          graphicsContext2D.setFill(fillColor(value).opacity(fillOpacity(value)))
          val translation = translate(x, y)
          val shape = template.map(point => (point._1 + translation._1, point._2 + translation._2))
          if (fill) graphicsContext2D.fillPolygon(shape)
          if (stroke) graphicsContext2D.strokePolygon(shape)
        }
      }
    }
  }

  trait Pattern extends Canvas {
    //TODO check the string is actually a hash
    val hash: String
    val baseColor: Color

    def xMax: Int

    def yMax: Int

    val fill = true
    val stroke = true

    private val hueOffset = rescale(hexVal(14, 3), (0, 4095), (0, 359))
    private val satOffset = hexVal(17)

    val backgroudColor = Color.hsb(
      //hue
      ((baseColor.hue * 360 - hueOffset) % 360),
      //saturation
      if (satOffset % 2 == 0)
        Math.min(1, ((baseColor.saturation * 100) + satOffset) / 100)
      else
        Math.max(0, ((baseColor.saturation * 100) - satOffset) / 100)
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

  def apply(hash: String,
            baseColor: Color = Color.web("#933c3c")): Pattern = {
    new NestedSquares(hash, baseColor)

  }

  private class Hexagons(val hash: String, val baseColor: Color) extends BasicPattern {
    val scale = hexVal(0)
    val sideLength = rescale(scale, (0, 15), (8, 60))
    val hexHeight = sideLength * Math.sqrt(3)
    val hexWidth = sideLength * 2

    val c: Double = sideLength
    val a: Double = c / 2
    val b: Double = Math.sin(60 * Math.PI / 180) * c

    override def xMax: Int = (width / (hexWidth + sideLength) * 2).intValue() + 1

    override def yMax: Int = (height / hexHeight).intValue() + 1

    override def translate(x: Double, y: Double): (Double, Double) =
      (x * sideLength * 1.5 - hexWidth / 2,
        if (x % 2 == 0) (y - 0.5) * hexHeight else (y - 0.5) * hexHeight + hexHeight / 2)

    val template: Seq[(Double, Double)] = Seq(
      (0, b),
      (a, 0),
      (a + c, 0),
      (2 * c, b),
      (a + c, 2 * b),
      (a, 2 * b),
      (0, b)
    )
  }

  private class Chevrons(val hash: String, val baseColor: Color) extends BasicPattern {
    val chevronWidth = rescale(hexVal(0), (0, 15), (30, 80))
    val chevronHeight = chevronWidth
    val e = chevronHeight * 0.66
    val template: Seq[(Double, Double)] = Seq(
      (chevronWidth / 2, chevronHeight - e),
      (chevronWidth, 0),
      (chevronWidth, e),
      (chevronWidth / 2, chevronHeight),
      (0, e),
      (0, 0),
      (chevronWidth / 2, chevronHeight - e)
    )

    override def xMax = (width / chevronWidth).intValue() + 1

    override def yMax = (height / e).intValue() + 1

    override def translate(x: Double, y: Double): (Double, Double) =
      (x * chevronWidth,
        (y - 0.5) * e)

  }

  private class PlusSigns(val hash: String, val baseColor: Color) extends BasicPattern {
    val squareSize = rescale(hexVal(0), (0, 15), (10, 25))
    val plusSize = squareSize * 3
    val template: Seq[(Double, Double)] = Seq(
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

    override def xMax = (width / squareSize * 2).intValue() + 1

    override def yMax = (height / squareSize * 2).intValue() + 1

    override def translate(x: Double, y: Double): (Double, Double) =
      (x * plusSize - x * squareSize + (if (y % 2 == 0) 0 else squareSize) - squareSize,
        y * plusSize - y * squareSize - plusSize / 2)

  }

  private class Octagons(val hash: String, val baseColor: Color) extends BasicPattern {
    val squareSize = rescale(hexVal(0), (0, 15), (10, 60))
    val s = squareSize
    val c = s * 0.33
    override val template: Seq[(Double, Double)] = Seq(
      (c, 0),
      (s - c, 0),
      (s, c),
      (s, s - c),
      (s - c, s),
      (c, s),
      (0, s - c),
      (0, c),
      (c, 0)
    )

    override def xMax: Int = (width.intValue() / squareSize).intValue()

    override def yMax: Int = (height.intValue() / squareSize).intValue()

    override def translate(x: Double, y: Double): (Double, Double) = (x * squareSize, y * squareSize)
  }

  private class Diamonds(val hash: String, val baseColor: Color) extends BasicPattern {
    val diamondWidth = rescale(hexVal(0), (0, 15), (10, 50))
    val diamondHeight = rescale(hexVal(1), (0, 15), (10, 50))
    override val template: Seq[(Double, Double)] = Seq(
      (diamondWidth / 2, 0),
      (diamondWidth, diamondHeight / 2),
      (diamondWidth / 2, diamondHeight),
      (0, diamondHeight / 2),
      (diamondWidth / 2, 0)
    )

    override def xMax: Int = (width.intValue() / diamondWidth * 2).intValue()

    override def yMax: Int = (height.intValue() / diamondHeight * 2).intValue()

    override def translate(x: Double, y: Double): (Double, Double) =
      (x * diamondWidth / 2 + (if (y % 2 == 0) 0 else diamondWidth / 2),
        (y - 1) * diamondHeight / 2)
  }

  private class Squares(val hash: String, val baseColor: Color) extends BasicPattern {

    val squareSize = rescale(hexVal(0), (0, 15), (10, 60))

    override val template: Seq[(Double, Double)] = Seq(
      (0, 0),
      (0, squareSize),
      (squareSize, squareSize),
      (squareSize, 0),
      (0, 0)
    )

    override def xMax: Int = (width.intValue() / squareSize).intValue()

    override def yMax: Int = (height.intValue() / squareSize).intValue()

    override def translate(x: Double, y: Double): (Double, Double) =
      (x * squareSize,
        y * squareSize)
  }

  private class OverlappingCircles(val hash: String, val baseColor: Color) extends Pattern {
    var scale = hexVal(0)
    var diameter = rescale(scale, (0, 15), (25, 200))
    var radius = diameter / 2

    override def xMax: Int = (width.intValue() / radius).intValue() + 1

    override def yMax: Int = (height.intValue() / radius).intValue() + 1

    override def drawPattern(): Unit = {
      graphicsContext2D.setStroke(STROKE_COLOR.opacity(STROKE_OPACITY))
      for (y <- 0 to yMax) {
        for (x <- 0 to xMax) {
          val index = (y * 6 + x % 6) % 36 + 1
          val value = hexVal(index)
          graphicsContext2D.setFill(fillColor(value).opacity(fillOpacity(value)))
          if (fill) graphicsContext2D.fillOval((x - 1) * radius, (y - 1) * radius, diameter, diameter)
          if (stroke) graphicsContext2D.strokeOval((x - 1) * radius, (y - 1) * radius, diameter, diameter)
        }
      }
    }
  }

  private class OverlappingRings(val hash: String, val baseColor: Color) extends Pattern {
    var scale = hexVal(0)
    var ringSize = rescale(scale, (0, 15), (10, 60))
    var strokeWidth = ringSize / 4

    override def xMax: Int = (width.intValue() / ringSize).intValue() + 1

    override def yMax: Int = (height.intValue() / ringSize).intValue() + 1

    override val fill: Boolean = false

    override def drawPattern(): Unit = {
      graphicsContext2D.lineWidth = ringSize / 4
      for (y <- 0 to yMax) {
        for (x <- 0 to xMax) {
          val index = (y * 6 + x % 6) % 36 + 1
          val value = hexVal(index)
          graphicsContext2D.setStroke(fillColor(value).opacity(fillOpacity(value)))
          if (stroke) graphicsContext2D.strokeOval((x - 0.5) * ringSize, (y - 0.5) * ringSize,
            ringSize * 2 - strokeWidth, ringSize * 2 - strokeWidth)
        }
      }
    }
  }

  private class ConcentricCircles(val hash: String, val baseColor: Color) extends Pattern {
    var scale = hexVal(0)
    var ringSize = rescale(scale, (0, 15), (10, 60))
    var strokeWidth = ringSize / 5

    override def xMax: Int = (width.intValue() / ringSize).intValue() + 1

    override def yMax: Int = (height.intValue() / ringSize).intValue() + 1

    override val fill: Boolean = false

    override def drawPattern(): Unit = {
      graphicsContext2D.lineWidth = strokeWidth
      for (y <- 0 to yMax) {
        for (x <- 0 to xMax) {
          val index = (y * 6 + x % 6) % 36 + 1
          val value = (hexVal(index), hexVal(39 - index))

          graphicsContext2D.setStroke(fillColor(value._1).opacity(fillOpacity(value._2)))
          graphicsContext2D.strokeOval((x - 0.5) * (ringSize + strokeWidth), (y - 0.5) * (ringSize + strokeWidth),
            ringSize, ringSize)

          graphicsContext2D.setFill(fillColor(value._2).opacity(fillOpacity(value._2)))
          graphicsContext2D.fillOval((x - 0.5) * (ringSize + strokeWidth) + strokeWidth, (y - 0.5) * (ringSize + strokeWidth) + strokeWidth,
            (ringSize + strokeWidth) / 2, (ringSize + strokeWidth) / 2)

        }
      }
    }
  }

  private class NestedSquares(val hash: String, val baseColor: Color) extends Pattern {
    var blockSize = rescale(hexVal(0), (0, 15), (4, 12))
    var squareSize = blockSize * 7

    override def xMax: Int = (width.intValue() / (blockSize + squareSize)).intValue() + 1

    override def yMax: Int = (height.intValue() / (blockSize + squareSize)).intValue() + 1

    override def drawPattern(): Unit = {
      graphicsContext2D.lineWidth = blockSize
      for (y <- 0 to yMax) {
        for (x <- 0 to xMax) {
          val index = (y * 6 + x % 6) % 36 + 1
          val value = (hexVal(index), hexVal(39 - index))

          graphicsContext2D.setStroke(fillColor(value._1).opacity(fillOpacity(value._2)))
          graphicsContext2D.strokeRect((x - 0.5) * (blockSize * 2 + squareSize) + blockSize, (y - 0.5) * (blockSize * 2 + squareSize) + blockSize,
            squareSize, squareSize)

          graphicsContext2D.setStroke(fillColor(value._2).opacity(fillOpacity(value._2)))
          graphicsContext2D.strokeRect((x - 0.5) * (blockSize * 2 + squareSize) + blockSize * 3, (y - 0.5) * (blockSize * 2 + squareSize) + blockSize * 3,
            blockSize *3, blockSize *3)

        }
      }
    }
  }

}