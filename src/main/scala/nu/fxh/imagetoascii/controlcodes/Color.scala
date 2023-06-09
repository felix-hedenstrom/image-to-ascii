package nu.fxh.imagetoascii.controlcodes

import nu.fxh.imagetoascii.ColoredPixel

import scala.io.AnsiColor
import scala.math.sqrt

sealed abstract class Color(val foreground: String, val background: String, val coloredPixel: ColoredPixel)

object Color {
  case object Black   extends Color(AnsiColor.BLACK, AnsiColor.BLACK_B, ColoredPixel(0, 0, 0))
  case object Red     extends Color(AnsiColor.RED, AnsiColor.RED_B, ColoredPixel(255, 0, 0))
  case object Green   extends Color(AnsiColor.GREEN, AnsiColor.GREEN_B, ColoredPixel(0, 255, 0))
  case object Yellow  extends Color(AnsiColor.YELLOW, AnsiColor.YELLOW_B, ColoredPixel(255, 255, 0))
  case object Blue    extends Color(AnsiColor.BLUE, AnsiColor.BLUE_B, ColoredPixel(0, 0, 255))
  case object Magenta extends Color(AnsiColor.MAGENTA, AnsiColor.MAGENTA_B, ColoredPixel(255, 0, 255))
  case object Cyan    extends Color(AnsiColor.CYAN, AnsiColor.CYAN_B, ColoredPixel(0, 255, 255))
  case object White   extends Color(AnsiColor.WHITE, AnsiColor.WHITE_B, ColoredPixel(255, 255, 255))

  val colors: Set[Color] = Set(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White)

  def closestColorsForPixel(pixel: ColoredPixel): List[Color] = colors.toList.sortBy { c =>
    val dr = pixel.red - c.coloredPixel.red
    val dg = pixel.green - c.coloredPixel.green
    val db = pixel.blue - c.coloredPixel.blue

    sqrt(dr * dr + dg * dg + db * db)
  }

  def closest(pixel: ColoredPixel): ColorCombination =
    ColorCombination.allCombinations.minBy { colorCombination =>
      val c = colorCombination.asColoredPixel(pixel.luminosity)

      val hueDiff        = pixel.hue.distance(c.hue) / 360.0
      val luminosityDiff = pixel.luminosity - c.luminosity

      Math.sqrt(hueDiff * hueDiff + luminosityDiff * luminosityDiff)
    }

  case class ColorCombination(background: Color, foreground: Color) {
    def asColoredPixel(brightness: Double): ColoredPixel = {
      val backgroundWeight = ColorCombination.backgroundWeight(brightness)
      val foregroundWeight = ColorCombination.foregroundWeight(brightness)
      val total            = backgroundWeight + foregroundWeight

      ColoredPixel(
        ((background.coloredPixel.red * backgroundWeight + foreground.coloredPixel.red * foregroundWeight) / total).toInt,
        ((background.coloredPixel.green * backgroundWeight + foreground.coloredPixel.green * foregroundWeight) / total).toInt,
        ((background.coloredPixel.blue * backgroundWeight + foreground.coloredPixel.blue * foregroundWeight) / total).toInt
      )
    }
  }

  object ColorCombination {
    def backgroundWeight(brightness: Double): Double =
      1 - foregroundWeight(brightness)

    def foregroundWeight(brightness: Double): Double =
      // How many percent of the screen does pixel does the foreground take up?
      brightness * 0.2 + 0.1

    final val allCombinations: List[ColorCombination] =
      colors.toList.flatMap(background => colors.toList.map(foreground => ColorCombination(background, foreground)))
  }

}
