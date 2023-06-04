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

  def closestColorForPixel(pixel: ColoredPixel): Color = colors.minBy { c =>
    val dr = pixel.red - c.coloredPixel.red
    val dg = pixel.green - c.coloredPixel.green
    val db = pixel.blue - c.coloredPixel.blue

    sqrt((dr * dr + dg * dg + db * db))
  }

}
