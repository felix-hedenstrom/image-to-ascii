package nu.fxh.imagetoascii

import nu.fxh.imagetoascii.controlcodes.Color.ColorCombination
import nu.fxh.imagetoascii.controlcodes.{Color, Control}

case class ColoredPixel(red: Int, green: Int, blue: Int) {
  // https://donatbalipapp.medium.com/colours-maths-90346fb5abda
  def luminosity: Double =
    (highestPixel + lowestPixel) / 2.0

  def highestPixel: Double = List(red, green, blue).max / 255.0
  def lowestPixel: Double  = List(red, green, blue).min / 255.0

  def hue: Option[Hue] = Hue.fromColoredPixel(this)

  def saturation: Double = if (luminosity == 1) 0 else { (highestPixel - lowestPixel) / (1 - (2 * luminosity - 1).abs) }

  def toAscii(withColor: Boolean): String = {

    val char = asciiFromBrightness(luminosity)

    if (withColor) {
      val ColorCombination(background, foreground) = Color.closest(this)

      s"${background.background}${foreground.foreground}$char${Control.Reset.value}"

    } else
      char.toString

  }
}

object ColoredPixel {

  def fromRgbInt(rgbInt: Int): ColoredPixel = {
    import java.awt.Color
    val color = new Color(rgbInt)

    ColoredPixel(color.getRed, color.getGreen, color.getBlue)
  }

}
