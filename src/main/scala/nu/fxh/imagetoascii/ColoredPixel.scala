package nu.fxh.imagetoascii

import cats.kernel.Monoid
import nu.fxh.asciiFromBrightness
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

  implicit val monoid: Monoid[ColoredPixel] = new Monoid[ColoredPixel] {
    override def empty: ColoredPixel = ColoredPixel(0, 0, 0)

    override def combine(a: ColoredPixel, b: ColoredPixel): ColoredPixel =
      ColoredPixel(a.red + b.red, a.green + b.green, a.blue + b.blue)
  }

  implicit val brightness: Brightness[ColoredPixel] = (value: ColoredPixel, ratio: Double) =>
    ColoredPixel((value.red * ratio).toInt, (value.green * ratio).toInt, (value.blue * ratio).toInt)

  def fromRgbInt(rgbInt: Int): ColoredPixel = {
    import java.awt.Color
    val color = new Color(rgbInt)

    ColoredPixel(color.getRed, color.getGreen, color.getBlue)
  }

}
