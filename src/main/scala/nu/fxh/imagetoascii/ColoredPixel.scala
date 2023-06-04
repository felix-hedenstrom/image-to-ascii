package nu.fxh.imagetoascii

import cats.kernel.Monoid
import nu.fxh.asciiFromBrightness
import nu.fxh.imagetoascii.controlcodes.{Color, Control}

case class ColoredPixel(red: Int, green: Int, blue: Int) {
  def brightness: Int = ((red + green + blue) / 3.0).toInt

  def toAscii(withColor: Boolean): String = {

    val char = asciiFromBrightness(brightness)

    if (withColor) {
      val closest :: secondClosest :: _ = Color.closestColorsForPixel(this)

      s"${closest.background}${secondClosest.foreground}$char${Control.Reset.value}"
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
