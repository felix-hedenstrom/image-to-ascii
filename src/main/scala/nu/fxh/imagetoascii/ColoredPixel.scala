package nu.fxh.imagetoascii

import nu.fxh.imagetoascii.ColoredPixel.{blueWeight, greenWeight, redWeight}
import nu.fxh.imagetoascii.controlcodes.{Color, ColorCombination, Control}

case class ColoredPixel(red: Int, green: Int, blue: Int) {
  // https://donatbalipapp.medium.com/colours-maths-90346fb5abda
  def luminosity: Double =
    (highestPixel + lowestPixel) / 2.0

  def weightedGrayscaleValue: Int = (red * redWeight + green * greenWeight + blue * blueWeight).toInt

  def highestPixel: Double = List(red, green, blue).max / 255.0

  def lowestPixel: Double = List(red, green, blue).min / 255.0

  def hue: Option[Hue] = Hue.fromColoredPixel(this)

  def saturation: Double = if (luminosity == 1) 0
  else {
    (highestPixel - lowestPixel) / (1 - (2 * luminosity - 1).abs)
  }

  def toAscii(charTransformationType: CharTransformationType): String = {

    val char = asciiFromBrightness(luminosity)

    val colorCombination: Option[ColorCombination] = charTransformationType match {
      case CharTransformationType.Grayscale               => None
      case CharTransformationType.MatchByHue              => Some(ColorCombination.closestByHue(this))
      case CharTransformationType.MatchByWeightedDistance => Some(ColorCombination.closestColorsWeighted(this))
    }

    colorCombination match {
      case Some(ColorCombination(background, foreground)) =>
        s"${background.background}${foreground.foreground}$char${Control.Reset.value}"
      case None =>
        char.toString
    }
  }
}

object ColoredPixel {

  // https://web.archive.org/web/20100316195057/http://www.dfanning.com/ip_tips/color2gray.html
  final val redWeight: Double   = 0.3
  final val greenWeight: Double = 0.59
  final val blueWeight: Double  = 0.11

  def fromRgbInt(rgbInt: Int): ColoredPixel = {
    import java.awt.Color
    val color = new Color(rgbInt)

    ColoredPixel(color.getRed, color.getGreen, color.getBlue)
  }

}
