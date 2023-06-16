package nu.fxh.imagetoascii.controlcodes

import nu.fxh.imagetoascii.ColoredPixel
import nu.fxh.imagetoascii.controlcodes.Color.colors

import scala.math.sqrt

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

  def closestColorsWeighted(pixel: ColoredPixel): ColorCombination =
    ColorCombination.allCombinations.minBy { colorCombination =>
      val c = colorCombination.asColoredPixel(pixel.weightedGrayscaleValue / 255.0)

      val grayscaleValueDiff = pixel.weightedGrayscaleValue - c.weightedGrayscaleValue

      val dr = pixel.red - c.red
      val dg = pixel.green - c.green
      val db = pixel.blue - c.blue

      sqrt(
        dr * dr * ColoredPixel.redWeight + dg * dg * ColoredPixel.greenWeight + db * db * ColoredPixel.blueWeight + grayscaleValueDiff * grayscaleValueDiff * 0.5
      )
    }

  def closestByHue(pixel: ColoredPixel): ColorCombination =
    ColorCombination.allCombinations.minBy { colorCombination =>
      val c = colorCombination.asColoredPixel(pixel.luminosity)

      val luminosityDiff = pixel.luminosity - c.luminosity
      val saturationDiff = pixel.saturation - c.saturation

      val hueDiff: Double = pixel.hue match {
        case Some(value) =>
          c.hue.map(_.distance(value) / 360.0).getOrElse(0.5)
        case None =>
          c.hue.fold(0.0)(_ => 0.5)
      }

      Math.sqrt(hueDiff * hueDiff + luminosityDiff * luminosityDiff)
    }

  def backgroundWeight(brightness: Double): Double =
    1 - foregroundWeight(brightness)

  def foregroundWeight(brightness: Double): Double =
    // How many percent of the screen does pixel does the foreground take up?
    brightness * 0.2 + 0.1

  final val allCombinations: List[ColorCombination] =
    colors.toList.flatMap(background => colors.toList.map(foreground => ColorCombination(background, foreground)))

}
