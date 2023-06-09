package nu.fxh.imagetoascii

// A value between 0-360 representing the hue of a pixel
// https://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
case class Hue(value: Double) {
  // Distance between hues. 0 meaning the same hue, 1 meaning completely different hues
  def distance(other: Hue): Double = {
    val diff = (value - other.value).abs

    diff min (360 - diff)
  }
}

object Hue {
  def fromColoredPixel(pixel: ColoredPixel): Option[Hue] = {
    val R = pixel.red / 255.0
    val G = pixel.green / 255.0
    val B = pixel.blue / 255.0

    val highest = List(R, G, B).max
    val lowest  = List(R, G, B).min

    if (highest == lowest)
      None
    else {

      val degrees = (if (R == highest)
                       (G - B) / (highest - lowest)
                     else if (G == highest)
                       2.0 + (B - R) / (highest - lowest)
                     else
                       4.0 + (R - G) / (highest - lowest)) * 60

      Some(
        Hue(
          if (degrees < 0)
            degrees + 360
          else
            degrees
        )
      )
    }
  }

}
