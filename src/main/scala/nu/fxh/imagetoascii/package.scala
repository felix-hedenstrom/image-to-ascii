package nu.fxh

import java.awt.image.BufferedImage

package object imagetoascii {
  def bufferedImageToAscii(bufferedImage: BufferedImage, maxSize: Option[Int], useColor: Boolean = false): String =
    imageToAscii(
      Image.fromBufferedImage(bufferedImage),
      maxSize,
      useColor
    )

  def imageToAscii(image: Image, maxSize: Option[Int] = None, useColor: Boolean = false): String = {
    val ratio = maxSize match {
      case Some(value) =>
        value.toDouble / (image.width max image.height)
      case None => 1
    }

    image
      // Always scale height because monospace characters are higher than they are wide
      .scale(scaleWidth = ratio, scaleHeight = ratio * 0.45)
      .rows
      .map(_.pixels.map(_.toAscii(withColor = useColor)).mkString)
      .mkString("\n")
  }

  def asciiFromBrightness(brightness: Double): Char = {
    val index = (brightness * (brightnessSortedAsciiChars.size - 1)).toInt
    brightnessSortedAsciiChars.lift(index).getOrElse(brightnessSortedAsciiChars.head)
  }

  val brightnessSortedAsciiChars =
    """ `.-':_,^=;><+!rc*/z?sLTv)J7(|Fi{C}fI31tlu[neoZ5Yxjya]2ESwqkP6h9d4VpOGbUAKXHm8RD#$Bg0MNWQ%&@""".toCharArray
}
