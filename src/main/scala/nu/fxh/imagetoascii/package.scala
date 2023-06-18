package nu.fxh

import java.awt.image.BufferedImage

package object imagetoascii {
  def bufferedImageToAscii(
    bufferedImage: BufferedImage,
    charTransformationType: CharTransformationType = CharTransformationType.Grayscale,
    maxSize: Option[Int] = Some(50)
  ): String =
    imageToAscii(
      Image.fromBufferedImage(bufferedImage),
      charTransformationType,
      maxSize
    )

  def imageToAscii(
    image: Image,
    charTransformationType: CharTransformationType = CharTransformationType.Grayscale,
    maxSize: Option[Int] = None,
    contrast: Option[Int] = None
  ): String = {
    val ratio = maxSize match {
      case Some(value) =>
        value.toDouble / (image.width max image.height)
      case None => 1
    }

    image
      // Always scale height because monospace characters are higher than they are wide
      .scale(scaleWidth = ratio, scaleHeight = ratio * 0.45)
      .setContrast(contrast.getOrElse(0))
      .rows
      .map(_.pixels.map(_.toAscii(charTransformationType)).mkString)
      .mkString("\n")
  }

  def asciiFromBrightness(brightness: Double): Char = {
    val index = (brightness * (brightnessSortedAsciiChars.size - 1)).toInt
    brightnessSortedAsciiChars.lift(index).getOrElse(brightnessSortedAsciiChars.head)
  }

  final val brightnessSortedAsciiChars =
    """ `.-':_,^=;><+!rc*/z?sLTv)J7(|Fi{C}fI31tlu[neoZ5Yxjya]2ESwqkP6h9d4VpOGbUAKXHm8RD#$Bg0MNWQ%&@""".toCharArray

  final val defaultSize = 50
}
