package nu

import nu.fxh.imagetoascii.Image

import java.awt.image.BufferedImage

package object fxh {
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

  def asciiFromBrightness(brightness: Int): Char = {
    val index = ((brightness / 255.0) * (brightnessSortedAsciiChars.size - 1)).toInt
    brightnessSortedAsciiChars.lift(index).getOrElse('@')
  }

  val brightnessSortedAsciiChars =
    """$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\|()1{}[]?-_+~<>i!lI;:,"^`'.""".reverse.toCharArray
}
