package nu

import nu.fxh.imagetoascii.Image
import nu.fxh.imagetoascii.Pixel.{AsciiPixel, ColoredPixel, GrayscalePixel}

import java.awt.image.BufferedImage

package object fxh {
  def bufferedImageToAscii(bufferedImage: BufferedImage): String = coloredToAscii(
    Image.fromBufferedImage(bufferedImage)
  )

  def coloredToAscii(image: Image[ColoredPixel], maxSize: Option[Int] = None): String =
    grayscaleToAscii(image.mapPixels(GrayscalePixel.fromColored), maxSize = maxSize)

  def grayscaleToAscii(image: Image[GrayscalePixel], maxSize: Option[Int] = None): String = {
    val ratio = maxSize match {
      case Some(value) =>
        value.toDouble / (image.width max image.height)
      case None => 1
    }

    image
      // Always scale height because monospace characters are higher than they are wide
      .scale(scaleWidth = ratio, scaleHeight = ratio * 0.45)
      .mapPixels(AsciiPixel.fromGrayscale)
      .rows
      .map(_.pixels.map(_.value).mkString)
      .mkString("\n")
  }
}
