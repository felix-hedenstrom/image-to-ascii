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
    val scaledImage = maxSize match {
      case Some(value) => image.scaleDimensionToFit(value)
      case None        => image
    }

    scaledImage.mapPixels(AsciiPixel.fromGrayscale).rows.map(_.pixels.map(_.value).mkString).mkString("\n")
  }
}
