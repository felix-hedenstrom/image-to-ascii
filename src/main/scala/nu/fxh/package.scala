package nu

import nu.fxh.imagetoascii.Image
import nu.fxh.imagetoascii.Pixel.{AsciiPixel, ColoredPixel, GrayscalePixel}


package object fxh {
  def coloredToAscii(image: Image[ColoredPixel], maxSize: Option[Int] = None): String =
    grayscaleToAscii(Image.updatePixels(image, GrayscalePixel.fromColored), maxSize = maxSize)

  def grayscaleToAscii(image: Image[GrayscalePixel], maxSize: Option[Int] = None): String = {
    val scaledImage = maxSize match {
      case Some(value) => image.scaleDimensionToFit(value)
      case None => image
    }

    Image.updatePixels(scaledImage, AsciiPixel.fromGrayscale).rows.map(_.pixels.map(_.value).mkString).mkString("\n")
  }
}
