package nu.fxh.imagetoascii

import cats.Monad
import cats.kernel.Monoid
import cats.syntax

sealed trait Pixel

object Pixel {
  case class GrayscalePixel(value: Int) extends Pixel

  object GrayscalePixel {
    def fromColored(pixel: ColoredPixel): GrayscalePixel = GrayscalePixel((pixel.blue + pixel.red + pixel.green) / 3)

    implicit val monoid: Monoid[GrayscalePixel] = new Monoid[GrayscalePixel] {
      override def empty: GrayscalePixel = GrayscalePixel(0)

      override def combine(x: GrayscalePixel, y: GrayscalePixel): GrayscalePixel = GrayscalePixel(x.value + y.value)
    }

    implicit val brightness: Brightness[GrayscalePixel] = (pixel, ratio) => GrayscalePixel((pixel.value * ratio).toInt)
  }

  case class ColoredPixel(red: Int, green: Int, blue: Int) extends Pixel

  object ColoredPixel {
    def fromRgbInt(rgbInt: Int): ColoredPixel = {
      import java.awt.Color
      val color = new Color(rgbInt)

      ColoredPixel(color.getRed, color.getGreen, color.getBlue)
    }
  }

  case class AsciiPixel(value: Char) extends Pixel

  object AsciiPixel {
    def fromGrayscale(pixel: GrayscalePixel): AsciiPixel = {
      val index = ((pixel.value.min(255).max(0) / 255.0) * (sortedChars.size - 1)).toInt

      AsciiPixel(sortedChars(index))
    }

    val sortedChars =
      """$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\|()1{}[]?-_+~<>i!lI;:,"^`'.""".reverse.toCharArray
  }

}
