package nu.fxh.imagetoascii

import nu.fxh.imagetoascii.Image.ImageRow
import nu.fxh.imagetoascii.Pixel.ColoredPixel
import cats.Monoid

import java.awt.image.BufferedImage

case class Image[P <: Pixel](rows: Vector[ImageRow[P]]) {
  def width: Int = rows.headOption.map(_.pixels.size).getOrElse(0)

  def height: Int = rows.size

  def get(x: Int, y: Int): Option[P] = for {
    row   <- rows.lift(y)
    pixel <- row.pixels.lift(x)
  } yield pixel

  def scaleDimensionToFit(n: Int)(implicit monoid: Monoid[P], brightness: Brightness[P]): Image[P] = {
    val ratio = n.toDouble / (width max height)

    Image(
      (0 until (ratio * height).ceil.toInt)
        .map(y =>
          ImageRow((0 until (ratio * width).ceil.toInt).map { x =>
            // average pixels
            val subpixels = (0 until (1 / ratio).toInt).flatMap(dy =>
              (0 until (1 / ratio).toInt).flatMap(dx => get((x / ratio).toInt + dx, (y / ratio).toInt + dy))
            )

            Brightness[P].adjustBrightness(Monoid[P].combineAll(subpixels), (ratio min 1))
          }.toVector)
        )
        .toVector
    )
  }

  def mapPixels[B <: Pixel](f: P => B): Image[B] =
    Image(rows.map(row => ImageRow(row.pixels.map(f))))
}

object Image {
  case class ImageRow[P <: Pixel](pixels: Vector[P])

  def fromBufferedImage(bufferedImage: BufferedImage): Image[ColoredPixel] =
    Image(
      (bufferedImage.getMinY until bufferedImage.getHeight)
        .map(y =>
          ImageRow(
            (bufferedImage.getMinX until bufferedImage.getWidth)
              .map(x => Pixel.ColoredPixel.fromRgbInt(bufferedImage.getRGB(x, y)))
              .toVector
          )
        )
        .toVector
    )

}
