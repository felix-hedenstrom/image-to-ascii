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

  def scale(scaleWidth: Double, scaleHeight: Double)(implicit monoid: Monoid[P], brightness: Brightness[P]): Image[P] =
    Image(
      (0 until (scaleHeight * height).ceil.toInt)
        .map(y =>
          ImageRow((0 until (scaleWidth * width).ceil.toInt).map { x =>
            // average pixels
            val subpixels = (0 until (1 / scaleHeight).toInt).flatMap(dy =>
              (0 until (1 / scaleWidth).toInt).flatMap(dx =>
                get((x / scaleWidth).toInt + dx, (y / scaleHeight).toInt + dy)
              )
            )

            Brightness[P].adjustBrightness(Monoid[P].combineAll(subpixels), 1.0 / subpixels.size)
          }.toVector)
        )
        .toVector
    )

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
