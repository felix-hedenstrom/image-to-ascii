package nu.fxh.imagetoascii

import nu.fxh.imagetoascii.Image.ImageRow
import cats.Monoid

import java.awt.image.BufferedImage

case class Image(rows: Vector[ImageRow]) {
  def width: Int = rows.headOption.map(_.pixels.size).getOrElse(0)

  def height: Int = rows.size

  def get(x: Int, y: Int): Option[ColoredPixel] = for {
    row   <- rows.lift(y)
    pixel <- row.pixels.lift(x)
  } yield pixel

  def scale(scaleWidth: Double, scaleHeight: Double)(implicit
    monoid: Monoid[ColoredPixel],
    brightness: Brightness[ColoredPixel]
  ): Image =
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

            Brightness[ColoredPixel].adjustBrightness(Monoid[ColoredPixel].combineAll(subpixels), 1.0 / subpixels.size)
          }.toVector)
        )
        .toVector
    )

  def mapPixels(f: ColoredPixel => ColoredPixel): Image =
    Image(rows.map(row => ImageRow(row.pixels.map(f))))
}

object Image {
  case class ImageRow(pixels: Vector[ColoredPixel])

  def fromBufferedImage(bufferedImage: BufferedImage): Image =
    Image(
      (bufferedImage.getMinY until bufferedImage.getHeight)
        .map(y =>
          ImageRow(
            (bufferedImage.getMinX until bufferedImage.getWidth)
              .map(x => ColoredPixel.fromRgbInt(bufferedImage.getRGB(x, y)))
              .toVector
          )
        )
        .toVector
    )

}
