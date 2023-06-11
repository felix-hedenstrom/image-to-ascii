package nu.fxh.imagetoascii

import nu.fxh.imagetoascii.Image.ImageRow

import java.awt.image.BufferedImage

case class Image(rows: Vector[ImageRow]) {
  def width: Int = rows.headOption.map(_.pixels.size).getOrElse(0)

  def height: Int = rows.size

  def get(x: Int, y: Int): Option[ColoredPixel] = for {
    row   <- rows.lift(y)
    pixel <- row.pixels.lift(x)
  } yield pixel

  def scale(scaleWidth: Double, scaleHeight: Double): Image =
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

            val combinedSubpixels = subpixels.foldLeft(ColoredPixel(0, 0, 0)) { case (a, b) =>
              ColoredPixel(a.red + b.red, a.green + b.green, a.blue + b.blue)
            }

            ColoredPixel(
              combinedSubpixels.red / subpixels.size,
              combinedSubpixels.green / subpixels.size,
              combinedSubpixels.blue / subpixels.size
            )
          }.toVector)
        )
        .toVector
    )
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
