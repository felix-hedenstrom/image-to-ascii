package nu.fxh.imagetoascii

import nu.fxh.imagetoascii.Pixel.{AsciiPixel, ColoredPixel, GrayscalePixel}
import zio.test._
import zio.{Scope, ZIO}

import java.awt.Color

object PixelSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Pixel")(
    suite("AsciiPixel")(
      test("convert from grayscale")(
        assertTrue(
          AsciiPixel.fromGrayscale(GrayscalePixel(255)).value == '$'
        )
      ),
      test("convert any grayscale")(
        check(Gen.int.map(GrayscalePixel.apply)) { pixel =>
          ZIO.attempt(AsciiPixel.fromGrayscale(pixel)).either.map(result => assertTrue(result.isRight))
        }
      )
    ),
    suite("ColoredPixel")(
      test("convert a java RGB int to a colored pixel")(
        assertTrue(
          ColoredPixel.fromRgbInt(Color.blue.getRGB) == ColoredPixel(0, 0, 255)
        )
      )
    )
  )
}
