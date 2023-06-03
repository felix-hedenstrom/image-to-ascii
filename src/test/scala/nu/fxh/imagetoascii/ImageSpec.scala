package nu.fxh.imagetoascii

import nu.fxh.imagetoascii.Pixel.GrayscalePixel
import zio.Scope
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

object ImageSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Image")(
    test("scale correctly") {

      val image =
        Image(
          Vector(
            Image.ImageRow(Vector(GrayscalePixel(0), GrayscalePixel(255), GrayscalePixel(255), GrayscalePixel(255)))
          )
        )

      assertTrue(
        image.scaleDimensionToFit(2) == Image(Vector(Image.ImageRow(Vector(GrayscalePixel(127), GrayscalePixel(255)))))
      )
    }
  )
}
