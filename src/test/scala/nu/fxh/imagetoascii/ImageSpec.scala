package nu.fxh.imagetoascii

import zio.Scope
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

object ImageSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Image")(
    test("scale correctly") {

      val image =
        Image(
          Vector(
            Image.ImageRow(
              Vector(
                ColoredPixel(0, 127, 0),
                ColoredPixel(0, 127, 255),
                ColoredPixel(255, 0, 255),
                ColoredPixel(0, 255, 255)
              )
            )
          )
        )

      assertTrue(
        image.scale(scaleWidth = 0.5, scaleHeight = 0.5) == Image(
          Vector(Image.ImageRow(Vector(ColoredPixel(0, 127, 127), ColoredPixel(127, 127, 255))))
        )
      )
    }
  )
}
