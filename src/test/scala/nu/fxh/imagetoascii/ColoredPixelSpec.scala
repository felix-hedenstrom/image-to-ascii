package nu.fxh.imagetoascii

import zio.Scope
import zio.test._

import java.awt.{Color => JavaColor}

object ColoredPixelSpec extends ZIOSpecDefault {

  val examplePixel = ColoredPixel(54, 155, 229)
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ColoredPixel")(
    suite("ColoredPixel")(
      test("convert a java RGB int to a colored pixel")(
        assertTrue(
          ColoredPixel.fromRgbInt(JavaColor.blue.getRGB) == ColoredPixel(0, 0, 255)
        )
      ),
      test("calculate luminosity")(
        assertTrue(
          (examplePixel.luminosity * 1000).round == 555
        )
      ),
      test("hue")(
        assertTrue(
          examplePixel.hue.get.value.round == 205L
        )
      ),
      test("saturation")(
        assertTrue((examplePixel.saturation * 100).round == 77)
      )
    )
  )
}
