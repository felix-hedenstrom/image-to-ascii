package nu.fxh.imagetoascii

import nu.fxh.imagetoascii.controlcodes.Color
import zio.Scope
import zio.test._

import java.awt.{Color => JavaColor}

object ColoredPixelSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ColoredPixel")(
    suite("ColoredPixel")(
      test("convert a java RGB int to a colored pixel")(
        assertTrue(
          ColoredPixel.fromRgbInt(JavaColor.blue.getRGB) == ColoredPixel(0, 0, 255)
        )
      ),
      test("convert and keep colors") {
        val pixel = ColoredPixel.fromRgbInt(JavaColor.blue.getRGB)

        println(pixel.toAscii(withColor = true))

        assertTrue(pixel.closestAnsiColor == Color.Black)
      }
    )
  )
}
