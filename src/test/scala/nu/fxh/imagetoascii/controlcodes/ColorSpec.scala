package nu.fxh.imagetoascii.controlcodes

import zio.Scope
import zio.test.{Gen, Spec, TestEnvironment, ZIOSpecDefault, assertTrue, check}

object ColorSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Color")(
    test("be the closest colors to themselves")(
      check(Gen.elements(Color.colors.toList: _*)) { color =>
        assertTrue(Color.closestColorsForPixel(color.coloredPixel).head == color)
      }
    )
  )
}
