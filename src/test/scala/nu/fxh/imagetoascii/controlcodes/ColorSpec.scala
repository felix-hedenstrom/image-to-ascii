package nu.fxh.imagetoascii.controlcodes

import nu.fxh.imagetoascii.controlcodes.Color.ColorCombination
import zio.Scope
import zio.test.TestAspect.withLiveRandom
import zio.test.{Gen, Spec, TestEnvironment, ZIOSpecDefault, assertTrue, check}

object ColorSpec extends ZIOSpecDefault {

  val genColor: Gen[Any, Color] = Gen.elements(Color.colors.toList: _*)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Color")(
    test("be the closest colors to themselves")(
      check(genColor) { color =>
        assertTrue(Color.closestColorsForPixel(color.coloredPixel).head == color)
      }
    ),
    test("be the closest color combination to themselves")(
      check(genColor) { color =>
        assertTrue(Color.closest(color.coloredPixel) == ColorCombination(color, color))
      }
    ) @@ withLiveRandom
  )
}
