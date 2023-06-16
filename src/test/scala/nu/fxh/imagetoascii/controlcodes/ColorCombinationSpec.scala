package nu.fxh.imagetoascii.controlcodes

import zio.Scope
import zio.test.TestAspect.withLiveRandom
import zio.test._

object ColorCombinationSpec extends ZIOSpecDefault {

  val genColor: Gen[Any, Color] = Gen.elements(Color.colors.toList: _*)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ColorCombination")(
    test("be the closest colors to themselves")(
      check(genColor) { color =>
        assertTrue(ColorCombination.closestColorsWeighted(color.coloredPixel) == ColorCombination(color, color))
      }
    ),
    test("be the closest color combination to themselves")(
      check(genColor) { color =>
        assertTrue(ColorCombination.closestByHue(color.coloredPixel) == ColorCombination(color, color))
      }
    ) @@ withLiveRandom
  )
}
