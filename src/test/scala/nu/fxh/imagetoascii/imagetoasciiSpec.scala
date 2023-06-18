package nu.fxh.imagetoascii

import zio.test.{Gen, Spec, TestEnvironment, ZIOSpecDefault, assertTrue, check}
import zio.{Scope, ZIO}

import java.io.File
import java.net.URL
import javax.imageio.ImageIO

object imagetoasciiSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("imagetoascii")(
    test("convert the turtle image to ascii")(
      readImage("turtle.png").map { image =>
        val size = 100

        val ascii = imageToAscii(image, CharTransformationType.MatchByHue, maxSize = Some(size), contrast = Some(-10))

        println(ascii)

        assertTrue((ascii.length max ascii.split("\n").head.length) == size)
      }
    ),
    test("convert the alphabet image to ascii")(
      readImage("alphabet.jpg").map { image =>
        val size  = 120
        val ascii = imageToAscii(image, maxSize = Some(size))
        println(ascii)

        assertTrue((ascii.length max ascii.split("\n").head.length) == size)
      }
    ),
    test("convert the xp image including color")(
      readImage("xp.jpg").map { image =>
        val size  = 120
        val ascii = imageToAscii(image, CharTransformationType.MatchByHue, maxSize = Some(size), contrast = Some(-28))
        println(ascii)

        assertTrue((ascii.length max ascii.split("\n").head.length) == size)
      }
    ),
    test("download online image and translate it")(
      for {
        bufferedImage <-
          ZIO.attempt(
            ImageIO.read(
              new URL(
                "https://cdn.discordapp.com/attachments/728706968864423966/1119368338729087016/an-artist-drew-spongebob-as-a-real-life-human-and-2-2227-1497636095-0_dblbig.png"
              )
            )
          )
        ascii = imageToAscii(
                  Image.fromBufferedImage(bufferedImage),
                  CharTransformationType.MatchByWeightedDistance,
                  maxSize = Some(200),
                  contrast = Some(-64)
                )
        _ <- ZIO.succeed(println(ascii))
      } yield assertTrue(ascii.length > 10)
    ),
    suite("asciiFromBrightness")(
      test("convert from pixel")(
        assertTrue(
          asciiFromBrightness(1) == '$'
        )
      ),
      test("convert any grayscale")(
        check(Gen.int) { brightness =>
          ZIO.attempt(asciiFromBrightness(brightness)).either.map(result => assertTrue(result.isRight))
        }
      )
    )
  )

  def readImage(path: String): ZIO[Any, Throwable, Image] = for {
    file <- ZIO.attempt {
              new File("src/test/resources/" + path)
            }
    bufferedImage <- ZIO.attempt(ImageIO.read(file))
  } yield Image.fromBufferedImage(bufferedImage)
}
