package nu.fxh.imagetoascii

import nu.fxh.{asciiFromBrightness, imageToAscii}
import zio.test.{Gen, Spec, TestEnvironment, ZIOSpecDefault, assertTrue, check}
import zio.{Scope, ZIO}

import java.io.File
import java.net.URL
import javax.imageio.ImageIO

object imagetoasciiSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("imagetoascii")(
    test("convert the turtle image to ascii")(
      readImage("turtle.png").map { image =>
        val size = 150

        val ascii = imageToAscii(image, maxSize = Some(size), useColor = true)

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
        val ascii = imageToAscii(image, maxSize = Some(size), useColor = true)
        println(ascii)

        assertTrue((ascii.length max ascii.split("\n").head.length) == size)
      }
    ),
    test("download online image and translate it")(
      for {
        bufferedImage <-
          ZIO.attempt(
            ImageIO.read(
              new URL("https://neode.se:8448/_matrix/media/r0/thumbnail/neode.se/lyqafxeouxnwfDXkrzaGMPoA?width=604&height=604&method=scale")
            )
          )
        ascii = imageToAscii(Image.fromBufferedImage(bufferedImage), maxSize = Some(200), useColor = true)
        _    <- ZIO.succeed(println(ascii))
      } yield assertTrue(ascii.length > 10)
    ),
    suite("asciiFromBrightness")(
      test("convert from pixel")(
        assertTrue(
          asciiFromBrightness(255) == '$'
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
