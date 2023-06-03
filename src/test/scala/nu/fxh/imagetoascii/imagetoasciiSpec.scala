package nu.fxh.imagetoascii

import nu.fxh.coloredToAscii
import nu.fxh.imagetoascii.Pixel.ColoredPixel
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}
import zio.{Scope, ZIO}

import java.io.File
import javax.imageio.ImageIO

object imagetoasciiSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("imagetoascii")(
    test("convert the turtle image to ascii")(
      readImage("turtle.png").map { image =>
        val size = 150

        val ascii = coloredToAscii(image, maxSize = Some(size))

        println(ascii)

        assertTrue(ascii.length == size || ascii.split("\n").head.length == size)
      }
    ),
    test("convert the alphabet image to ascii")(
      readImage("alphabet.jpg").map { image =>
        val size  = 150
        val ascii = coloredToAscii(image, maxSize = Some(size))
        println(ascii)

        assertTrue(ascii.length == size || ascii.split("\n").head.length == size)
      }
    )
  )

  def readImage(path: String): ZIO[Any, Throwable, Image[ColoredPixel]] = for {
    file <- ZIO.attempt {
              new File("src/test/resources/" + path)
            }
    bufferedImage <- ZIO.attempt(ImageIO.read(file))
  } yield Image.fromBufferedImage(bufferedImage)
}
