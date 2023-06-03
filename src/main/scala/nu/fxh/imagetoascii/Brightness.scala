package nu.fxh.imagetoascii

trait Brightness[A] {

  def adjustBrightness(value: A, ratio: Double): A

}

object Brightness {
  def apply[A](implicit brightness: Brightness[A]): Brightness[A] = brightness

}


