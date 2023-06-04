package nu.fxh.imagetoascii.controlcodes

import scala.io.AnsiColor

sealed abstract class Color(val foreground: String, val background: String)

object Color {
  case object Black extends Color(AnsiColor.BLACK, AnsiColor.BLACK_B)
  case object Red   extends Color(AnsiColor.RED, AnsiColor.RED_B)

}
