package nu.fxh.imagetoascii.controlcodes

import scala.io.AnsiColor

sealed abstract class Control(val value: String)

object Control {
  case object Reset extends Control(AnsiColor.RESET)

}
