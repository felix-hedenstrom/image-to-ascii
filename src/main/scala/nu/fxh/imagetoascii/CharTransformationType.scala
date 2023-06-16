package nu.fxh.imagetoascii

sealed trait CharTransformationType

object CharTransformationType {
  case object Grayscale extends CharTransformationType

  case object MatchByHue extends CharTransformationType

  case object MatchByWeightedDistance extends CharTransformationType
}
