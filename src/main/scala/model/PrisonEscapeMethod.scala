package model

sealed trait PrisonEscapeMethod
final case object RollingDie extends PrisonEscapeMethod
final case object PayingFreedom extends PrisonEscapeMethod