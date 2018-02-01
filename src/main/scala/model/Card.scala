package model

sealed trait Card {
  def text: String
  def value: Int
}

final case class PayCollect(text: String, value: Int) extends Card
final case class GoToSquare(text: String, value: Int) extends Card
final case class PerHouseHotel(text: String, value: Int) extends Card
final case class PerPlayer(text: String, value: Int) extends Card
final case class EscapePrison(text: String) extends Card { val value = 0 }
final case class ConvertMe(text: String, value: Int) extends Card