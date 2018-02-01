package model

sealed trait Square {
  def position: Int
}

final case class TaxSquare(position: Int, value: Int) extends Square
final case class JudgeSquare(position: Int) extends Square
final case class ParkingSquare(position: Int) extends Square
final case class CardSquare(position: Int) extends Square
final case class PrisonSquare(position: Int) extends Square
final case object StartSquare extends Square {
  val position = 0
}

final case class PropertyOperation(newState: StreetSquare, cost: Int)

case class StreetSquare(
    position: Int,
    cost:     Int,
    title:    PropertyTitle,
    owner:    Option[IPlayer] = None,
    houses:   Int = 0,
    hotels:   Int = 0) extends Square {

  val MaxHouses = 4
  val MaxHotels = 4

  lazy val totalPrice: Int = {
    val p = cost + (houses + hotels) * title.edificationPrice
    (p * (1 + title.revaluationFactor)).toInt
  }

  lazy val mortgageValue : Int = {
    val mb = title.mortgageBase
    mb *  1 + (houses * 0.5 + hotels).toInt
  }

  def buildHouse(): Option[PropertyOperation] = owner match {
    case Some(p) if houses < 4 * p.speculationFactor =>
      Some(PropertyOperation(copy(houses = houses + 1), title.edificationPrice))
    case _ => None
  }

  def buildHotel() : Option[PropertyOperation] = owner match {
    case Some(p) if houses >= 4 && hotels < 4 * p.speculationFactor =>
      Some(PropertyOperation(copy(hotels = hotels + 1), title.edificationPrice))
    case _ => None
  }

  def mortgage(): Option[PropertyOperation] =
    title.mortgage map { t => PropertyOperation(copy(title = t), -mortgageValue) }

  def cancelMortgage(): Option[PropertyOperation] = title.cancelMortgage map { t =>
    val cancelPrice = (mortgageValue * 1.1).toInt
    PropertyOperation(copy(title = t), cancelPrice)
  }

  def sellProperty(): Option[PropertyOperation] = owner map { _ =>
    val newState = copy(owner = None, houses = 0, hotels = 0)
    PropertyOperation(newState, totalPrice)
  }

  def setOwner(player: IPlayer) : Option[StreetSquare] = owner match {
    case None => Some(copy(owner = Some(player)))
    case _ => None
  }

  override def toString: String =
    s"""===STREET SQUARE===
       | Position: $position
       | Cost: $cost
       | Houses: $houses
       | Hotels: $hotels
       | Title:
       | $title
     """.stripMargin
}

