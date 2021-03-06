package model

sealed trait IPlayer {
  def name: String
  def balance: Int
  def properties: List[Int]
  def freedomCard: Option[EscapePrison]
  def currentSquare: Int
  def speculationFactor: Int
  def imprisoned: Boolean

  def newState(balance: Int = balance,
               properties: List[Int] = properties,
               freedomCard: Option[EscapePrison] = freedomCard,
               imprisoned: Boolean = imprisoned,
               currentSquare: Int = currentSquare): IPlayer

  lazy val sentToJail: IPlayer = newState(imprisoned = true)
  lazy val released: IPlayer = newState(imprisoned = false)

  def updatePosition(square: Square): IPlayer = {
    val balanceAddition = square match {
      case StartSquare       => Qytetet.InitialBalance
      case TaxSquare(_, tax) => -tax
      case StreetSquare(_, _, title, Some(player), _, _)
          if !player.imprisoned && !player.equals(this) =>
        -title.rentBase
      case _ => 0
    }
    newState(currentSquare = square.position,
             balance = balance + balanceAddition)
  }

  def addBalance(quantity: Int): IPlayer =
    newState(balance = balance + quantity)

  def giveFreedomCard(card: EscapePrison): Option[IPlayer] = freedomCard match {
    case None    => Some(newState(freedomCard = Some(card)))
    case Some(_) => None
  }

  def returnFreedomCard: Option[(Card, IPlayer)] =
    freedomCard map { _ -> newState(freedomCard = None) }

  def convertToSpeculator(bail: Int): Speculator = this match {
    case Player(n, b, p, fc, cs, i) => Speculator(n, b, p, fc, cs, i, bail)
    case p: Speculator              => p.copy(bail = bail)
  }

  def payTaxes(count: Int): IPlayer = this match {
    case _: Player     => addBalance(-count)
    case _: Speculator => addBalance(-count / 2)
  }

  def addProperty(square: StreetSquare): IPlayer =
    newState(properties = properties :+ square.position)

  def removeProperty(prop: StreetSquare): Option[IPlayer] = {
    if (properties.contains(prop.position)) {
      Some(newState(properties = properties.filterNot(_ == prop.position)))
    } else {
      None
    }
  }

  def sellProperty(property: StreetSquare): Option[(IPlayer, StreetSquare)] =
    for {
      player <- removeProperty(property)
      op <- property.sellProperty()
      result = player.addBalance(op.cost) -> op.newState
    } yield result

  override def toString: String =
    s"""Name: $name
       |Balance: $balance
       |Properties: ${properties}
       |Freedom card?: ${freedomCard.isDefined}
       |Current square: $currentSquare
       |Speculation factor: $speculationFactor
       |Imprisoned?: $imprisoned
     """.stripMargin
}

case class Player(name: String,
                  balance: Int,
                  properties: List[Int],
                  freedomCard: Option[EscapePrison],
                  currentSquare: Int,
                  imprisoned: Boolean)
    extends IPlayer {

  val speculationFactor = 1

  def newState(balance: Int,
               properties: List[Int],
               freedomCard: Option[EscapePrison],
               imprisoned: Boolean,
               currentSquare: Int): IPlayer =
    copy(name, balance, properties, freedomCard, currentSquare, imprisoned)

  override def toString: String = "===PLAYER===\n" + super.toString
}

case class Speculator(name: String,
                      balance: Int = 0,
                      properties: List[Int] = Nil,
                      freedomCard: Option[EscapePrison] = None,
                      currentSquare: Int = StartSquare.position,
                      imprisoned: Boolean = false,
                      bail: Int = 0)
    extends IPlayer {

  val speculationFactor = 2

  def newState(balance: Int,
               properties: List[Int],
               freedomCard: Option[EscapePrison],
               imprisoned: Boolean,
               currentSquare: Int): IPlayer =
    copy(name, balance, properties, freedomCard, currentSquare, imprisoned)

  override def toString: String = "===SPECULATOR===\n" + super.toString
}

object IPlayerStats {
  def totalCapital(implicit state: Game, player: IPlayer): Int =
    player.balance + properties.map(_.propertyCapital).sum

  def currentSquare(implicit state: Game, player: IPlayer): Square =
    state.board.list(player.currentSquare)

  def properties(implicit state: Game, player: IPlayer): List[StreetSquare] =
    state.board.list.collect {
      case s: StreetSquare if hasProperty(s.position) => s
    }.toList

  def countHouseAndHotels(implicit state: Game, player: IPlayer): Int =
    properties.map(p => p.hotels + p.houses).sum

  def mortgagedProperties(mortgaged: Boolean)(
      implicit state: Game,
      player: IPlayer): List[StreetSquare] =
    properties filter { _.title.mortgaged == mortgaged }

  def hasProperty(propertyIndex: Int)(implicit state: Game,
                                      player: IPlayer): Boolean =
    properties(state, player).contains(propertyIndex)
}
