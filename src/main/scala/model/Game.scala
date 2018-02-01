package model

sealed case class Game(
    die:            Int,
    currentPlayer:  Int,
    currentCard:    Option[Card],
    playerList:     Vector[IPlayer],
    deck:           List[Card],
    board:          Board) {

  lazy val player: IPlayer = playerList(currentPlayer)

  def changePlayer(player: IPlayer): Game = {
    val pList = playerList.updated(currentPlayer, player)
    copy(playerList = pList)
  }

  def extractCard: Game = copy(currentCard = Some(deck.head), deck = deck.tail)

  def returnCard(card: Card): Game = currentCard match {
    case Some(c) if c.equals(card) => copy(currentCard = None, deck = deck :+ card)
    case _                         => copy(deck = deck :+ card)
  }

  def updateSquare(square: Square) : Option[Game] = {
    if (square.position > 0 && square.position < board.list.size) {
      val l = board.list.updated(square.position, square)
      val b = board.copy(list = l)
      Some(copy(board = b))
    }
    else {
      None
    }
  }
}
