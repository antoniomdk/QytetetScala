package model

import scalaz.Lens

sealed case class Game(die: Int,
                       currentPlayer: Int,
                       currentCard: Option[Card],
                       playerList: Vector[IPlayer],
                       deck: Deck.Container,
                       board: Board) {

  lazy val player: IPlayer = playerList(currentPlayer)

  def extractCard: Game = copy(currentCard = Some(deck.head), deck = deck.tail)

  def returnCard(card: Card): Game = currentCard match {
    case Some(c) if c.equals(card) =>
      copy(currentCard = None, deck = deck :+ card)
    case _ => copy(deck = deck :+ card)
  }

  def updateSquare(square: Square): Option[Game] = {
    if (square.position > 0 && square.position < board.list.size) {
      val l = board.list.updated(square.position, square)
      val b = board.copy(list = l)
      Some(copy(board = b))
    } else {
      None
    }
  }
}

object GameLens {
  val currentPlayer: Lens[Game, Int] = Lens.lensu(
    (game, newValue) => game.copy(currentPlayer = newValue),
    _.currentPlayer
  )

  val board: Lens[Game, Board] = Lens.lensu(
    (game, value) => game.copy(board = value),
    _.board
  )

  val player: Lens[Game, IPlayer] = Lens.lensu(
    (game, value) => {
      val newPlayerList = game.playerList.updated(game.currentPlayer, value)
      game.copy(playerList = newPlayerList)
    },
    _.player
  )

  val deck: Lens[Game, Deck.Container] = Lens.lensu(
    (game, value) => game.copy(deck = value),
    _.deck
  )

  val currentCard: Lens[Game, Option[Card]] = Lens.lensu(
    (game, value) => game.copy(currentCard = value),
    _.currentCard
  )

  val playerList: Lens[Game, Vector[IPlayer]] = Lens.lensu(
    (game, value) => game.copy(playerList = value),
    _.playerList
  )

  val die: Lens[Game, Int] = Lens.lensu(
    (game, value) => game.copy(die = value),
    _.die
  )
}
