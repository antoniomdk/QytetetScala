package model

import scalaz.State
import scalaz.State.{get, modify, put}
import scala.util.Random

object Qytetet {
  val MaxPlayers = 4
  val FreedomPrice = 200
  val InitialBalance = 2000

  val noChangeUnit: State[Game, Unit] = modify(identity);
  val noChangeBoolean: State[Game, Boolean] = noChangeUnit.map(_ => false)

  val nextPlayer: State[Game, Unit] = modify[Game] { state =>
    val playerIndex = (state.currentPlayer + 1) % state.playerList.size
    GameLens.currentPlayer.set(state, playerIndex)
  }

  val randomizeGame: State[Game, Unit] = for {
    deck <- GameLens.deck
    playerList <- GameLens.playerList
    _ <- GameLens.deck := Random.shuffle(deck)
    _ <- GameLens.currentPlayer := Random.nextInt(playerList.size)
  } yield ()

  def initialState(names: Seq[String]): Option[Game] = {
    lazy val deck = Deck.initial
    lazy val board = Board.initial
    lazy val playerList = names map createDefaultPlayer
    val validNameList = names.nonEmpty && names.lengthCompare(MaxPlayers) <= 0
    if (validNameList && deck.nonEmpty && board.list.nonEmpty) {
      Some(Game(-1, 0, None, playerList.toVector, deck, board))
    } else {
      None
    }
  }

  private def createDefaultPlayer(name: String): Player =
    Player(name, InitialBalance, List.empty, None, StartSquare.position, imprisoned = false)

  def applyCard(card: Card): State[Game, Unit] = {
    val stateChange = card match {
      case PayCollect(_, value) => GameLens.player %== (_.addBalance(value))
      case ConvertMe(_, bail) =>
        GameLens.player %== (_.convertToSpeculator(bail))
      case c: PerPlayer     => applyPerPlayerCard(c)
      case c: GoToSquare    => applyGoToSquareCard(c)
      case c: PerHouseHotel => applyPerHouseHotelCard(c)
      case c: EscapePrison  => applyEscapePrisonCard(c)
    }
    // Return card unless it's given to a player.
    card match {
      case _: EscapePrison => stateChange
      case _               => stateChange.imap(_.returnCard(card))
    }
  }

  def applyEscapePrisonCard(card: EscapePrison): State[Game, Unit] =
    for {
      player <- GameLens.player
      maybeFreePlayer = player.giveFreedomCard(card)
      _ <- GameLens.player := maybeFreePlayer.getOrElse(player)
    } yield ()

  def applyPerHouseHotelCard(card: PerHouseHotel): State[Game, Unit] =
    for {
      state <- get[Game]
      player <- GameLens.player
      totalValue = IPlayerStats.countHouseAndHotels(state, player) * card.value
      _ <- GameLens.player %== (_.addBalance(totalValue))
    } yield ()

  def applyGoToSquareCard(card: GoToSquare): State[Game, Unit] =
    for {
      board <- GameLens.board
      _ <- if (board.isJail(card.value)) jailPlayer else noChangeUnit
      _ <- movePlayer(card.value)
    } yield ()

  def applyPerPlayerCard(card: PerPlayer): State[Game, Unit] = for {
    player <- GameLens.player
    playerList <- GameLens.playerList
    newPlayer = player.addBalance(card.value * playerList.size - 1)
    newPlayerList = playerList map { p =>
      if (p != player) p.addBalance(card.value) else newPlayer
    }
    _ <- GameLens.playerList := newPlayerList
  } yield ()

  def movePlayer(pos: Int, relative: Boolean = false): State[Game, Unit] =
    for {
      player <- GameLens.player
      board <- GameLens.board
      relSquare = board.get(pos, player.currentSquare)
      absSquare = board.get(pos)
      newSquare = if (relative) relSquare else absSquare
      _ <- GameLens.player := player.updatePosition(newSquare)
    } yield ()

  def ranking(state: Game): Seq[(IPlayer, Int)] =
    state.playerList
      .map(p => p -> IPlayerStats.totalCapital(state, p))
      .sortBy { case (_, totalCapital) => totalCapital }

  private def playerCanBuyProperty(player: IPlayer, square: StreetSquare) =
    square.owner.isEmpty && player.balance > square.cost

  private def tryBuy(player: IPlayer, square: StreetSquare, state: Game): Option[Game] = {
    val newPlayer = player.addBalance(-square.cost).addProperty(square)
    for {
      newSquare <- square.setOwner(newPlayer)
      state <- state.updateSquare(newSquare)
      newState = GameLens.player.set(state, newPlayer)
    } yield newState
  }

  def buyProperty: State[Game, Boolean] = State { implicit state =>
    implicit val player: IPlayer = state.player
    IPlayerStats.currentSquare match {
      case s: StreetSquare if playerCanBuyProperty(player, s) =>
        val st = tryBuy(player, s, state)
        st.fold(state -> false)(_ -> true)
      case _ => state -> false
    }
  }

  def sellProperty(prop: StreetSquare): State[Game, Boolean] = State { state =>
    val change = for {
      (p, ns) <- state.player.sellProperty(prop)
      s <- state.updateSquare(ns)
      result = GameLens.player.set(s, p)
    } yield result
    change.fold(state -> false)(_ -> true)
  }

  def mortgageProperty(prop: StreetSquare): State[Game, Boolean] = State {
    implicit state =>
      implicit val player: IPlayer = state.player
      val canMortgage = !prop.title.mortgaged && IPlayerStats.hasProperty(
        prop.position)
      if (canMortgage) {
        performPropertyOperation(prop.mortgage()).run(state)
      } else {
        state -> false
      }
  }

  def performPropertyOperation(
      action: => Option[PropertyOperation]): State[Game, Boolean] = State {
    state =>
      val maybeNewState = for {
        a <- action
        p = state.player.addBalance(-a.cost)
        s <- GameLens.player.set(state, p).updateSquare(a.newState)
      } yield s -> true
      maybeNewState.getOrElse(state -> false)
  }

  def buildHouse(square: StreetSquare): State[Game, Boolean] =
    performPropertyOperation(square.buildHouse())

  def buildHotel(square: StreetSquare): State[Game, Boolean] =
    performPropertyOperation(square.buildHotel())

  def cancelMortgage(prop: StreetSquare): State[Game, Boolean] =
    performPropertyOperation(prop.cancelMortgage())

  def play(implicit die: Die): State[Game, Unit] = for {
    dieValue <- GameLens.die := die.roll()
    _ <- movePlayer(dieValue, relative = true)
    state <- get[Game]
    _ <- IPlayerStats.currentSquare(state, state.player) match {
      case _: CardSquare  => put[Game](state.extractCard)
      case _: JudgeSquare => jailPlayer
      case _              => noChangeUnit
    }
  } yield ()

  def tryEscapePrison(method: PrisonEscapeMethod)
                     (implicit die: Die): State[Game, Boolean] = for {
      state <- get[Game]
      player <- GameLens.player
      (newState, escaped) = method match {
        case PayingFreedom if player.balance > FreedomPrice =>
          val newPlayer = player.addBalance(-FreedomPrice).released
          GameLens.player.set(state, newPlayer) -> true
        case RollingDie if die.roll > 5 =>
          GameLens.player.set(state, player.released) -> true
        case _ => state -> false
      }
      _ <- put[Game](newState)
    } yield escaped

  val jailPlayer: State[Game, Boolean] = for {
    player <- GameLens.player
    board <- GameLens.board
    result <- player.returnFreedomCard match {
      case Some((card, newPlayer)) => for {
          _ <- GameLens.player := newPlayer
          newState <- get[Game]
          _ <- put[Game](newState.returnCard(card))
        } yield false
      case _ => for {
          _ <- GameLens.player := player.sentToJail
          _ <- movePlayer(board.jailPosition)
        } yield true
    }
  } yield result
}