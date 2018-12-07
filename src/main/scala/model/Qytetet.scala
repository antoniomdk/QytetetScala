package model

import scalaz.State
import scalaz.State.{get, modify, put}
import scala.util.Random

object Qytetet {
  val MaxPlayers = 4
  val FreedomPrice = 200
  val InitialBalance = 2000

  val noChangeUnit: State[Game, Unit] = modify(identity)
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
    lazy val pList = names map {
      Player(_, InitialBalance, List.empty, None, StartSquare.position, imprisoned = false)
    }
    if (names.nonEmpty && names.lengthCompare(MaxPlayers) <= 0 &&
      deck.nonEmpty && board.list.nonEmpty) {
      Some(Game(-1, 0, None, pList.toVector, deck, board))
    } else {
      None
    }
  }

  def applyCard(card: Card): State[Game, Unit] = {
    val stateChange = card match {
      case PayCollect(_, value) => GameLens.player %== (_.addBalance(value))
      case ConvertMe(_, bail) => GameLens.player %== (_.convertToSpeculator(bail))
      case c: PerPlayer => applyPerPlayerCard(c)
      case c: GoToSquare => applyGoToSquareCard(c)
      case c: PerHouseHotel => applyPerHouseHotelCard(c)
      case c: EscapePrison => applyEscapePrisonCard(c)
    }
    // Return card unless it's given to a player.
    card match {
      case _: EscapePrison => stateChange
      case _ => stateChange.imap(_.returnCard(card))
    }
  }

  def applyEscapePrisonCard(card: EscapePrison): State[Game, Unit] = for {
    player <- GameLens.player
    maybeFreePlayer = player.giveFreedomCard(card)
    _ <- GameLens.player := maybeFreePlayer.getOrElse(player)
  } yield ()

  def applyPerHouseHotelCard(card: PerHouseHotel): State[Game, Unit] = for {
    state <- get[Game]
    player <- GameLens.player
    totalValue = IPlayerStats.countHouseAndHotels(state, player) * card.value
    _ <- GameLens.player %== (_.addBalance(totalValue))
  } yield ()

  def applyGoToSquareCard(card: GoToSquare): State[Game, Unit] = for {
    board <- GameLens.board
    _ <- if (board.isJail(card.value)) jailPlayer else noChangeUnit
    _ <- movePlayer(card.value)
  } yield ()

  def applyPerPlayerCard(card: PerPlayer): State[Game, Unit] = modify { state =>
    val player = state.player
    val newPlayer = player.addBalance(card.value * state.playerList.size - 1)
    val newPlayerList = state.playerList map { p =>
      if (p != player) p.addBalance(card.value) else newPlayer
    }
    state.copy(playerList = newPlayerList)
  }

  def movePlayer(pos: Int, relative: Boolean = false): State[Game, Unit] = for {
    player <- GameLens.player
    board <- GameLens.board
    relSquare = board.get(pos, player.currentSquare)
    absSquare = board.get(pos)
    newSquare = if (relative) relSquare else absSquare
    _ <- GameLens.player := player.updatePosition(newSquare)
  } yield ()

  // Players ranking sorted by capital
  def ranking(state: Game): Seq[(IPlayer, Int)] =
    state.playerList
      .map(p => p -> IPlayerStats.totalCapital(state, p))
      .sortWith(_._2 > _._2)

  def buyProperty: State[Game, Boolean] = {
    def canBuy(p: IPlayer, s: StreetSquare) =
      s.owner.isEmpty && p.balance > s.cost

    def tryBuy(p: IPlayer, s: StreetSquare, g: Game): Option[Game] = {
      val newPlayer = p.addBalance(-s.cost).addProperty(s)
      for {
        newSquare <- s.setOwner(newPlayer)
        state <- g.updateSquare(newSquare)
        newState = GameLens.player.set(state, newPlayer)
      } yield newState
    }

    State[Game, Boolean] { implicit state =>
      implicit val player = state.player
      IPlayerStats.currentSquare match {
        case s: StreetSquare if canBuy(player, s) => {
          val st = tryBuy(player, s, state)
          st.fold(state -> false)(_ -> true)
        }
        case _ => state -> false
      }
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
      implicit val player = state.player
      val canMortgage = !prop.title.mortgaged && IPlayerStats.hasProperty(prop.position)
      if (canMortgage) {
        performPropertyOperation(prop.mortgage).run(state)
      } else {
        state -> false
      }
  }

  def performPropertyOperation(action: => Option[PropertyOperation]): State[Game, Boolean] = State {
    state =>
      val newState = for {
        a <- action
        p = state.player.addBalance(-a.cost)
        s <- GameLens.player.set(state, p).updateSquare(a.newState)
      } yield s -> true
      newState.getOrElse(state -> false)
  }

  def buildHouse(square: StreetSquare): State[Game, Boolean] =
    performPropertyOperation(square.buildHouse)

  def buildHotel(square: StreetSquare): State[Game, Boolean] =
    performPropertyOperation(square.buildHotel)

  def cancelMortgage(prop: StreetSquare): State[Game, Boolean] =
    performPropertyOperation(prop.cancelMortgage)

  def play(implicit die: Die): State[Game, Unit] = modify { state =>
    val dieValue = die.roll
    val newState =
      movePlayer(dieValue, relative = true).exec(state.copy(die = dieValue))

    IPlayerStats.currentSquare(newState, newState.player) match {
      case _: CardSquare => newState.extractCard
      case _: JudgeSquare => jailPlayer.exec(newState)
      case _ => newState
    }
  }

  def tryEscapePrison(method: PrisonEscapeMethod)(
    implicit die: Die): State[Game, Boolean] = for {
    state <- get[Game]
    player <- GameLens.player
    (newState, result) = method match {
      case PayingFreedom if player.balance > FreedomPrice => {
        val newPlayer = player.addBalance(-FreedomPrice).released
        GameLens.player.set(state, newPlayer) -> true
      }
      case RollingDie if die.roll > 5 =>
        GameLens.player.set(state, player.released) -> true
      case _ => state -> false
    }
    _ <- put[Game](newState)
  } yield result

  // Jail player if does not have freedom card.
  lazy val jailPlayer: State[Game, Boolean] = for {
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
