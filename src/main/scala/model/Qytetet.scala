package model

import scalaz.State
import scalaz.State.{ get, modify, put }
import scala.util.Random

object Qytetet {
  val MaxPlayers = 4
  val FreedomPrice = 200
  val InitialBalance = 2000

  lazy val noChangeBoolean: State[Game, Boolean] = State  { _ -> false }
  lazy val noChangeUnit: State[Game, Unit] = State.modify(identity)

  lazy val nextPlayer: State[Game, Unit] = modify[Game] { state =>
    val playerIndex = (state.currentPlayer + 1) % state.playerList.size
    state.copy(currentPlayer = playerIndex)
  }

  def initialState(names: Seq[String], randomInitialPlayer: Boolean = false): Option[Game] = {
    lazy val deck = Random.shuffle(Deck.initial)
    lazy val board = Board.initial
    lazy val pList = names map {
      Player(_, InitialBalance, List.empty, None, StartSquare.position, false)
    }
    if (names.nonEmpty && names.lengthCompare(MaxPlayers) <= 0 &&
      deck.nonEmpty && board.list.nonEmpty) {
      val currentPlayer = if (randomInitialPlayer) Random.nextInt(pList.size) else 0
      Some(Game(-1, currentPlayer, None, pList.toVector, deck, board))
    }
    else {
      None
    }
  }

  def applyCard(card: Card): State[Game, Unit] = modify[Game] { implicit state =>
    implicit val player: IPlayer = state.player

    val st = card match {
      case PayCollect(_, value) => state.changePlayer(player.changeBalance(value))
      case GoToSquare(_, pos) if state.board.isJail(pos) => jailPlayer.exec(state)
      case GoToSquare(_, pos) => movePlayer(pos).exec(state)
      case ConvertMe(_, bail) => state.changePlayer(player.convert(bail))
      case c: PerPlayer       => applyPerPlayerCard(c).exec(state)

      case PerHouseHotel(_, value) => {
        val totalValue = IPlayerStats.countHouseHotels * value
        state.changePlayer(player.changeBalance(totalValue))
      }

      case c: EscapePrison => {
        val p = player.giveFreedomCard(c)
        p.fold(state) { state.changePlayer }
      }
    }
    // Return card unless it's given to a player.
    card match {
      case _:EscapePrison => st
      case _ => st.returnCard(card)
    }
  }

  def applyPerPlayerCard(card: PerPlayer): State[Game, Unit] = modify { state =>
    val player = state.player
    val newPlayer = player.changeBalance(card.value * state.playerList.size - 1)
    val newPlayerList = state.playerList map {
      p =>  if (p != player) p.changeBalance(card.value) else newPlayer
    }
    state.copy(playerList = newPlayerList)
  }

  def movePlayer(pos: Int, relative: Boolean = false): State[Game, Unit] =
    modify[Game] { state =>
      val player = state.player
      lazy val relSquare = state.board.get(pos, player.currentSquare)
      lazy val absSquare = state.board.get(pos)
      val newSquare = if (relative) relSquare else absSquare
      state.changePlayer(player.updatePosition(newSquare))
    }

  // Players ranking sorted by capital
  def ranking(state: Game): Seq[(IPlayer, Int)] =
    state.playerList.view
      .map(p => p -> IPlayerStats.totalCapital(state, p))
      .sortWith(_._2 > _._2).force

  def buyProperty: State[Game, Boolean] = {
    def canBuy(p: IPlayer, s: StreetSquare) = s.owner.isEmpty && p.balance > s.cost

    def tryBuy(p: IPlayer, s: StreetSquare, g: Game): Option[Game] = {
      val newPlayer = p.changeBalance(-s.cost).addProperty(s)
      for {
        newSquare <- s.setOwner(newPlayer)
        state <- g.updateSquare(newSquare)
        result = state.changePlayer(newPlayer)
      } yield result
    }

    State[Game, Boolean] { implicit state =>
      implicit val player = state.player
      IPlayerStats.currentSquare match {
        case s: StreetSquare if canBuy(player, s) => {
          val st = tryBuy(player, s, state)
          st.fold(state -> false) { _ -> true}
        }
        case _ => state -> false
      }
    }
  }

  def sellProperty(prop: StreetSquare): State[Game, Boolean] = State { state =>
    val change = for {
      (p, ns) <- state.player.sellProperty(prop)
      s <- state.updateSquare(ns)
      result = s.changePlayer(p)
    } yield result

    change.fold(state -> false) { _ -> true }
  }

  def mortgageProperty(prop: StreetSquare): State[Game, Boolean] = State { state =>
    val canMortgage = !prop.title.mortgaged && state.player.properties.contains(prop.position)

    if (canMortgage) {
      performPropertyOperation(prop.mortgage).run(state)
    }
    else {
      state -> false
    }
  }

  def performPropertyOperation(action: => Option[PropertyOperation]):
  State[Game, Boolean] = State { state =>
    val newState = for {
      a <- action
      p = state.player.changeBalance(-a.cost)
      s <- state.changePlayer(p).updateSquare(a.newState)
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
    val newState = movePlayer(dieValue, relative = true).exec(state.copy(die = dieValue))

    IPlayerStats.currentSquare(newState, newState.player) match {
      case _: CardSquare => newState.extractCard
      case _: JudgeSquare => jailPlayer.exec(newState)
      case _ => newState
    }
  }

  def tryEscapePrison(method: PrisonEscapeMethod)(implicit die: Die):
  State[Game, Boolean] = State { state =>
    val player = state.player
    method match {
      case PayingFreedom if player.balance > FreedomPrice => {
        val p = player.changeBalance(-FreedomPrice).released
        state.changePlayer(p) -> true
      }
      case RollingDie if die.roll > 5 => state.changePlayer(player.released) -> true
      case _ => state -> false
    }
  }

  // Jail player if does not have freedom card.
  lazy val jailPlayer: State[Game, Boolean] = State { state =>
    val player = state.player
    lazy val jailAction = for {
      s <- get[Game]
      _ <- put(s.changePlayer(player.sentToJail))
      _ <- movePlayer(state.board.jailPosition)
    } yield true

    player.returnFreedomCard.fold(jailAction.run(state)) {
      case (c, p) => state.changePlayer(p).returnCard(c) -> false
    }
  }
}