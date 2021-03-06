package model

import org.scalatest.FunSuite

class QytetetTest extends FunSuite {

  test("testPlay") {}

  test("testTryEscapePrison") {}

  test("testNextPlayer") {
    val game = Qytetet
      .initialState(List("Player1", "Player2", "Player3", "Player4"))
      .get

    assert(Qytetet.nextPlayer.exec(game).currentPlayer == 0)

    val s = for {
      _ <- Qytetet.nextPlayer
      _ <- Qytetet.nextPlayer
      _ <- Qytetet.nextPlayer
    } yield ()

    val s2 = for {
      _ <- s
      _ <- Qytetet.nextPlayer
    } yield ()

    assert(s.exec(game).currentPlayer == 3)
    assert(s2.exec(game).currentPlayer == 0)
  }

  test("testMortgageProperty") {}

  test("testInitialState") {
    val g1 = Qytetet.initialState(List("Player 1")).get
    assert(g1.currentPlayer == 0)
    assert(g1.player.name == "Player 1")
    assert(g1.board.list.nonEmpty)
    assert(g1.playerList.size == 1)

    val g2 = Qytetet.initialState(List("Player 1", "Player 2", "Player 3")).get
    assert(g2.currentPlayer == 0)
    assert(g2.playerList.size == 3)

    // Board should be independent of the player list.
    assert(g1.board == g2.board)

    // Testing with empty list
    val g3 = Qytetet.initialState(List.empty)
    assert(g3.isEmpty)

    // Testing with a list bigger than allowed.
    val list = (0 to Qytetet.MaxPlayers + 1).map {
      "Player " + _
    }

    val g4 = Qytetet.initialState(list)

    assert(g4.isEmpty)
  }

  test("testSellProperty") {}

  test("testApplyCard") {}

  test("testBuyProperty") {}

  test("testJailPlayer") {
    val game =
      Qytetet.initialState(List("Player 1", "Player 2", "Player 3")).get
    val c = Some(EscapePrison(""))
    val g1 = GameLens.player.set(game, game.player.newState(freedomCard = c))
    val r1 = Qytetet.jailPlayer.exec(g1)
    val p1 = r1.player

    assert(
      !p1.imprisoned && !IPlayerStats
        .currentSquare(r1, p1)
        .isInstanceOf[PrisonSquare])

    val g2 = GameLens.player.set(game, game.player.newState(freedomCard = None))
    val r2 = Qytetet.jailPlayer.exec(g2)
    val p2 = r2.player

    assert(
      !p2.imprisoned && !IPlayerStats
        .currentSquare(r2, p2)
        .isInstanceOf[PrisonSquare])
  }

  test("testRanking") {
    val playerNames = List("Player 1", "Player 2", "Player 3")
    val game = Qytetet.initialState(playerNames).get
    // Changing player balance according to index. (Higher index, higher balance)
    val newPlayerList = game.playerList.zipWithIndex.map {
      case (p, i) => p.addBalance(i * 100)
    }
    val newGame = GameLens.playerList.set(game, newPlayerList)
    val initialBalance = Qytetet.InitialBalance
    val ranking = Qytetet.ranking(newGame).map {
      case (p, b) => (p.name, b)
    }
    val expectedRanking = Seq(
      "Player 3" -> (initialBalance + 200),
      "Player 2" -> (initialBalance + 100),
      "Player 1" -> (initialBalance + 0)
    )
    assert(ranking == expectedRanking)
  }
}
