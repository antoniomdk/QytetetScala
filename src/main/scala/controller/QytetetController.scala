package controller

import model._
import view.{DieView, PlayerListView, QytetetView}
import scalaz.State
import scala.collection.JavaConverters
import scala.swing.BorderPanel.Position.{South, Center}
import javax.swing.JOptionPane
import scala.swing.event.ButtonClicked
import scala.swing.{
  MainFrame,
  Dimension,
  Button,
  BorderPanel,
  FlowPanel,
  Dialog,
  Component,
  GridPanel
}

object QytetetController {
  val WINDOW_SIZE = (750, 650)
  val BIG_SIZE = new Dimension(120, 80)
  val TITLE = "Qytetet Game"
  val MSG_BUY = "Buy property title?"
  val MSG_ACCEPTED_PURCHASE = "Congratulations, title acquired"
  val MSG_REJECTED_PURCHASE = "Purchase failed"
  val MSG_ESCAPE = "Player released"
  val MSG_NO_ESCAPE = "Player NOT released"
  val MSG_MANAGEMENT = "Perform any management"
  val MSG_MANAGEMENT_TITLE = "Select"
  val MSG_SELECT_PROPS = "Choose an option"
  val MSG_MANAGEMENT_PERFORMED = "Management performed successfully"
  val MSG_MANAGEMENT_NOT_PERFORMED = "Management failed"
  val MSG_EXIT = "Game over"
  val MSG_ESCAPE_OPTIONS = Seq("Rolling die", "Paying freedom")
  val MSG_MANAGEMENT_OPTIONS = Seq("Build house",
                                   "Build hotel",
                                   "Sell property",
                                   "Mortgage",
                                   "Cancel Mortgage")

  val qytetetView = new QytetetView()
  implicit var state: Game = _

  def changeState[A](change: State[Game, A]): A = {
    val (s, v) = change.run(state)
    updateState(s)
    checkGameOver()
    v
  }

  def updateState(s: Game): Unit = {
    state = s
    qytetetView.update(state)
  }

  def checkGameOver(): Unit = {
    if (state.player.balance < 0) {
      val ranking = Qytetet
        .ranking(state)
        .view
        .zipWithIndex
        .map { case ((p, c), i) => s"${i + 1}. ${p.name}: $c\n" }
        .fold("") { _ + _ }
      showMessage(ranking, MSG_EXIT)
      System.exit(0)
    }
  }

  def getConfirmation(msg: String): Boolean =
    Dialog.showConfirmation(mainFrame, msg) == Dialog.Result.Yes

  def showMessage(msg: String, title: String = ""): Unit =
    Dialog.showMessage(mainFrame, msg, title)

  def getOption(msg: String, entries: Seq[Any]): Int = {
    val e = (entries map { _.asInstanceOf[AnyRef] }).toArray
    JOptionPane.showOptionDialog(mainFrame.peer,
                                 msg,
                                 "",
                                 JOptionPane.DEFAULT_OPTION,
                                 JOptionPane.QUESTION_MESSAGE,
                                 null,
                                 e,
                                 e(0))
  }

  implicit object VisualDie extends Die {
    lazy val view = {
      DieView.createInstance(mainFrame.peer)
      DieView.getInstance()
    }
    override def roll: Int = view.nextNumber()
  }

  lazy val mainFrame: MainFrame = new MainFrame {
    title = TITLE
    preferredSize = new Dimension(WINDOW_SIZE._1, WINDOW_SIZE._2)

    val playButton = new Button {
      text = "Play"
      enabled = true
      preferredSize = BIG_SIZE
    }

    val nextPlayerButton = new Button {
      text = "Next Player"
      enabled = false
      preferredSize = BIG_SIZE
    }

    val propertyManagementButton = new Button {
      text = "Manage Property"
      enabled = false
    }

    val buyPropertyButton = new Button {
      text = "Buy Property"
      enabled = false
    }

    val applyCardButton = new Button {
      text = "Apply card"
      enabled = false
    }

    val buttonsPanel = new FlowPanel {
      contents += playButton
      contents += new GridPanel(2, 1) {
        contents += propertyManagementButton
        contents += buyPropertyButton
      }
      contents += applyCardButton
      contents += nextPlayerButton
    }

    listenTo(
      playButton,
      buyPropertyButton,
      propertyManagementButton,
      applyCardButton,
      nextPlayerButton
    )

    def resetButtons(): Unit = {
      propertyManagementButton.enabled = false
      applyCardButton.enabled = false
      nextPlayerButton.enabled = false
      buyPropertyButton.enabled = false
    }

    def nextPlayer(): Unit = {
      lazy val escapePrison = getOption("", MSG_ESCAPE_OPTIONS) match {
        case 0 => Qytetet.tryEscapePrison(RollingDie)
        case 1 => Qytetet.tryEscapePrison(PayingFreedom)
        case _ => Qytetet.noChangeBoolean
      }

      changeState(Qytetet.nextPlayer)
      resetButtons()

      if (state.player.imprisoned) {
        val free = changeState(escapePrison)
        if (!free) {
          showMessage(MSG_NO_ESCAPE)
          nextPlayer()
        } else {
          showMessage(MSG_ESCAPE)
        }
      }
      playButton.enabled = true
    }

    def updateButtons(): Unit = {
      implicit val player = state.player
      val square = IPlayerStats.currentSquare

      square match {
        case _: CardSquare   => applyCardButton.enabled = true
        case s: StreetSquare => buyPropertyButton.enabled = s.owner.isEmpty
        case _               => ()
      }

      square match {
        case _: CardSquare => ()
        case _ => {
          if (player.properties.nonEmpty && !player.imprisoned) {
            propertyManagementButton.enabled = true
          }
          nextPlayerButton.enabled = true
        }
      }
    }

    contents = new BorderPanel {
      layout(Component.wrap(qytetetView)) = Center
      layout(buttonsPanel) = South
    }

    reactions += {
      case ButtonClicked(`nextPlayerButton`) => nextPlayer()

      case ButtonClicked(`playButton`) => {
        playButton.enabled = false
        changeState(Qytetet.play)
        updateButtons()
      }

      case ButtonClicked(`applyCardButton`) => {
        applyCardButton.enabled = false
        val card = state.currentCard
        val change = card.fold(Qytetet.noChangeUnit) { Qytetet.applyCard }
        changeState(change)
        card match {
          case Some(c) if c.isInstanceOf[GoToSquare] => updateButtons()
          case _ => {
            nextPlayerButton.enabled = true
          }
        }
      }

      case ButtonClicked(`buyPropertyButton`) => {
        if (getConfirmation(MSG_BUY)) {
          buyPropertyButton.enabled = false
          val done = changeState(Qytetet.buyProperty)
          val msg = if (done) MSG_ACCEPTED_PURCHASE else MSG_REJECTED_PURCHASE
          showMessage(msg)
          updateButtons()
        }
      }

      case ButtonClicked(`propertyManagementButton`) => {
        implicit val player = state.player
        val action = getOption(MSG_MANAGEMENT, MSG_MANAGEMENT_OPTIONS)
        val options = IPlayerStats.mortgagedProperties(action == 4)
        lazy val propIndex = getOption(MSG_SELECT_PROPS, options map {
          _.title.name
        })
        lazy val property = options(propIndex)
        lazy val change = action match {
          case 0 => Qytetet.buildHouse(property)
          case 1 => Qytetet.buildHotel(property)
          case 2 => Qytetet.sellProperty(property)
          case 3 => Qytetet.mortgageProperty(property)
          case 4 => Qytetet.cancelMortgage(property)
          case _ => Qytetet.noChangeBoolean
        }

        if (options.nonEmpty && action >= 0 && propIndex >= 0) {
          val done = changeState(change)
          val msg =
            if (done) MSG_MANAGEMENT_PERFORMED else MSG_MANAGEMENT_NOT_PERFORMED
          showMessage(msg)
          if (done) {
            updateButtons()
            propertyManagementButton.enabled = false
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    mainFrame.visible = true
    val nameList = new PlayerListView(mainFrame.peer, true).getNameList()
    val initialState =
      Qytetet.initialState(JavaConverters.asScalaBuffer(nameList))
    initialState.fold(System.exit(1)) { updateState }
  }
}
