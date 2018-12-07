package model

import scala.util.Try
import scala.xml.{Elem, XML}
import scala.collection.immutable.Queue

object Deck {
  val Filename = "data/cards.xml"
  type Container = List[Card]

  lazy val initial: Container = {
    val xml = XML.loadFile(Filename)
    val nodeSeq = xml.child.collect { case n: Elem => n }
    val seq: Seq[Card] = for {
      node <- nodeSeq
      card <- parseCard(node)
    } yield card

    seq.toList
  }

  def parseCard(node: Elem): Option[Card] = {
    val maybeCard = for {
      cardType <- Try(node \@ "tipo")
      value <- Try((node \ "valor").text.toInt)
      text <- Try((node \ "texto").text)
    } yield createCard(cardType, text, value)
    maybeCard.toOption
  }

  def createCard(cardType: String, text: String, value: Int): Card =
    cardType match {
      case "SALIRCARCEL"  => EscapePrison(text)
      case "IRACASILLA"   => GoToSquare(text, value)
      case "PORJUGADOR"   => PerPlayer(text, value)
      case "PAGARCOBRAR"  => PayCollect(text, value)
      case "CONVERTIRME"  => ConvertMe(text, value)
      case "PORCASAHOTEL" => PerHouseHotel(text, value)
    }
}
