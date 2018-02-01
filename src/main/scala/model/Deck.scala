package model

import scala.util.Try
import scala.xml.{Elem, XML}

object Deck {
  val Filename = "data/cards.xml"

  lazy val initial : List[Card] = {
    val xml = XML.loadFile(Filename)
    val nodeSeq = xml.child.collect { case n: Elem => n }
    val seq : Seq[Card] = for {
      node <- nodeSeq
      card <- parseCard(node)
    } yield card

    seq.toList
  }

  def parseCard(node: Elem): Option[Card] = for {
    value <- Try((node \ "valor").text.toInt).toOption
    text  <- Try((node \ "texto").text).toOption
    cardType <- Try(node \@ "tipo").toOption
  } yield cardType match {
    case "SALIRCARCEL"  => EscapePrison(text)
    case "IRACASILLA"   => GoToSquare(text, value)
    case "PORJUGADOR"   => PerPlayer(text, value)
    case "PAGARCOBRAR"  => PayCollect(text, value)
    case "CONVERTIRME"  => ConvertMe(text, value)
    case "PORCASAHOTEL" => PerHouseHotel(text, value)
  }
}
