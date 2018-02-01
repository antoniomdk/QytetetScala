package model
import scala.xml.{Elem, XML}
import scala.util.Try

case class Board(list: Vector[Square], jailPosition: Int) {
  def get(i: Int) : Square = list(i)
  def get(i: Int, current: Int) : Square = get((current + i) % list.size)
  def isJail(square: Square): Boolean = square.isInstanceOf[PrisonSquare]
  def isJail(position: Int): Boolean = position == jailPosition
}

object Board {
  val Filename = "data/board.xml"

  lazy val initial : Board  = {
    val xml = XML.loadFile(Filename)
    val nodeSeq = xml.child.collect { case n: Elem => n }

    val seq : Seq[Square] = for {
      (node, index) <- nodeSeq.zipWithIndex
      pos = index + 1
      square <- node.label match {
        case "sorpresa" => Some(CardSquare(pos))
        case "parking"  => Some(ParkingSquare(pos))
        case "juez"     => Some(JudgeSquare(pos))
        case "impuesto" => parseTaxSquare(node, pos)
        case "calle"    => parseStreetSquare(node, pos)
        case "carcel"   => Some(PrisonSquare(pos))
      }
    } yield square

    val list = StartSquare +: seq.toVector
    val jailPos = list.indexWhere(_.isInstanceOf[PrisonSquare])

    Board(list, jailPos)
  }

  private def parseStreetSquare(node: Elem, index: Int): Option[StreetSquare] = {
    val name = node \@ "nombre"
    for {
      c  <- Try((node \@ "precio").toInt).toOption
      pe <- Try((node \ "p_edificacion").text.toInt).toOption
      fv <- Try((node \ "f_revalorizacion").text.toFloat).toOption
      pa <- Try((node \ "pb_alquiler").text.toInt).toOption
      ph <- Try((node \ "pb_hipoteca").text.toInt).toOption
    } yield StreetSquare(index, c, PropertyTitle(name, pa, fv, ph, pe, false), None)
  }

  private def parseTaxSquare(node: Elem, index: Int): Option[TaxSquare] =
    Try((node \@ "coste").toInt).toOption map { TaxSquare(index, _) }
}