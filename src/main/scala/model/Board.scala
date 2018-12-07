package model
import scala.xml.{Elem, XML}
import scala.util.Try

case class Board(list: Vector[Square], jailPosition: Int) {
  def get(i: Int): Square = list(i)
  def get(i: Int, current: Int): Square = get((current + i) % list.size)
  def isJail(square: Square): Boolean = square.isInstanceOf[PrisonSquare]
  def isJail(position: Int): Boolean = position == jailPosition
}

object Board {
  val Filename = "data/board.xml"

  lazy val xmlNodes =
    XML.loadFile(Filename).child.toVector.collect { case n: Elem => n }

  lazy val initial: Board = {
    val squares: Vector[Square] = for {
      (node, index) <- xmlNodes.zipWithIndex
      pos = index + 1
      square <- createSquare(node.label, pos, node)
    } yield square

    val list = StartSquare +: squares
    val jailPos = list.indexWhere(_.isInstanceOf[PrisonSquare])
    Board(list, jailPos)
  }

  def createSquare(squareType: String, position: Int, node: Elem) =
    squareType match {
      case "sorpresa" => Some(CardSquare(position))
      case "parking"  => Some(ParkingSquare(position))
      case "juez"     => Some(JudgeSquare(position))
      case "impuesto" => parseTaxSquare(node, position)
      case "calle"    => parseStreetSquare(node, position)
      case "carcel"   => Some(PrisonSquare(position))
    }

  private def parseStreetSquare(node: Elem,
                                index: Int): Option[StreetSquare] = {
    val name = node \@ "nombre"
    val result = for {
      c  <- Try((node \@ "precio").toInt)
      pe <- Try((node \ "p_edificacion").text.toInt)
      fv <- Try((node \ "f_revalorizacion").text.toFloat)
      pa <- Try((node \ "pb_alquiler").text.toInt)
      ph <- Try((node \ "pb_hipoteca").text.toInt)
      propertyTitle = PropertyTitle(name, pa, fv, ph, pe, false)
    } yield StreetSquare(index, c, propertyTitle, None)
    result.toOption
  }

  private def parseTaxSquare(node: Elem, index: Int): Option[TaxSquare] =
    Try((node \@ "coste").toInt).toOption map { TaxSquare(index, _) }
}
