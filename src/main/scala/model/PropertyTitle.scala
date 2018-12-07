package model

case class PropertyTitle(name: String,
                         rentBase: Int,
                         revaluationFactor: Float,
                         mortgageBase: Int,
                         edificationPrice: Int,
                         mortgaged: Boolean) {

  def mortgage: Option[PropertyTitle] =
    if (!mortgaged) Some(copy(mortgaged = true)) else None

  def cancelMortgage: Option[PropertyTitle] =
    if (mortgaged) Some(copy(mortgaged = false)) else None

  override def toString: String =
    s"""Name: $name
       | Base Rent: $rentBase
       | Revaluation factor: $revaluationFactor
       | Base mortgage: $mortgageBase
       | Edification Price: $edificationPrice
       | Mortgaged?: $mortgaged
     """.stripMargin
}
