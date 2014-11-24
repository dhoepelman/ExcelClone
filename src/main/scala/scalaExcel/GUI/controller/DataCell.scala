package scalaExcel.GUI.controller

sealed trait DataCell extends Ordered[DataCell] {
  val expression: String
  val evaluated: Any
  val style: String

  def verboseString: String
}

object DataCell {
  def newEmpty(): DataCell =
    new DataCellImpl("", null)

  def newEvaluated(absoluteIndex: (Int, Int), expression: String, value: Any): DataCell =
    new DataCellImpl(expression, value)
}


private class DataCellImpl(val expression: String,
                           val evaluated: Any) extends DataCell {

  val style = "" //TODO smth like stylist.style(this)

  override def toString: String = if (evaluated == null) "" else evaluated.toString //TODO smth like formatter.format(this)

  def verboseString: String = {
    "Cell{expr=" + expression + ", fmt=" + toString + ", sty=" + style + ", eval=" + evaluated + "}"
  }

  override def compare(that: DataCell): Int =
    toString.compareToIgnoreCase(if (that == null) "" else that.toString)

}
