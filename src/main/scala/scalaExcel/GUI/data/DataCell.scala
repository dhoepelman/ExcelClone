package scalaExcel.GUI.data

import scalaExcel.model.Styles

sealed trait DataCell {
  val expression: String
  val evaluated: Any
  val styles: Styles
  val styleString: String

  def verboseString: String
}

object DataCell {
  def newEmpty(): DataCell =
    new DataCellImpl("", null, Styles.DEFAULT)

  def newEvaluated(expression: String, value: Any, styles: Styles): DataCell =
    new DataCellImpl(expression, value, styles)
}


private class DataCellImpl(val expression: String,
                           val evaluated: Any,
                           val styles: Styles) extends DataCell {

  val styleString = "" //TODO smth like stylist.style(this)

  override def toString: String = if (evaluated == null) "" else evaluated.toString //TODO smth like formatter.format(this)

  def verboseString: String = {
    "Cell{expr=" + expression + ", fmt=" + toString + ", sty=" + styleString + ", eval=" + evaluated + "}"
  }
}
