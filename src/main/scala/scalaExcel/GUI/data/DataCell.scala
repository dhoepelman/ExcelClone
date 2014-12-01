package scalaExcel.GUI.data

import scalaExcel.formula.{Value, VDouble, VBool, VString, VEmpty}
import scalaExcel.model._
import scalaExcel.GUI.util.CSSHelper

sealed trait DataCell {
  val expression: String
  val value: Value
  val styles: Styles
  val styleString: String
  def verboseString: String
}

object DataCell {

  def newEmpty(): DataCell = {
    new DataCellImpl("", VEmpty, Styles.DEFAULT)
  }

  def newDummy(expression: String): DataCell = {
    new DataCellImpl(expression, VEmpty, Styles.DEFAULT)
  }

  def newEvaluated(expression: String, value: Value, styles: Styles): DataCell = {
    new DataCellImpl(expression, value, styles)
  }

}


private class DataCellImpl(val expression: String,
                           val value: Value,
                           val styles: Styles) extends DataCell {

  val styleString = (styles.align, value) match {
    case (NoAlign, VDouble(_)) => CSSHelper.getCSSFromStyle(styles.setAlign(RightAlign))
    case (NoAlign, VBool(_)) => CSSHelper.getCSSFromStyle(styles.setAlign(CenterAlign))
    case _ => CSSHelper.getCSSFromStyle(styles)
  }

  override def toString = value match {
    case VEmpty => ""
    case VDouble(d) =>
      val formatter =
        if (styles.format != null && styles.format != "") styles.format
        else if (d % 1 == 0) "%1.0f"
        else "%1.2f"
      formatter format d
    case VString(s) => s
    case _ => value.toString
  }

  def verboseString: String = {
    "Cell{expr=" + expression + ", fmt=" + toString + ", sty=" + styleString + ", eval=" + value + "}"
  }

}
