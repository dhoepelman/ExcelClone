package scalaExcel.GUI.data

import scalaExcel.GUI.util.CSSHelper
import scalaExcel.model.Styles
import scalaExcel.model.Right
import scalaExcel.model.Default
import scalaExcel.formula.{VString, VDouble, Value}

sealed trait DataCell {
  val expression: String
  val evaluated: Value
  val styles: Styles
  val styleString: String

  def verboseString: String
}

object DataCell {

  def newEmpty(): DataCell = {
    new DataCellImpl("", null, Styles.DEFAULT)
  }

  def newDummy(expression: String): DataCell = {
    new DataCellImpl(expression, null, Styles.DEFAULT)
  }

  def newEvaluated(expression: String, value: Value, styles: Styles): DataCell = {
    new DataCellImpl(expression, value, styles)
  }

}


private class DataCellImpl(val expression: String,
                           val evaluated: Value,
                           val styles: Styles) extends DataCell {

  val styleString = evaluated match {
    case VDouble(d) =>
      if (styles.align.isInstanceOf[Default])
        CSSHelper.CSSFromStyle(styles.setAlign(Right()))
      else
        CSSHelper.CSSFromStyle(styles)
    case _ => CSSHelper.CSSFromStyle(styles)
  }

  override def toString = evaluated match {
    case null => ""
    case VDouble(d) =>
      val formatter =
        if (styles.format != null && styles.format != "") styles.format
        else if (d % 1 == 0) "%1.0f"
        else "%1.2f"
      formatter format d
    case VString(s) => s
    case _ => evaluated.toString
  }

  def verboseString: String = {
    "Cell{expr=" + expression + ", fmt=" + toString + ", sty=" + styleString + ", eval=" + evaluated + "}"
  }

}