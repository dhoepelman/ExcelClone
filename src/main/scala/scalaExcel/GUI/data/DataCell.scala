package scalaExcel.GUI.data

import scalaExcel.formula._
import scalaExcel.formula.Evaluator.boolToString
import scalaExcel.GUI.util.CSSHelper
import scalaExcel.model._

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

  val styleString = {
    val aligned = styles.align match {
      case NoAlign => value match {
        case _ : VDouble => styles.setAlign(RightAlign)
        case _ : VBool => styles.setAlign(CenterAlign)
        case _ => styles
      }
      case _ => styles
    }

    value match {
      case _ : VErr => CSSHelper.asError(aligned)
      case _ => CSSHelper.getCSSFromStyle(aligned)
    }
  }

  override def toString = styles.format(value)

  def verboseString: String = {
    "Cell{expr=" + expression + ", fmt=" + toString + ", sty=" + styleString + ", eval=" + value + "}"
  }

}
