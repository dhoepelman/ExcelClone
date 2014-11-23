package scalaExcel.GUI.modelwrapper

import rx.lang.scala.Subscription
import scalaExcel.GUI.util.CSSHelper

sealed trait SheetCell extends Ordered[SheetCell] {
  val absoluteIndex: (Int, Int)
  val expression: String
  val evaluated: Any
  val style: String

  def verboseString: String
}

object SheetCell {
  //TODO apply() if necessary

  def newEmpty(absoluteIndex: (Int, Int)): SheetCell =
    new SheetCellImpl(absoluteIndex, "", SheetCellFormatter.default, SheetCellStylist.default, null)

  def modifyStylist(cell: SheetCell, stylist: SheetCellStylist): SheetCell = {
    val impl = cell.asInstanceOf[SheetCellImpl]
    new SheetCellImpl(impl.absoluteIndex, impl.expression, impl.formatter, stylist, impl.evaluated)
  }

  def modifyStyleProperty(cell: SheetCell, property: String, value: Any): SheetCell = {
    val impl = cell.asInstanceOf[SheetCellImpl]
    val style = CSSHelper.modifyStyleProperty(impl.style, property, value)
    new SheetCellImpl(impl.absoluteIndex, impl.expression, impl.formatter, new SheetCellStylist(style, null), impl.evaluated)
  }

  def modifyFormatter(cell: SheetCell, formatter: SheetCellFormatter): SheetCell = {
    val impl = cell.asInstanceOf[SheetCellImpl]
    new SheetCellImpl(impl.absoluteIndex, impl.expression, formatter, impl.stylist, impl.evaluated)
  }

  def markEvaluated(cell: SheetCell, expression: String, value: Any, subscription: Subscription): SheetCell = {
    val impl = cell.asInstanceOf[SheetCellImpl]
    new SheetCellImpl(impl.absoluteIndex, expression, impl.formatter, impl.stylist, value)
  }

  def newEvaluated(absoluteIndex: (Int, Int), expression: String, value: Any): SheetCell =
    new SheetCellImpl(absoluteIndex, expression, SheetCellFormatter.default, SheetCellStylist.default, value)

  private class SheetCellImpl(val absoluteIndex: (Int, Int),
                              val expression: String,
                              val formatter: SheetCellFormatter,
                              val stylist: SheetCellStylist,
                              val evaluated: Any) extends SheetCell {

    val style = stylist.style(this)

    override def toString: String = formatter.format(this)

    def verboseString: String = {
      "Cell{expr=" + expression + ", fmt=" + toString + ", sty=" + style + ", eval=" + evaluated + "}"
    }

    override def compare(that: SheetCell): Int =
      toString.compareToIgnoreCase(if (that == null) "" else that.toString)
  }

}


