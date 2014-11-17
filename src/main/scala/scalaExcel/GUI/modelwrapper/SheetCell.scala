package scalaExcel.GUI.modelwrapper

import rx.lang.scala.Subscription
import scalaExcel.GUI.util.{CSSHelper, AwaitingEvaluation}

sealed trait SheetCell {
  val expression: String
  val evaluated: Any
  val style: String

  def verboseString: String
}

object SheetCell {
  //TODO apply() if necessary

  def newEmpty(): SheetCell =
    new SheetCellImpl("", SheetCellFormatter.default, SheetCellStylist.default, null)

  def modifyStylist(cell: SheetCell, stylist: SheetCellStylist): SheetCell = {
    val impl = cell.asInstanceOf[SheetCellImpl]
    new SheetCellImpl(impl.expression, impl.formatter, stylist, impl.evaluated)
  }

  def modifyStyleProperty(cell: SheetCell, property: String, value: Any): SheetCell = {
    val impl = cell.asInstanceOf[SheetCellImpl]
    val style = CSSHelper.modifyStyleProperty(impl.style, property, value)
    new SheetCellImpl(impl.expression, impl.formatter, new SheetCellStylist(style, null), impl.evaluated)
  }

  def modifyFormatter(cell: SheetCell, formatter: SheetCellFormatter): SheetCell = {
    val impl = cell.asInstanceOf[SheetCellImpl]
    new SheetCellImpl(impl.expression, formatter, impl.stylist, impl.evaluated)
  }

  def markEvaluated(cell: SheetCell, expression: String, value: Any, subscription: Subscription): SheetCell = {
    val impl = cell.asInstanceOf[SheetCellImpl]
    new SheetCellImpl(expression, impl.formatter, impl.stylist, value)
  }

  def newEvaluated(cell: SheetCell, expression: String, value: Any): SheetCell = cell match {
    case null =>
      new SheetCellImpl(expression, SheetCellFormatter.default, SheetCellStylist.default, value)
    case impl: SheetCellImpl =>
      new SheetCellImpl(expression, impl.formatter, impl.stylist, value)
  }

  private class SheetCellImpl(expression_ : String,
                              formatter_ : SheetCellFormatter,
                              stylist_ : SheetCellStylist,
                              evaluated_ : Any) extends SheetCell {
    val expression = expression_
    val formatter = formatter_
    val stylist = stylist_
    val evaluated = evaluated_
    val style = stylist.style(this)

    override def toString: String = formatter.format(this)

    def verboseString: String = {
      "Cell{expr=" + expression + ", fmt=" + toString + ", sty=" + style + ", eval=" + evaluated + "}"
    }

  }

}


