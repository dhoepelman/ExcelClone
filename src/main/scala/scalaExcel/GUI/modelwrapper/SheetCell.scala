package scalaExcel.GUI.modelwrapper

import rx.lang.scala.Subscription
import scalaExcel.GUI.util.AwaitingEvaluation

sealed trait SheetCell {
  val expression: String
  val formatter: Any => String
  val stylist: Any => String
  val evaluated: Any
  val subscription: Subscription

  def verboseString: String
}

object SheetCell {
  //TODO apply() if necessary

  val makeYellow = (_: Any) => "-fx-background-color: #FFFF00;"
  val makeGreen = (_: Any) => "-fx-background-color: #008000;"
  val makeError = (_: Any) => "-fx-background-color: #FF0000;"

  def newNormal(expression: String): SheetCell =
    new SheetCellImpl(expression, null, null, new AwaitingEvaluation(expression), null)

  def newEmpty(): SheetCell =
    new SheetCellImpl("", null, null, null, null)

  def modifyStylist(cell: SheetCell, stylist: Any => String): SheetCell =
    new SheetCellImpl(cell.expression, cell.formatter, stylist, cell.evaluated, null)

  def modifyFormatter(cell: SheetCell, formatter: Any => String): SheetCell =
    new SheetCellImpl(cell.expression, formatter, cell.stylist, cell.evaluated, null)

  def markEvaluated(cell: SheetCell, expression: String, value: Any, subscription: Subscription): SheetCell =
    cell match {
      case null => new SheetCellImpl(expression, null, null, value, subscription)
      case old => new SheetCellImpl(expression, old.formatter, old.stylist, value, subscription)
    }

  private class SheetCellImpl(expression_ : String,
                              formatter_ : Any => String,
                              stylist_ : Any => String,
                              evaluated_ : Any,
                              subscription_ : Subscription) extends SheetCell {
    val expression = expression_
    val formatter = if (formatter_ == null) (v: Any) => if (v == null) "" else v.toString else formatter_
    val stylist = if (stylist_ == null) (v: Any) => "" else stylist_
    val evaluated = evaluated_
    val subscription = subscription_

    override def toString: String = formatter(if (evaluated == null) "" else evaluated)

    def verboseString: String = {
      "Cell{expr=" + expression + ", fmt=" + formatter(null) + ", sty=" + stylist(null) + ", eval=" + evaluated + ", subs=" + subscription + "}"
    }

  }

}


