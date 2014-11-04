package scalaExcel.GUI.model

import rx.lang.scala.Subscription
import scalaExcel.GUI.util.AwaitingEvaluation

sealed trait SheetCell {
  val expr: String
  val formatter: Any => String
  val stylist: Any => String
  val evaluated: Any
  val subscription: Subscription

  def verboseString: String

  def exprString: String
}

object SheetCell {
  //TODO apply() if necessary

  val makeYellow = (_: Any) => "-fx-background-color: #FFFF00;"
  val makeGreen = (_: Any) => "-fx-background-color: #008000;"
  val makeError = (_: Any) => "-fx-background-color: #FF0000;"

  def newNormal(expr: String): SheetCell =
    new SheetCellImpl(expr, null, null, null, null)

  def newEmpty(): SheetCell =
    new SheetCellImpl( "", null, null, null, null)

  def modifyExpr(cell: SheetCell, expr: String, subscription: Subscription): SheetCell =
    new SheetCellImpl(expr, cell.formatter, cell.stylist, new AwaitingEvaluation(expr), subscription)

  def modifySubscription(cell: SheetCell, subscription: Subscription): SheetCell =
    new SheetCellImpl(cell.expr, cell.formatter, cell.stylist, cell.evaluated, subscription)

  def modifyStylist(cell: SheetCell, stylist: Any => String): SheetCell =
    new SheetCellImpl(cell.expr, cell.formatter, stylist, cell.evaluated, null)

  def modifyFormatter(cell: SheetCell, formatter: Any => String): SheetCell =
    new SheetCellImpl(cell.expr, formatter, cell.stylist, cell.evaluated, null)

  def markEvaluated(cell: SheetCell, expr: String, value: Any): SheetCell =
    if (cell != null)
      new SheetCellImpl(expr, cell.formatter, cell.stylist, value, cell.subscription)
    else
      new SheetCellImpl(expr, null, null, value, null)

  private class SheetCellImpl(expr_ : String,
                              formatter_ : Any => String,
                              stylist_ : Any => String,
                              evaluated_ : Any,
                              subscription_ : Subscription) extends SheetCell {
    val expr = expr_
    val formatter = if (formatter_ == null) (v: Any) => if (v == null) "" else v.toString else formatter_
    val stylist = if (stylist_ == null) (_: Any) => "" else stylist_
    val evaluated = evaluated_
    val subscription = subscription_

    override def toString: String = formatter(if (evaluated == null) "" else evaluated)

    def verboseString: String = {
      "Cell{expr=" + expr + ", fmt=" + formatter(null) + ", sty=" + stylist(null) + ", eval=" + evaluated + ", subs=" + subscription + "}"
    }

    def exprString: String = expr
  }

}


