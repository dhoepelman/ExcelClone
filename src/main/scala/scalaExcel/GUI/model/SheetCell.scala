package scalaExcel.GUI.model

import rx.lang.scala.Subscription
import scalaExcel.GUI.util.{CircularEvaluation, ErroneousEvaluation, AwaitingEvaluation}

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

  val makeYellow = (_: Any) => "-fx-background-color: yellow;"
  val makeGreen = (_: Any) => "-fx-background-color: green;"
  val makeError = (_: Any) => "-fx-background-color: red;"

  def newNormal(index: (Int, Int), expr: String): SheetCell =
    new SheetCellImpl(index, expr, null, null, null, null)

  def newEmpty(index: (Int, Int)): SheetCell =
    new SheetCellImpl(index, "", null, null, null, null)

  def modifyExpr(index: (Int, Int), cell: SheetCell, expr: String): SheetCell =
    new SheetCellImpl(index, expr, cell.formatter, cell.stylist, new AwaitingEvaluation(expr), null)

  def modifyStylist(index: (Int, Int), cell: SheetCell, stylist: Any => String): SheetCell =
    new SheetCellImpl(index, cell.expr, cell.formatter, stylist, cell.evaluated, null)

  def modifyFormatter(index: (Int, Int), cell: SheetCell, formatter: Any => String): SheetCell =
    new SheetCellImpl(index, cell.expr, formatter, cell.stylist, cell.evaluated, null)

  def markEvaluated(index: (Int, Int), cell: SheetCell, expr: String, value: Any, subscription: Subscription): SheetCell =
    if (cell != null)
    new SheetCellImpl(index, expr, cell.formatter, cell.stylist, value, subscription)
  else
    new SheetCellImpl(index, expr, null, null, value, subscription)

  private class SheetCellImpl(index_ : (Int, Int),
                              expr_ : String,
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
      "Cell{expr=" + expr + ", formatting=" + formatter(null) + ", styling=" + stylist(null) + ", evaluated=" + evaluated + "}"
    }

    def exprString: String = expr
  }

}


