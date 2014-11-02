package scalaExcel.GUI.model

import scalaExcel.GUI.util.CellExprParser

sealed trait SheetCell {
  val expr: String
  val formatter: Any => String
  val stylist: Any => String
  var evaluated: Any

  def verboseString: String

  def exprString: String
}

object SheetCell {
  def apply(location_ : (Int, Int),
            expr_ : String,
            formatter_ : Any => String,
            stylist_ : Any => String): SheetCell = {
    new SheetCellImpl(location_, expr_, formatter_, stylist_, null)
  }

  def newNormal(location: (Int, Int), expr: String): SheetCell =
    new SheetCellImpl(location, expr, null, null, null)

  def newEmpty(location: (Int, Int)): SheetCell =
    new SheetCellImpl(location, "", null, null, null)

  def newYellow(location: (Int, Int), expr: String): SheetCell = {
    val stylist = (_: Any) => "-fx-background-color: yellow;"
    new SheetCellImpl(location, expr, null, stylist, null)
  }

  def modifyExpr(location: (Int, Int), cell: SheetCell, expr: String): SheetCell =
    new SheetCellImpl(location, expr, cell.formatter, cell.stylist, null)

  def modifyStylist(location: (Int, Int), cell: SheetCell, stylist: Any => String): SheetCell =
    new SheetCellImpl(location, cell.expr, cell.formatter, stylist, cell.evaluated)

  def modifyFormatter(location: (Int, Int), cell: SheetCell, formatter: Any => String): SheetCell =
    new SheetCellImpl(location, cell.expr, formatter, cell.stylist, cell.evaluated)

  def markEvaluated(location: (Int, Int), cell: SheetCell, value: Any): SheetCell =
    if (cell != null)
      new SheetCellImpl(location, cell.expr, cell.formatter, cell.stylist, value)
    else
      new SheetCellImpl(location, null, null, null, value)

  private class SheetCellImpl(location_ : (Int, Int),
                              expr_ : String,
                              formatter_ : Any => String,
                              stylist_ : Any => String,
                              evaluated_ : Any) extends SheetCell {
    val expr = expr_
    val formatter = if (formatter_ == null) (v: Any) => if (v == null) "" else v.toString else formatter_
    val stylist = if (stylist_ == null) (_: Any) => "" else stylist_
    var evaluated = evaluated_
    val subscription = if (evaluated_ != null) null else new CellExprParser(expr, location_, this).getSubscription

    override def toString: String = formatter(if (evaluated == null) "" else evaluated)

    def verboseString: String = {
      "Cell{expr=" + expr + ", formatting=" + formatter(null) + ", styling=" + stylist(null) + ", evaluated=" + evaluated + "}"
    }

    def exprString: String = expr
  }

}


