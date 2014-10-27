package scalaExcel.GUI.app

import scalafx.util.StringConverter
import scalafx.scene.control.TableColumn

class SheetCell(expr_ : String,
                formatter_ : Any => String,
                stylist_ : Any => String) {
  val expr = expr_
  val formatter = if (formatter_ == null) (v: Any) => v.toString else formatter_
  val stylist = if (stylist_ == null) (_: Any) => "" else stylist_

  //TODO replace with formatter(parse(expr))
  override def toString: String = formatter(expr)

  def verboseString: String = expr
}

class SheetCellStringConverter(column_ : TableColumn[SheetBuilder.RowBuffer, SheetCell]) extends StringConverter[SheetCell] {
  override def toString(cell: SheetCell): String = cell.toString

  override def fromString(expr: String): SheetCell = {
    val table = column_.getTableView
    val cell = column_.getCellData(table.getEditingCell.getRow)
    new SheetCell(expr, cell.formatter, cell.stylist)
  }
}
