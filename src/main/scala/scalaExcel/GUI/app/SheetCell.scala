package scalaExcel.GUI.app

import scalafx.beans.property.{StringProperty}
import scalafx.util.StringConverter
import scalafx.scene.control.TableColumn

class SheetCell(objectValue_ : Any,
                formatter_ : Any => String,
                stylist_ : Any => String) {
  val objectValue = objectValue_
  val formatter = if (formatter_ == null) (v: Any) => v.toString else formatter_
  val stylist = if (stylist_ == null) (_: Any) => "" else stylist_
  val stringValue = new StringProperty(this, "objectString", formatter(objectValue))

  override def toString: String = stringValue.value
}

class SheetCellStringConverter(column_ : TableColumn[SheetBuilder.RowBuffer, SheetCell]) extends StringConverter[SheetCell]{
  override def toString(cell: SheetCell): String = cell.toString

  override def fromString(string: String): SheetCell = {
    val table = column_.getTableView
    val cell = column_.getCellData(table.getEditingCell.getRow)
    //TODO parser call for actual object value
    new SheetCell(string, cell.formatter, cell.stylist)
  }
}
