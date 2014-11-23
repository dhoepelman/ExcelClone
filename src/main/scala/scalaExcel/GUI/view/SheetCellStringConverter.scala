package scalaExcel.GUI.view

import scalafx.util.StringConverter
import scalaExcel.GUI.controller.Mediator
import scalaExcel.GUI.modelwrapper.SheetCell

object SheetCellStringConverter {

  class SheetCellStringConverter(column: SheetCellColumn) extends StringConverter[SheetCell] {
    override def toString(cell: SheetCell): String = if (cell == null) "" else cell.toString

    override def fromString(expression: String): SheetCell = {
      val table = column.getTableView
      val index = column.getTableView.getEditingCell
      val cell = table.getItems.get(index.getRow).get(index.getColumn - 1).value
      // account for numbered column
      Mediator.changeCellExpression(cell.absoluteIndex, expression)
      Mediator.changeEditorText(expression)
      // return a mock cell instance
      SheetCell.newEmpty((-1, -1))
    }
  }

}
