package scalaExcel.GUI.view

import scalafx.util.StringConverter
import scalaExcel.GUI.controller.Mediator
import scalaExcel.GUI.modelwrapper.SheetCell

object SheetCellStringConverter {

  class SheetCellStringConverter(column: SheetCellColumn) extends StringConverter[SheetCell] {
    override def toString(cell: SheetCell): String = if (cell == null) "" else cell.toString

    override def fromString(expression: String): SheetCell = {
      // return a mock cell instance
      val cell = column.getTableView.getEditingCell
      // account for numbered column
      Mediator.changeCellExpression((cell.getRow, cell.getColumn - 1), expression)
      Mediator.changeEditorText(expression)
      SheetCell.newEmpty()
    }
  }

}
