package scalaExcel.GUI.view

import scalafx.util.StringConverter
import scalaExcel.GUI.controller.Mediator
import scalaExcel.GUI.modelwrapper.SheetCell

object SheetCellStringConverter {

  class SheetCellStringConverter extends StringConverter[SheetCell] {
    override def toString(cell: SheetCell): String = if (cell == null) "" else cell.toString

    override def fromString(expression: String): SheetCell = {
      // return a mock cell instance
      Mediator.editingCellChanged(expression)
      SheetCell.newEmpty()
    }
  }

}
