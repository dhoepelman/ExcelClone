package scalaExcel.GUI.model

import scalafx.util.StringConverter
import scalaExcel.GUI.controller.Mediator

object SheetCellStringConverter {

  class SheetCellStringConverter extends StringConverter[SheetCell] {
    override def toString(cell: SheetCell): String = if (cell == null) "" else cell.toString

    override def fromString(expr: String): SheetCell = {
      Mediator.composeEditingCell(expr)
    }
  }

}
