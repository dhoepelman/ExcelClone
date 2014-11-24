package scalaExcel.GUI.data

/**
 * Types of manipulations of the data window
 */
abstract class WindowActions

case class RefreshWindow() extends WindowActions
case class ReorderColumns(permutations: Map[Int, Int]) extends WindowActions
case class SortRows(sortColumn: Int, sortAscending: Boolean) extends WindowActions
case class ChangeCellExpression(index: (Int, Int), expression: String) extends WindowActions
case class AddNewColumn(position: Int) extends WindowActions
case class AddNewRow() extends WindowActions
case class SlideWindowBy(offsets: (Int, Int, Int, Int)) extends WindowActions
case class SlideWindowTo(bounds: (Int, Int, Int, Int)) extends WindowActions
